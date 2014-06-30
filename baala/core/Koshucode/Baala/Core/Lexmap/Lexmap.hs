{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
( -- * Data type
  Lexmap (..),
  LexmapType (..),
  lexOpName,
  lexAddMessage,
  lexMessageList,

  -- * Constructor
  ConsLexmap,
  ConsLexmapBody,
  RelmapSource,
  consLexmap,
  withTerms,
) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Lexmap.Roamap     as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Message

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TokenTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType     :: LexmapType  -- ^ Type of lexmap
    , lexOpToken  :: B.Token     -- ^ Token of operator
    , lexAttr     :: C.Roa       -- ^ Attribute of relmap operation
    , lexSubmap   :: [Lexmap]    -- ^ Submaps in the attribute
    , lexMessage  :: [String]    -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapWith         -- ^ @-with@ variable
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Write Lexmap where
    write sh Lexmap { lexOpToken = opToken, lexAttr = attr } =
        case lookup C.attrNameAttr attr of
          Nothing -> B.writeH sh [op, "..."]
          Just xs -> B.writeH sh [op, show xs]
        where op = B.tokenContent opToken

instance B.CodePointer Lexmap where
    codePoint = B.codePoint . lexOpToken

-- | Name of relmap operator
lexOpName :: Lexmap -> String
lexOpName = B.tokenContent . lexOpToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexOpToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePointDisplay ("", pt)
          pt  = head $ B.codePoint tok


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [C.GlobalSlot] -> [RelmapSource] -> ConsLexmapBody

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = B.Named ([B.TokenTree], C.Roamap)

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TokenTree] -> B.Ab (Lexmap, [C.Roal Lexmap])

consLexmap :: [B.Named C.RoaSorter] -> ConsLexmap
consLexmap sorters gslot derives = lexmap where

    lexmap :: ConsLexmapBody
    lexmap source =
        B.abortableTrees "lexmap" source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TText _ 0 _) : trees)] -> derived rop trees
           [(B.TreeL rop@(B.TText _ 3 _) : trees)] -> user LexmapWith rop trees
           [[B.TreeB 1 _ trees]] -> lexmap trees
           [[B.TreeB _ _ _]]     -> Message.adlib "bracket"
           [[]]                  -> baseOf "id" []
           [_]                   -> Message.unkRelmap "???"
           trees2                -> baseOf "append" $ map B.treeWrap trees2

    derived :: B.Token -> ConsLexmapBody
    derived rop trees =
        let n = B.tokenContent rop
        in case lookup n derives of
             Just _  -> user LexmapDerived rop trees
             Nothing -> base n rop trees

    baseOf n = base n $ B.textToken n

    base :: String -> B.Token -> ConsLexmapBody
    base n rop trees =
        case lookup n sorters of
          Nothing     ->  user LexmapDerived rop trees
          Just sorter ->  do roa <- sorter trees
                             submap $ cons LexmapBase rop roa trees

    user :: LexmapType -> B.Token -> ConsLexmapBody
    user LexmapWith rop [] = submap $ cons LexmapWith rop [] []
    user LexmapWith _ _ = Message.extraAttr
    user ty rop trees = do roa <- C.roaBranch trees
                           submap $ cons ty rop roa trees

    cons :: LexmapType -> B.Token -> C.Roa -> [B.TokenTree] -> Lexmap
    cons ty rop roa trees =
        check $ Lexmap { lexType    = ty
                       , lexOpToken = rop
                       , lexAttr    = ((C.attrNameAttr, trees) : roa)
                       , lexSubmap  = []
                       , lexMessage = [] }

    check :: B.Map Lexmap
    check lx | lexType lx == LexmapDerived
                 = let n = lexOpName lx
                       msg  = "Same name as base relmap operator '" ++ n ++ "'"
                   in case lookup n sorters of
                        Just _  -> lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    submap :: Lexmap -> B.Ab (Lexmap, [C.Roal Lexmap])
    submap lx@Lexmap { lexAttr = roa } =
        case lookup "-relmap" $ B.mapFstTo (take 7) roa of
          Nothing    -> do lxs <- slot lx   -- no submaps
                           Right (lx, lxs)
          Just trees -> do ws   <- withVars roa
                           subs <- mapM (lexmap . B.li1) $ withTrees ws trees
                           Right ( lx { lexSubmap = map fst subs }
                                 , concatMap snd subs )

    slot :: Lexmap -> B.Ab [C.Roal Lexmap]
    slot lx | lexType lx /= LexmapDerived = Right []
            | otherwise
                = let n   = lexOpName  lx
                      roa = lexAttr    lx
                      sub = lexSubmap  lx
                  in B.abortable "slot" [lx] $ case lookup n derives of
                      Nothing -> B.concatMapM slot sub
                      Just (trees, roamap) ->
                            do roa2       <- C.roamapRun roamap roa
                               trees2     <- C.substSlot gslot roa2 trees
                               (lx2, lxs) <- lexmap trees2
                               sub2       <- B.concatMapM slot sub
                               Right $ ((n, roa), lx2) : lxs ++ sub2

    withTrees :: [String] -> B.Map [B.TokenTree]
    withTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TreeL (B.TText p 0 w)) | w `elem` ws = B.TreeL (B.TText p 3 w)
        loop tree = tree

    withVars :: C.Roa -> B.Ab [String]
    withVars roa =
        case lookup "-with" roa of
          Nothing -> Right []
          Just ws -> withNames ws

    withNames :: [B.TokenTree] -> B.Ab [String]
    withNames ws = do ts <- withTerms ws
                      Right $ map snd ts

-- | Parse @-with@ attribute.
withTerms :: [B.TokenTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ [n]) :
          B.TreeL (B.TText _ 0 v) : xs)  =  next (n, v) xs
    loop (B.TreeL (B.TTerm _ [n]) : xs)  =  next (n, n) xs
    loop (B.TreeL (B.TText _ 0 v) : xs)  =  next (v, v) xs
    loop [] = Right []
    loop _  = Message.reqTermName

    next p xs = do xs' <- loop xs
                   Right $ p : xs'



-- ----------------------
-- $ConstructionProcess
--
--  Construction process of lex relmaps from source trees.
--
--  [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--     Divide list of 'B.TokenTree' by vertical bar (@|@).
--
--  [@\[\[TokenTree\]\] -> \[Lexmap\]@]
--     Construct each 'C.Lexmap' from lists of 'B.TokenTree'.
--     When there are submaps in token trees,
--     constructs 'C.Lexmap' recursively.
--
--  [@\[Lexmap\] -> Lexmap@]
--     Wrap list of 'C.Lexmap' into one 'C.Lexmap'
--     that has these relmaps in 'C.lexSubmap'.
--


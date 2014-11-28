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
import qualified Koshucode.Baala.Core.Message           as Msg

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType     :: LexmapType    -- ^ Type of lexmap
    , lexOpToken  :: B.Token       -- ^ Token of operator
    , lexAttr     :: C.AttrTrees   -- ^ Attribute of relmap operation
    , lexSubmap   :: [Lexmap]      -- ^ Submaps in the attribute
    , lexMessage  :: [String]      -- ^ Messages on lexmap
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

instance B.CodePtr Lexmap where
    codePts = B.codePts . lexOpToken

-- | Name of relmap operator
lexOpName :: Lexmap -> C.RopName
lexOpName = B.tokenContent . lexOpToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexOpToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codeDisplay ("", pt)
          pt  = head $ B.codePts tok


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [C.GlobalSlot] -> [RelmapSource] -> ConsLexmapBody

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = B.Named ([B.TTree], C.Roamap)

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TTree] -> B.Ab (Lexmap, [C.Roal Lexmap])

consLexmap :: (C.RopName -> Maybe C.AttrSort) -> ConsLexmap
consLexmap find gslot derives = lexmap where

    lexmap :: ConsLexmapBody
    lexmap source =
        Msg.abLexmap source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TText _ B.TextRaw _) : trees)] -> derived rop trees
           [(B.TreeL rop@(B.TText _ B.TextKey _) : trees)] -> user LexmapWith rop trees
           [[B.TreeB B.BracketGroup _ trees]]      -> lexmap trees
           [[B.TreeB _ _ _]]     -> Msg.adlib "bracket"
           [[]]                  -> baseOf "id" []
           [_]                   -> Msg.unkRelmap "???"
           trees2                -> baseOf "append" $ map B.wrapTrees trees2

    derived :: B.Token -> ConsLexmapBody
    derived rop trees =
        let n = B.tokenContent rop
        in case lookup n derives of
             Just _  -> user LexmapDerived rop trees
             Nothing -> base n rop trees

    baseOf :: C.RopName -> ConsLexmapBody
    baseOf n = base n $ B.textToken n

    base :: C.RopName -> B.Token -> ConsLexmapBody
    base n rop trees =
        case find n of
          Nothing     ->  user LexmapDerived rop trees
          Just sorter ->  do roa <- sorter trees
                             submap $ cons LexmapBase rop roa trees

    user :: LexmapType -> B.Token -> ConsLexmapBody
    user LexmapWith rop [] = submap $ cons LexmapWith rop [] []
    user LexmapWith _ _ = Msg.extraAttr
    user ty rop trees = do roa <- C.attrSortBranch trees
                           submap $ cons ty rop roa trees

    cons :: LexmapType -> B.Token -> C.AttrTrees -> [B.TTree] -> Lexmap
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
                   in case find n of
                        Just _  -> lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    submap :: Lexmap -> B.Ab (Lexmap, [C.Roal Lexmap])
    submap lx@Lexmap { lexAttr = roa } =
        case B.lookupBy C.isAttrRelmap roa of
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
                  in Msg.abSlot [lx] $ case lookup n derives of
                      Nothing -> B.concatMapM slot sub
                      Just (trees, roamap) ->
                            do roa2       <- C.roamapRun roamap roa
                               trees2     <- C.substSlot gslot roa2 trees
                               (lx2, lxs) <- lexmap trees2
                               sub2       <- B.concatMapM slot sub
                               Right $ ((n, roa), lx2) : lxs ++ sub2

    withTrees :: [String] -> B.Map [B.TTree]
    withTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TreeL (B.TText p B.TextRaw w)) | w `elem` ws = B.TreeL (B.TText p B.TextKey w)
        loop tree = tree

    withVars :: C.AttrTrees -> B.Ab [String]
    withVars roa =
        case lookup (C.AttrTree "-with") roa of
          Nothing -> Right []
          Just ws -> withNames ws

    withNames :: [B.TTree] -> B.Ab [String]
    withNames ws = do ts <- withTerms ws
                      Right $ map snd ts

-- | Parse @-with@ attribute.
withTerms :: [B.TTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ 0 [n]) :
          B.TreeL (B.TText _ B.TextRaw v)   : xs)  =  next (n, v) xs
    loop (B.TreeL (B.TTerm _ 0 [n]) : xs)  =  next (n, n) xs
    loop (B.TreeL (B.TText _ B.TextRaw v)   : xs)  =  next (v, v) xs
    loop [] = Right []
    loop _  = Msg.reqTermName

    next p xs = do xs' <- loop xs
                   Right $ p : xs'



-- ----------------------
-- $ConstructionProcess
--
--  Construction process of lex relmaps from source trees.
--
--  [@\[TTree\] -> \[\[TTree\]\]@]
--     Divide list of 'B.TTree' by vertical bar (@|@).
--
--  [@\[\[TTree\]\] -> \[Lexmap\]@]
--     Construct each 'C.Lexmap' from lists of 'B.TTree'.
--     When there are submaps in token trees,
--     constructs 'C.Lexmap' recursively.
--
--  [@\[Lexmap\] -> Lexmap@]
--     Wrap list of 'C.Lexmap' into one 'C.Lexmap'
--     that has these relmaps in 'C.lexSubmap'.
--


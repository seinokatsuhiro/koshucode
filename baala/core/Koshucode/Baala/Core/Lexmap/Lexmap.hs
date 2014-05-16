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

import qualified Data.Generics                       as G
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Lexmap.Operand as C
import qualified Koshucode.Baala.Core.Lexmap.Rodmap  as C
import qualified Koshucode.Baala.Core.Lexmap.Slot    as C
import qualified Koshucode.Baala.Core.Message        as Message

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TokenTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType     :: LexmapType  -- ^ Type of lexmap
    , lexOpToken  :: B.Token     -- ^ Token of operator
    , lexOperand  :: C.Rod       -- ^ Operand of relmap operator
    , lexSubmap   :: [Lexmap]    -- ^ Submaps in the operand
    , lexMessage  :: [String]    -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapWith         -- ^ @-with@ variable
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Pretty Lexmap where
    doc Lexmap { lexOpToken = opTok, lexOperand = opd } =
        case lookup "@operand" opd of
          Nothing -> B.doch [op, "..."]
          Just xs -> B.doch [op, show xs]
        where op = B.tokenContent opTok

instance B.TokenList Lexmap where
    tokenList Lexmap { lexOpToken = op } = [op]

-- | Name of relmap operator
lexOpName :: Lexmap -> String
lexOpName = B.tokenContent . lexOpToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexOpToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePointDisplay "" $ B.codePoint tok


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [C.GlobalSlot] -> [RelmapSource] -> ConsLexmapBody

-- | Source of relmap: its name, replacement, and operand editor.
type RelmapSource = B.Named ([B.TokenTree], C.Rodmap)

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TokenTree] -> B.Ab (Lexmap, [C.Rody Lexmap])

consLexmap :: [B.Named C.RodSorter] -> ConsLexmap
consLexmap sorters gslot derives = lexmap where

    lexmap :: ConsLexmapBody
    lexmap source =
        B.abortableTrees "lexmap" source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TWord _ 0 _) : trees)] -> derived rop trees
           [(B.TreeL rop@(B.TWord _ 3 _) : trees)] -> user LexmapWith rop trees
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

    baseOf n = base n (B.tokenWord n)

    base :: String -> B.Token -> ConsLexmapBody
    base n rop trees =
        case lookup n sorters of
          Nothing     ->  user LexmapDerived rop trees
          Just sorter ->  do rod <- sorter trees
                             submap $ cons LexmapBase rop rod trees

    user :: LexmapType -> B.Token -> ConsLexmapBody
    user LexmapWith rop [] = submap $ cons LexmapWith rop [] []
    user LexmapWith _ _ = Message.extraOperand
    user ty rop trees = do rod <- C.rodBranch trees
                           submap $ cons ty rop rod trees

    cons :: LexmapType -> B.Token -> C.Rod -> [B.TokenTree] -> Lexmap
    cons ty rop rod trees =
        check $ Lexmap { lexType    = ty
                       , lexOpToken = rop
                       , lexOperand = (("@operand", trees) : rod)
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

    submap :: Lexmap -> B.Ab (Lexmap, [C.Rody Lexmap])
    submap lx@Lexmap { lexOperand = rod } =
        case lookup "-relmap" $ B.mapFstTo (take 7) rod of
          Nothing    -> do lxs <- slot lx   -- no submaps
                           Right (lx, lxs)
          Just trees -> do ws   <- withVars rod
                           subs <- mapM (lexmap . B.singleton) $ withTrees ws trees
                           Right ( lx { lexSubmap = map fst subs }
                                 , concatMap snd subs )

    slot :: Lexmap -> B.Ab [C.Rody Lexmap]
    slot lx | lexType lx /= LexmapDerived = Right []
            | otherwise
                = let n   = lexOpName  lx
                      rod = lexOperand lx
                      sub = lexSubmap  lx
                  in B.abortableFrom "slot" lx $ case lookup n derives of
                      Nothing -> B.concatMapM slot sub
                      Just (trees, rodmap) ->
                            do rod2       <- C.rodmapRun rodmap rod
                               trees2     <- C.slotTrees gslot rod2 trees
                               (lx2, lxs) <- lexmap trees2
                               sub2       <- B.concatMapM slot sub
                               Right $ ((n, rod), lx2) : lxs ++ sub2

    withTrees :: [String] -> B.Map [B.TokenTree]
    withTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TreeL (B.TWord p 0 w)) | w `elem` ws = B.TreeL (B.TWord p 3 w)
        loop tree = tree

    withVars :: C.Rod -> B.Ab [String]
    withVars rod =
        case lookup "-with" rod of
          Nothing -> Right []
          Just ws -> withNames ws

    withNames :: [B.TokenTree] -> B.Ab [String]
    withNames ws = do ts <- withTerms ws
                      Right $ map snd ts

-- | Parse @-with@ operand.
withTerms :: [B.TokenTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ [n]) :
          B.TreeL (B.TWord _ 0 v) : xs)  =  next (n, v) xs
    loop (B.TreeL (B.TTerm _ [n]) : xs)  =  next (n, n) xs
    loop (B.TreeL (B.TWord _ 0 v) : xs)  =  next (v, v) xs
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


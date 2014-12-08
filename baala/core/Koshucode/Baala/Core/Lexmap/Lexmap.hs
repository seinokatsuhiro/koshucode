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
    LexmapTable,
    RelmapSource, NName, NNamed,
    consLexmap,
    nestTerms,
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
    , lexAttr     :: [C.AttrTree]  -- ^ Attribute of relmap operation
    , lexSubmap   :: [Lexmap]      -- ^ Submaps in the attribute
    , lexMessage  :: [String]      -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapNest         -- ^ Nested relation reference
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

-- | Numbered name.
type NName = (Int, String)

-- | Pair which key is a numbered name.
type NNamed a = (NName, a)

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = NNamed ([B.TTree], C.Roamap)

type LexmapTable = [(C.RelmapKey, Lexmap)]

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [C.GlobalSlot] -> [RelmapSource] -> ConsLexmapBody

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TTree] -> B.Ab (Lexmap, LexmapTable)

consLexmap :: (C.RopName -> Maybe C.AttrSort) -> ConsLexmap
consLexmap findSorter gslot derives = lexmap where

    lexmap1 = lexmap . B.li1

    lexmap :: ConsLexmapBody
    lexmap source = Msg.abLexmap source $
        case B.divideTreesByBar source of
          []        -> Msg.bug "empty list @consLexmap"
          [relmap]  -> single relmap
          relmaps   -> baseOf "append" $ map B.wrapTrees relmaps

    -- relmap "/N E" is equivalent to "add /N ( E )"
    -- relmap "| R | R" is equivalent to "id | R | R"
    single (B.TreeL rop@(B.TText _ B.TextRaw _) : trees)  = dispatch rop trees
    single (B.TreeL rop@(B.TText _ B.TextKey _) : trees)  = nest rop trees
    single [B.TreeB B.BracketGroup _ trees]               = lexmap trees
    single [B.TreeB _ _ _]                                = Msg.reqGroup
    single (n@(B.TreeL (B.TTerm _ _ [_])) : trees)        = baseOf "add" $ n : [B.wrapTrees trees]
    single []                                             = baseOf "id" []
    single _                                              = Msg.unkRelmap "???"

    dispatch :: B.Token -> ConsLexmapBody
    dispatch rop trees =
        let n = B.tokenContent rop
        in case resolve 0 n derives of
             Nothing -> base n rop trees
             Just _  -> deriv rop trees

    resolve :: Int -> String -> [RelmapSource] -> Maybe ([B.TTree], C.Roamap)
    resolve _ name = loop where
        loop [] = Nothing
        loop (((_, n), src) : xs)
            | n == name    = Just src
            | otherwise    = loop xs

    baseOf :: C.RopName -> ConsLexmapBody
    baseOf n = base n $ B.textToken n

    base :: C.RopName -> B.Token -> ConsLexmapBody
    base n rop trees =
        case findSorter n of
          Nothing     -> deriv rop trees
          Just sorter -> do attr <- sorter trees
                            submap $ cons LexmapBase rop attr trees

    deriv :: B.Token -> ConsLexmapBody
    deriv rop trees  = do attr <- C.attrSortBranch trees
                          submap $ cons LexmapDerived rop attr trees

    -- construct lexmap except for submaps
    cons :: LexmapType -> B.Token -> [C.AttrTree] -> [B.TTree] -> Lexmap
    cons ty rop attr trees =
        check $ Lexmap { lexType     = ty
                       , lexOpToken  = rop
                       , lexAttr     = ((C.attrNameAttr, trees) : attr)
                       , lexSubmap   = []
                       , lexMessage  = [] }

    check :: B.Map Lexmap
    check lx | lexType lx == LexmapDerived
                 = let n    = lexOpName lx
                       msg  = "Same name as base relmap operator '" ++ n ++ "'"
                   in case findSorter n of
                        Just _  -> lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    -- construct lexmaps of submaps
    submap :: Lexmap -> B.Ab (Lexmap, LexmapTable)
    submap lx@Lexmap { lexAttr = attr } =
        case B.lookupBy C.isAttrNameRelmap attr of
          Nothing    -> do lxs   <- uses lx   -- no submaps
                           Right (lx, lxs)
          Just trees -> do ws    <- nestVars attr
                           subs  <- lexmap1 `mapM` nestTrees ws trees
                           let (sublx, us)  = unzip subs
                               lx2          = lx { lexSubmap = sublx }
                           Right (lx2, concat us)

    uses :: Lexmap -> B.Ab LexmapTable
    uses lx | ty /= LexmapDerived = Right []
            | otherwise = Msg.abSlot [lx] $
                case resolve 0 name derives of
                  Just def -> expand name attr def
                  Nothing  -> Right []
        where
          ty    = lexType   lx
          name  = lexOpName lx
          attr  = lexAttr   lx

    expand name attr (repl, edit) =
        do attr2       <- C.roamapRun edit attr
           repl2       <- C.substSlot gslot attr2 repl
           (lx, lxs)   <- lexmap repl2
           Right $ ((name, attr), lx) : lxs

    nest :: B.Token -> ConsLexmapBody
    nest rop []  = Right (cons LexmapNest rop [] [], [])
    nest _ _     = Msg.extraAttr

    nestVars :: [C.AttrTree] -> B.Ab [String]
    nestVars attr = case B.lookupBy C.isAttrNameNest attr of
                      Nothing  -> Right []
                      Just ws  -> Right . map snd =<< nestTerms ws

    nestTrees :: [String] -> B.Map [B.TTree]
    nestTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TreeL (B.TText p B.TextRaw w)) | w `elem` ws = B.TreeL (B.TText p B.TextKey w)
        loop tree = tree

-- | Parse nested relation attribute.
nestTerms :: [B.TTree] -> B.Ab [B.Terminal String]
nestTerms = loop where
    loop (B.TreeL (B.TTerm _ 0 [n]) :
          B.TreeL (B.TText _ B.TextRaw v) : xs)  = next (n, v) xs
    loop (B.TreeL (B.TTerm _ 0 [n]) : xs)        = next (n, n) xs
    loop (B.TreeL (B.TText _ B.TextRaw v) : xs)  = next (v, v) xs
    loop []                                      = Right []
    loop _                                       = Msg.reqTermName

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


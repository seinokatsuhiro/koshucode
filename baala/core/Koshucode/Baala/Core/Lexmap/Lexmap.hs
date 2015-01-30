{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lexical relmap.

module Koshucode.Baala.Core.Lexmap.Lexmap
  ( -- * Data type
    Lexmap (..),
    LexmapType (..),
    lexRopName,
    lexAddMessage,
    lexMessageList,
  
    -- * Constructor
    consLexmap,
    nestTerms,
    ConsLexmap,
    ConsLexmapBody,
    SecNo,
    FindRelmap,
    LexmapLinkTable,
    RelmapSource, NName, NNamed,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attrmap    as C
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Msg

-- | Intermediate data that represents use of relmap operator.
--   Lexmap is constructed from a list of 'B.TTree',
--   and generic relmap is constructed from a lexmap.
data Lexmap = Lexmap
    { lexType      :: LexmapType    -- ^ Type of lexmap
    , lexRopToken  :: B.Token       -- ^ Token of operator
    , lexAttr      :: [C.AttrTree]  -- ^ Attribute of relmap operation
    , lexSubmap    :: [Lexmap]      -- ^ Submaps in the attribute
    , lexMessage   :: [String]      -- ^ Messages on lexmap
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

data LexmapType
    = LexmapBase         -- ^ Built-in relmap
    | LexmapDerived      -- ^ User-defined relmap
    | LexmapNest         -- ^ Nested relation reference
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Write Lexmap where
    write sh Lexmap { lexRopToken = opToken, lexAttr = attr } =
        case lookup C.attrNameAttr attr of
          Nothing -> B.writeH sh [op, "..."]
          Just xs -> B.writeH sh [op, show xs]
        where op = B.tokenContent opToken

instance B.CodePtr Lexmap where
    codePtList = B.codePtList . lexRopToken

-- | Name of relmap operator
lexRopName :: Lexmap -> C.RopName
lexRopName = B.tokenContent . lexRopToken

lexAddMessage :: String -> B.Map Lexmap
lexAddMessage msg lx = lx { lexMessage = msg : lexMessage lx }

lexMessageList :: Lexmap -> [String]
lexMessageList Lexmap { lexRopToken = tok, lexMessage = msg }
    | null msg  = []
    | otherwise = msg ++ src
    where src = map (("  " ++) . fst) $ B.codePtDisplay ("", pt)
          pt  = head $ B.codePtList tok


-- ----------------------  Constructor

type ConsLexmap = [C.GlobalSlot] -> FindRelmap -> ConsLexmapBody

type FindRelmap = SecNo -> C.RopName -> [RelmapSource]

type ConsLexmapBody = SecNo -> [B.TTree] -> B.Ab (Lexmap, LexmapLinkTable)

type LexmapLinkTable = [(Lexmap, Lexmap)]

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = NNamed ([B.TTree], C.Attrmap)

-- | Section number.
type SecNo = Int

-- | Numbered name.
type NName = (SecNo, String)

-- | Pair which key is a numbered name.
type NNamed a = (NName, a)

-- | First step of constructing relmap,
--   construct lexmap from token trees.
--   The function returns lexmap and related lexmap links.
consLexmap :: (C.RopName -> Maybe C.AttrSort) -> ConsLexmap
consLexmap findSorter gslot findRelmap = lexmap where

    lexmap1 sec = lexmap sec . B.li1

    lexmap :: ConsLexmapBody
    lexmap sec source = Msg.abLexmap source $
        case B.divideTreesByBar source of
          []        -> Msg.bug "empty list @consLexmap"
          [relmap]  -> single sec relmap
          relmaps   -> baseOf "append" sec $ map B.wrapTrees relmaps

    -- relmap "/N E" is equivalent to "add /N ( E )"
    -- relmap "| R | R" is equivalent to "id | R | R"
    single sec (B.TreeL rop@(B.TTextRaw _ _) : trees)  = dispatch rop sec trees
    single sec (B.TreeL rop@(B.TTextKey _ _) : trees)  = nest     rop sec trees
    single sec [B.TreeB B.BracketGroup _ trees]        = lexmap       sec trees
    single _   [B.TreeB _ _ _]                         = Msg.reqGroup
    single sec (n@(B.TermLeaf _ _ [_]) : trees)        = baseOf "add" sec $ n : [B.wrapTrees trees]
    single sec []                                      = baseOf "id"  sec []
    single _ _                                         = Msg.unkRelmap "???"

    dispatch :: B.Token -> ConsLexmapBody
    dispatch rop sec trees =
        let name = B.tokenContent rop
        in case findRelmap sec name of
             [src] -> deriv rop src sec trees
             []    -> base name rop sec trees
             ds    -> Msg.ambRelmap name ds

    baseOf :: C.RopName -> ConsLexmapBody
    baseOf n = base n (B.textToken n)

    -- construct base lexmap
    base :: C.RopName -> B.Token -> ConsLexmapBody
    base name rop sec trees =
        case findSorter name of
          Nothing     -> Msg.unkRelmap name
          Just sorter -> do attr <- sorter trees
                            submap sec $ cons LexmapBase rop attr trees

    -- construct derived lexmap
    deriv :: B.Token -> RelmapSource -> ConsLexmapBody
    deriv rop src _ trees =
        do attr <- C.attrBranch trees
           let lx = cons LexmapDerived rop attr trees
           tab <- table lx src
           Right (lx, tab)

    -- construct lexmap for nested relation reference
    nest :: B.Token -> ConsLexmapBody
    nest rop _ []  = Right (cons LexmapNest rop [] [], [])
    nest _ _ _     = Msg.extraAttr

    -- construct lexmap except for submaps
    cons :: LexmapType -> B.Token -> [C.AttrTree] -> [B.TTree] -> Lexmap
    cons ty rop attr trees =
        check $ Lexmap { lexType      = ty
                       , lexRopToken  = rop
                       , lexAttr      = ((C.attrNameAttr, trees) : attr)
                       , lexSubmap    = []
                       , lexMessage   = [] }

    check :: B.Map Lexmap
    check lx | lexType lx == LexmapDerived
                 = let n    = lexRopName lx
                       msg  = "Same name as base relmap operator '" ++ n ++ "'"
                   in case findSorter n of
                        Just _  -> lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    -- construct lexmaps of submaps
    submap :: SecNo -> Lexmap -> B.Ab (Lexmap, LexmapLinkTable)
    submap sec lx@Lexmap { lexAttr = attr } =
        case B.lookupBy C.isAttrNameRelmap attr of
          Nothing    -> Right (lx, [])
          Just trees -> do ws    <- nestVars attr
                           subs  <- lexmap1 sec `mapM` nestTrees ws trees
                           let (sublx, tabs) = unzip subs
                               lx2           = lx { lexSubmap = sublx }
                           Right (lx2, concat tabs)

    table :: Lexmap -> RelmapSource -> B.Ab LexmapLinkTable
    table lx ((sec, _), (form, edit)) =
        Msg.abSlot [lx] $ do
          attr2       <- C.runAttrmap edit $ lexAttr lx
          form2       <- C.substSlot gslot attr2 form
          (lx2, tab)  <- lexmap sec form2
          Right $ (lx, lx2) : tab

    nestVars :: [C.AttrTree] -> B.Ab [String]
    nestVars attr = case B.lookupBy C.isAttrNameNest attr of
                      Nothing  -> Right []
                      Just ws  -> Right . map snd =<< nestTerms ws

    nestTrees :: [String] -> B.Map [B.TTree]
    nestTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TextLeafRaw p w) | w `elem` ws = B.TextLeafKey p w
        loop tree = tree

-- | Parse nested relation attribute.
nestTerms :: [B.TTree] -> B.Ab [B.Terminal C.RopName]
nestTerms = loop where
    loop (B.TermLeaf _ 0 [n] :
          B.TextLeafRaw _ v : xs)   = next (n, v) xs
    loop (B.TermLeaf _ 0 [n] : xs)  = next (n, n) xs
    loop (B.TextLeafRaw _ v : xs)   = next (v, v) xs
    loop []                         = Right []
    loop _                          = Msg.reqTermName

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


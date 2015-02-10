{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Construction of lexical relmaps.

module Koshucode.Baala.Core.Lexmap.Construct
  ( -- * Constructor
    consLexmap,
    -- * Constructor types
    ConsLexmap, FindRelmap, RelmapSource,
    -- * Types with section number
    SecNo, NName, NNamed,
    -- * Local types
    FindSorter, ConsLexmapBody, LexmapLinkTable,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attrmap    as C
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Lexmap.Lexmap     as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Msg


-- ----------------------  Section number

-- | Section number.
type SecNo = Int

-- | Numbered name.
type NName = (SecNo, String)

-- | Pair which key is a numbered name.
type NNamed a = (NName, a)


-- ----------------------  Constructor type

type ConsLexmap = [C.GlobalSlot] -> FindRelmap -> ConsLexmapBody

type ConsLexmapBody = SecNo -> [B.TTree] -> B.Ab (C.Lexmap, LexmapLinkTable)

type LexmapLinkTable = [(C.Lexmap, C.Lexmap)]

type FindSorter  = C.RopName -> Maybe C.AttrSortPara

type FindRelmap  = SecNo -> C.RopName -> [RelmapSource]

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = NNamed ([B.TTree], C.Attrmap)


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   construct lexmap from token trees.
--   The function returns lexmap and related lexmap links.
consLexmap :: FindSorter -> ConsLexmap
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
    single sec (B.TreeL rop@(B.TTextRaw _ _) : trees)   = dispatch rop sec trees
    single sec (B.TreeL rop@(B.TTextKey _ _) : trees)   = nest     rop sec trees
    single sec (B.TreeL (B.TTermVar cp _ n) : trees)    = nest (B.TTextKey cp n) sec trees
    single sec [B.TreeB B.BracketGroup _ trees]         = lexmap       sec trees
    single _   [B.TreeB _ _ _]                          = Msg.reqGroup
    single sec (n@(B.TermLeaf _ _ [_]) : trees)         = baseOf "add" sec $ n : [B.wrapTrees trees]
    single sec []                                       = baseOf "id"  sec []
    single _ _                                          = Msg.unkRelmap "???"

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
          Just sorter -> do para <- sorter trees
                            submap sec $ cons C.LexmapBase rop para

    -- construct derived lexmap
    deriv :: B.Token -> RelmapSource -> ConsLexmapBody
    deriv rop src _ trees =
        do para <- C.attrBranch trees
           let lx = cons C.LexmapDerived rop para
           tab <- table lx src
           Right (lx, tab)

    table :: C.Lexmap -> RelmapSource -> B.Ab LexmapLinkTable
    table lx ((sec, _), (form, edit)) =
        Msg.abSlot [lx] $ do
          attr2       <- C.runAttrmap edit $ C.lexAttrTree lx
          form2       <- C.substSlot gslot attr2 form
          (lx2, tab)  <- lexmap sec form2
          Right $ (lx, lx2) : tab

    -- construct lexmap for nested relation reference
    nest :: B.Token -> ConsLexmapBody
    nest rop _ []  = Right (cons C.LexmapNest rop B.paraEmpty, [])
    nest _ _ _     = Msg.extraAttr

    -- construct lexmap except for submaps
    cons :: C.LexmapType -> B.Token -> C.AttrPara -> C.Lexmap
    cons ty rop para = check $ C.lexBase { C.lexType      = ty
                                         , C.lexRopToken  = rop
                                         , C.lexAttr      = para }

    check :: B.Map C.Lexmap
    check lx | C.lexType lx == C.LexmapDerived
                 = let n    = C.lexRopName lx
                       msg  = "Same name as base relmap operator '" ++ n ++ "'"
                   in case findSorter n of
                        Just _  -> C.lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    -- construct lexmaps of submaps
    submap :: SecNo -> C.Lexmap -> B.Ab (C.Lexmap, LexmapLinkTable)
    submap sec lx =
        let attr       = C.lexAttrTree lx
            attrRelmap = B.filterFst C.isAttrNameRelmap attr
            attrNest   = B.filterFst C.isAttrNameNest   attr
        in case attrRelmap of
             []            -> Right (lx, [])
             [(_, trees)]  -> do ws      <- nestVars attrNest
                                 let ws2  = nestVars2 trees
                                     ws'  = ws ++ ws2
                                 subs    <- lexmap1 sec `mapM` nestTrees ws' trees
                                 let (sublx, tabs) = unzip subs
                                     lx2           = lx { C.lexSubmap = sublx
                                                        , C.lexNest   = ws2 }
                                 Right (lx2, concat tabs)
             _             -> Msg.bug "submpa"

    nestVars :: [C.AttrTree] -> B.Ab [String]
    nestVars [(_, ws)]  = Right . map snd =<< nestTerms ws
    nestVars []         = Right []
    nestVars _          = Msg.bug "nest"

    nestVars2 :: [B.TTree] -> [String]
    nestVars2 = B.mapMaybe termVarName . concatMap B.untree

    termVarName :: B.Token -> Maybe String
    termVarName (B.TTermVar _ _ n)  = Just n
    termVarName _                   = Nothing

    nestTrees :: [String] -> B.Map [B.TTree]
    nestTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TextLeafRaw p w) | w `elem` ws = B.TextLeafKey p w
        loop tree = tree

    -- | Parse nested relation attribute.
    nestTerms :: [B.TTree] -> B.Ab [B.Terminal C.RopName]
    nestTerms = loop where
        loop (B.TermLeafPath _ [n] :
              B.TextLeafRaw  _  v  : xs)   = next (n, v) xs
        loop (B.TermLeafPath _ [n] : xs)   = next (n, n) xs
        loop (B.TextLeafRaw  _  v  : xs)   = next (v, v) xs
        loop []                            = Right []
        loop _                             = Msg.reqTermName

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


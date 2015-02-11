{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Construction of lexical relmaps.

module Koshucode.Baala.Core.Lexmap.Construct
  ( -- * Constructor
    consLexmap,
    -- * Constructor types
    ConsLexmap, FindDeriv, RelmapSource,
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

type ConsLexmap = [C.GlobalSlot] -> FindDeriv -> SecNo -> ConsLexmapBody

type ConsLexmapBody = [B.TTree] -> B.Ab (C.Lexmap, LexmapLinkTable)

type LexmapLinkTable = [(C.Lexmap, C.Lexmap)]

-- | Find attribute sorter of relmap operator.
type FindSorter = C.RopName -> Maybe C.AttrSortPara

-- | Find derived relmap operator.
type FindDeriv = SecNo -> C.RopName -> [RelmapSource]

-- | Source of relmap: its name, replacement, and attribute editor.
type RelmapSource = NNamed ([B.TTree], C.Attrmap)


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   construct lexmap from token trees.
--   The function returns lexmap and related lexmap links.
consLexmap :: FindSorter -> ConsLexmap
consLexmap findSorter gslot findDeriv = lexmap where

    lexmap1 sec tree = lexmap sec [tree]

    lexmap :: SecNo -> ConsLexmapBody
    lexmap sec trees = result where
        result = Msg.abLexmap trees
                   $ case B.divideTreesByBar trees of
                       []    -> Msg.bug "empty list"
                       [ts]  -> single ts
                       tss   -> baseOf "append" $ map B.wrapTrees tss

        -- operator
        single (B.TreeL rop@(B.TTextRaw _ _) : ts)   = find rop ts
        single (B.TreeL (B.TTermNest cp n) : ts)     = let rop = B.TTextKey cp n
                                                       in nest rop ts
        -- group
        single [B.TreeB B.BracketGroup _ ts]         = lexmap sec ts
        single [B.TreeB _ _ _]                       = Msg.reqGroup

        -- special case
        -- relmap "/N E" is equivalent to "add /N ( E )"
        -- relmap "| R | R" is equivalent to "id | R | R"
        single (n@(B.TermLeaf _ _ [_]) : ts)         = baseOf "add" $ n : [B.wrapTrees ts]
        single []                                    = baseOf "id"  []
        single _                                     = Msg.unkRelmap "???"

        find :: B.Token -> ConsLexmapBody
        find rop ts = let name = B.tokenContent rop
                      in case findDeriv sec name of
                           [src] -> deriv rop src ts
                           []    -> base name rop ts
                           ds    -> Msg.ambRelmap name ds

        -- -----------  base lexmap

        baseOf :: C.RopName -> ConsLexmapBody
        baseOf n = base n $ B.textToken n

        base :: C.RopName -> B.Token -> ConsLexmapBody
        base n rop ts = case findSorter n of
                          Nothing     -> Msg.unkRelmap n
                          Just sorter -> do attr  <- sorter ts
                                            let lx = cons C.LexmapBase rop attr
                                            submap lx

        -- -----------  derived lexmap

        deriv :: B.Token -> RelmapSource -> ConsLexmapBody
        deriv rop src ts =
            do attr  <- C.attrBranch ts
               let lx = cons C.LexmapDerived rop attr
               tab   <- table lx src
               Right (lx, tab)

        table :: C.Lexmap -> RelmapSource -> B.Ab LexmapLinkTable
        table lx ((sec', _), (form, edit)) =
            Msg.abSlot [lx] $ do
              attr2       <- C.runAttrmap edit $ C.lexAttrTree lx
              form2       <- C.substSlot gslot attr2 form
              (lx2, tab)  <- lexmap sec' form2
              Right $ (lx, lx2) : tab

        -- -----------  lexmap for nested relation reference

        nest :: B.Token -> ConsLexmapBody
        nest rop []  = Right (cons C.LexmapLocal rop B.paraEmpty, [])
        nest _ _     = Msg.extraAttr

        -- -----------  construct lexmap except for submaps

        cons :: C.LexmapType -> B.Token -> C.AttrPara -> C.Lexmap
        cons ty rop attr = check $ C.lexBase { C.lexType      = ty
                                             , C.lexRopToken  = rop
                                             , C.lexAttr      = attr }

        check :: B.Map C.Lexmap
        check lx | C.lexType lx == C.LexmapDerived
                     = let n    = C.lexRopName lx
                           msg  = "Same name as base relmap operator '" ++ n ++ "'"
                       in case findSorter n of
                            Just _  -> C.lexAddMessage msg lx
                            Nothing -> lx
        check lx = lx

        -- -----------  construct lexmaps of submaps

        submap :: C.Lexmap -> B.Ab (C.Lexmap, LexmapLinkTable)
        submap lx =
            let attr         = C.lexAttrTree lx
                attrRelmap   = B.filterFst C.isAttrNameRelmap attr
                attrLocal    = B.filterFst C.isAttrNameLocal  attr
            in case attrRelmap of
                 []         -> Right (lx, [])  -- no submaps
                 [(_, ts)]  -> do vs1     <- varsLocal attrLocal
                                  let vs2  = varsNest ts
                                      vs'  = vs1 ++ vs2
                                  subs    <- lexmap1 sec `mapM` nestTrees vs' ts
                                  let (sublx, tabs) = unzip subs
                                      lx2           = lx { C.lexSubmap = sublx
                                                         , C.lexNest   = vs2 }
                                  Right (lx2, concat tabs)
                 _          -> Msg.bug "submap"

    -- -----------  Local relation name

    varsLocal :: [C.AttrTree] -> B.Ab [String]
    varsLocal [(_, vs)]  = mapM vars vs
    varsLocal []         = Right []
    varsLocal _          = Msg.bug "nest"

    vars :: B.TTree -> B.Ab String
    vars (B.TextLeafRaw _ v)  = Right v
    vars _                    = Msg.reqTermName

    -- -----------  Nested relation reference

    varsNest :: [B.TTree] -> [String]
    varsNest = B.mapMaybe unwrap . concatMap B.untree

    unwrap :: B.Token -> Maybe String
    unwrap (B.TTermNest _ n)   = Just n
    unwrap _                   = Nothing

    -- -----------  Rewrite trees

    nestTrees :: [String] -> B.Map [B.TTree]
    nestTrees vs = map loop where
        loop (B.TreeB t cp trees) = B.TreeB t cp $ map loop trees
        loop (B.TextLeafRaw cp v) | v `elem` vs = B.TreeL (B.TTermNest cp v)
        loop tree = tree



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


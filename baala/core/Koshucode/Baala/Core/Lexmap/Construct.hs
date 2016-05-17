{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Construction of lexical relmaps.

module Koshucode.Baala.Core.Lexmap.Construct
  ( -- * Constructor
    consLexmap,

    -- * Constructor types
    ConsLexmap, FindDeriv, LexmapClause,
    -- ** Local types
    RopParaze, ConsLexmapBody, LexmapLinkTable,

    -- * Types with section number
    SecNo, NName, NNamed,
  ) where

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Core.Lexmap.Lexmap       as C
import qualified Koshucode.Baala.Core.Lexmap.LexmapTrees  as C
import qualified Koshucode.Baala.Data.Message             as Msg
import qualified Koshucode.Baala.Core.Lexmap.Message      as Msg


-- ----------------------  Section number

-- | Section number.
type SecNo = Int

-- | Numbered name.
type NName = (SecNo, String)

-- | Pair which key is a numbered name.
type NNamed a = (NName, a)


-- ----------------------  Constructor type

-- | Construct lexmap.
type ConsLexmap = [S.GlobalSlot] -> FindDeriv -> SecNo -> ConsLexmapBody

-- | Find derived relmap operator.
type FindDeriv = SecNo -> C.RopName -> [LexmapClause]

-- | Source of relmap: its name, replacement, and attribute editor.
type LexmapClause = NNamed C.LexmapTrees

-- ----------------------  Local type

-- | Find parameterizer for relmap operator.
type RopParaze = C.RopName -> Maybe S.AttrParaze

-- | Construct lexmap from token trees.
type ConsLexmapBody = [S.TTree] -> B.Ab (C.Lexmap, LexmapLinkTable)

-- | Lexmap and linked lexmap.
type LexmapLinkTable = [(C.Lexmap, C.Lexmap)]


-- ----------------------  Constructor

-- | First step of constructing relmap,
--   construct lexmap from token trees.
--   The function returns lexmap and related lexmap links.
consLexmap :: RopParaze -> ConsLexmap
consLexmap paraze gslot findDeriv = lexmap 0 where

    lexmap :: Int -> SecNo -> ConsLexmapBody
    lexmap eid sec trees = result where
        result = Msg.abLexmap trees $ case S.splitTreesBy "|" trees of
                   Left ts -> single ts
                   Right (ts, _, rest) ->
                       baseOf "append" [S.ttreeGroup ts, S.ttreeGroup rest]

        single :: ConsLexmapBody
        single (B.TreeL tok@(S.TTextRaw _ n)   : ts)
            = find tok n ts                       -- derived or base
        single (B.TreeL tok@(S.TLocal _ _ _ _) : ts)
            = local tok ts                        -- local relation "^/x" or "^x"
        single [B.TreeB S.BracketGroup _ ts]
            = lexmap eid sec ts                   -- group "( ... )"
        single [B.TreeB _ _ _]
            = Msg.reqGroup                        -- unknown group
        single (n@(S.TermLeafName _ _ _) : ts)
            = baseOf "add" $ n : [S.ttreeGroup ts] -- "/N E" means "add /N ( E )"
        single []
            = baseOf "id" []                      -- "| R | R" means "id | R | R"
        single _
            = Msg.unkRelmap "???"                 -- unknown relmap

        -- derived or base relmap operator
        find :: S.Token -> String -> ConsLexmapBody
        find tok n ts = case findDeriv sec n of
                           [src] -> deriv tok src ts
                           []    -> base n tok ts
                           ds    -> Msg.ambRelmap n ds

        -- -----------  local, derived, or base lexmap

        local :: S.Token -> ConsLexmapBody
        local tok [] = Right (cons C.LexmapLocal tok B.def, [])
        local _ _    = Msg.extraAttr

        deriv :: S.Token -> LexmapClause -> ConsLexmapBody
        deriv tok src ts =
            do attr  <- S.attrPara ts
               let lx = cons C.LexmapDerived tok attr
               tab   <- table lx src
               Right (lx, tab)

        table :: C.Lexmap -> LexmapClause -> B.Ab LexmapLinkTable
        table lx ((sec', _), C.LexmapTrees { C.lexmapTrees = form, C.lexmapAttrEd = edit }) =
            Msg.abSlot [lx] $ do
              attr2       <- S.runAttrEd edit $ C.lexAttrTree lx
              form2       <- S.substSlot gslot attr2 form
              (lx2, tab)  <- lexmap (eid + 1) sec' form2
              Right $ (lx, lx2) : tab

        baseOf :: C.RopName -> ConsLexmapBody
        baseOf n = base n $ S.textToken n

        base :: C.RopName -> S.Token -> ConsLexmapBody
        base n tok ts =
            case paraze n of
              Nothing  -> Msg.unkRelmap n
              Just ze  -> do para <- ze ts
                             let lx = cons C.LexmapBase tok para
                             submap lx

        -- -----------  construct lexmap except for its submaps

        cons :: C.LexmapType -> S.Token -> S.AttrPara -> C.Lexmap
        cons ty tok para = check $ B.def { C.lexType   = ty
                                         , C.lexToken  = tok
                                         , C.lexAttr   = para }

        check :: B.Map C.Lexmap
        check lx | C.lexType lx == C.LexmapDerived
                     = let n    = C.lexName lx
                           msg  = "Same name as base relmap operator '" ++ n ++ "'"
                       in case paraze n of
                            Just _  -> C.lexAddMessage msg lx
                            Nothing -> lx
        check lx = lx

        -- -----------  construct lexmaps for submaps

        submap :: C.Lexmap -> B.Ab (C.Lexmap, LexmapLinkTable)
        submap lx =
            let mark = markLocalToken $ C.lexToken lx
                attr = C.lexAttrTree lx
            in case markLocalRelmap mark <$> B.filterFst S.isAttrNameRelmap attr of
                 []     -> Right (lx, [])  -- no submaps
                 locs   -> submap2 lx locs

        submap2 :: C.Lexmap -> [(S.AttrName, [S.TTree])] -> B.Ab (C.Lexmap, LexmapLinkTable)
        submap2 lx attr =
            do subs <- lexmap1 `mapM` attr
               let (sublx, tabs) = unzip subs
                   lx'           = lx { C.lexSubmap = sublx }
               Right (lx', concat tabs)

        lexmap1 (_, ts) = lexmap eid sec ts

        -- Cons up parent token when no parent or same eid.
        markLocalToken :: S.Token -> B.Map S.Token
        markLocalToken p (S.TLocal cp v eid' ps) | null ps || eid == eid'
                        = S.TLocal cp v eid $ p : ps
        markLocalToken _ loc = loc

        markLocalRelmap :: B.Map S.Token -> B.Map (S.AttrName, [S.TTree])
        markLocalRelmap mark (n@(S.AttrRelmapLocal _), ts) = (n, B.mapToLeaf mark <$> ts)
        markLocalRelmap _ (n, ts) = (n, ts)


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


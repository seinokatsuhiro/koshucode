{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Internal representation of relational data resource.
--   Data resource bundles data themselves and calculation stuffs.
--   In Koshucode, data is represented as set of judgements,
--   and calculation is as relation-to-relation mapping.
--   This internal representation is usually constructed from
--   computer files or web resources addressed by URI.
--   Result of calculation on some resource becomes also data resource.

module Koshucode.Baala.Core.Resource.Resource
  ( -- * Data type
    Resource (..), AbResource,
    resEmpty, resIncluded, resInput, resInputPoint, resPattern,
    addMessage, addMessages,

    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, Intmed,
    ShortAssert, ShortAsserts,
    global,

    -- * Concrete type
    AboutC, AboutJudgesC, GlobalC, JudgeC,
    ResourceC, ResultC, ResultWriterC,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Data           as D
import qualified Koshucode.Baala.Core.Assert    as C
import qualified Koshucode.Baala.Core.Lexmap    as C
import qualified Koshucode.Baala.Core.Relkit    as C
import qualified Koshucode.Baala.Core.Relmap    as C


-- ----------------------  Data type

-- | Relational data resource
data Resource c = Resource
    { resGlobal     :: Global c            -- ^ Global parameter
    , resOption     :: C.Option c          -- ^ Options
    , resImport     :: [Resource c]        -- ^ Importing resources
    , resExport     :: [String]            -- ^ Exporting names
    , resSlot       :: [D.NamedTrees]      -- ^ Global slots
    , resLexmap     :: [C.LexmapClause]    -- ^ Source of relmaps
    , resAssert     :: [ShortAssert c]     -- ^ Assertions of relmaps
    , resJudge      :: [D.Judge c]         -- ^ Affirmative or denial judgements
    , resInputStack :: ([C.InputPoint], [C.InputPoint], [B.CodePiece])  -- ^ Input points
    , resOutput     :: B.IOPoint           -- ^ Output point
    , resEcho       :: [[D.TokenLine]]     -- ^ Echo text
    , resLicense    :: [(C.SecNo, String)] -- ^ License text
    , resMessage    :: [String]            -- ^ Collection of messages
    , resLastSecNo  :: C.SecNo             -- ^ Last section number
    , resSelect     :: C.RelSelect c
    }

instance Show (Resource c) where
    show Resource { resInputStack = art }
        = "Resources " ++ show art

instance D.SelectRel Resource where
    selectRel Resource { resSelect = sel } = sel

instance C.GetGlobal Resource where
    getGlobal Resource { resGlobal = g } = g

-- | Abort or resource.
type AbResource c = B.Ab (Resource c)

-- | Resource that has no contents.
resEmpty :: (D.CContent c) => Resource c
resEmpty = Resource
           { resGlobal     = global
           , resOption     = C.option
           , resImport     = []
           , resExport     = []
           , resSlot       = []
           , resLexmap     = []
           , resAssert     = []
           , resJudge      = []
           , resInputStack = ([], [], [])
           , resOutput     = B.IOPointStdout
           , resEcho       = []
           , resLicense    = []
           , resMessage    = []
           , resLastSecNo  = 0
           , resSelect     = \_ _ -> D.reldee
           }

resIncluded :: Resource c -> [B.CodePiece]
resIncluded Resource { resInputStack = (_, _, done) } = done

resInput :: Resource c -> [B.IOPoint]
resInput = map C.inputPoint . resInputPoint

resInputPoint :: Resource c -> [C.InputPoint]
resInputPoint Resource { resInputStack = (in1, in2, in3) }
    = in1 ++ in2 ++ map (ip . B.codeName) in3 where
      ip p = C.InputPoint p []

resPattern :: Resource c -> [D.JudgePat]
resPattern Resource { resAssert = ass } = map (C.assPattern . D.shortBody) ass

addMessage :: String -> B.Map (Resource c)
addMessage msg res = res { resMessage = msg : resMessage res }

addMessages :: [String] -> B.Map (Resource c)
addMessages msg res = res { resMessage = msg ++ resMessage res }


-- ----------------------  Hook

type Assert          c = C.Assert'          Resource c
type ConsRelmap      c = C.ConsRelmap'      Resource c
type Global          c = C.Global'          Resource c
type Relmap          c = C.Relmap'          Resource c
type RelkitHook      c = C.RelkitHook'      Resource c
type RelmapLinkTable c = C.RelmapLinkTable' Resource c
type Rop             c = C.Rop'             Resource c
type RopCons         c = C.RopCons'         Resource c
type Intmed          c = C.Intmed'          Resource c
type ShortAssert     c = C.ShortAssert'     Resource c
type ShortAsserts    c = C.ShortAsserts'    Resource c

global :: (D.CContent c) => Global c
global = C.global' resEmpty


-- ----------------------  Concrete type

type AboutC            = D.About        D.BaalaC
type AboutJudgesC      = D.AboutJudges  D.BaalaC
type GlobalC           = Global         D.BaalaC
type JudgeC            = D.Judge        D.BaalaC
type ResourceC         = Resource       D.BaalaC
type ResultC           = C.Result       D.BaalaC
type ResultWriterC     = C.ResultWriter D.BaalaC


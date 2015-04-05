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
    resEmpty, resIncluded, resCodeName,
    addMessage, addMessages,

    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, Intmed,
    ShortAssert, ShortAsserts,
    global,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Core.Assert    as C
import qualified Koshucode.Baala.Core.Content   as C
import qualified Koshucode.Baala.Core.Lexmap    as C
import qualified Koshucode.Baala.Core.Relkit    as C
import qualified Koshucode.Baala.Core.Relmap    as C


-- ----------------------  Data type

-- | Relational data resource
data Resource c = Resource
    { resGlobal     :: Global c           -- ^ Global parameter
    , resOption     :: C.Option c         -- ^ Options
    , resImport     :: [Resource c]       -- ^ Importing resources
    , resExport     :: [String]           -- ^ Exporting names
    , resSlot       :: [B.NamedTrees]     -- ^ Global slots
    , resLexmap     :: [C.LexmapClause]   -- ^ Source of relmaps
    , resAssert     :: [ShortAssert c]    -- ^ Assertions of relmaps
    , resJudge      :: [B.Judge c]        -- ^ Affirmative or denial judgements
    , resInput      :: ([B.CodeName], [B.CodeName], [B.CodePiece])  -- ^ Input points
    , resOutput     :: B.CodeName         -- ^ Output point
    , resMessage    :: [String]           -- ^ Collection of messages
    , resLastSecNo  :: C.SecNo            -- ^ Last section number
    , resSelect     :: C.RelSelect c
    }

instance Show (Resource c) where
    show Resource { resInput = art }
        = "Resources " ++ show art

instance B.SelectRel Resource where
    selectRel Resource { resSelect = sel } = sel

instance C.GetGlobal Resource where
    getGlobal Resource { resGlobal = g } = g

-- | Abort or resource.
type AbResource c = B.Ab (Resource c)

-- | Resource that has no contents.
resEmpty :: (C.CContent c) => Resource c
resEmpty = Resource
           { resGlobal     = global
           , resOption     = C.option
           , resImport     = []
           , resExport     = []
           , resSlot       = []
           , resLexmap     = []
           , resAssert     = []
           , resJudge      = []
           , resInput      = ([], [], [])
           , resOutput     = B.CodeStdin
           , resMessage    = []
           , resLastSecNo  = 0
           , resSelect     = \_ _ -> B.reldee
           }

resIncluded :: Resource c -> [B.CodePiece]
resIncluded Resource { resInput = (_, _, done) } = done

resCodeName :: Resource c -> B.IOPoints
resCodeName (Resource { resInput = (_, _, inputs), resOutput = output })
    = (map B.codeName inputs, output)

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

global :: (C.CContent c) => Global c
global = C.global' resEmpty

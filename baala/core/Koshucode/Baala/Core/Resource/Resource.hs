{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-incomplete-patterns #-}

-- | Internal representation of data resource.
--   Data resource bundles data themselves and calculation stuffs.
--   In Koshucode, data is represented as set of judgements,
--   and calculation is as relation-to-relation mapping.
--   This internal representation is usually constructed from
--   computer files or web resources addressed by URI.
--   Result of calculation on some resource becomes also data resource.

module Koshucode.Baala.Core.Resource.Resource
  ( -- * Data type
    Resource (..),
    resEmpty, resIncluded,
    addMessage, addMessages,

    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, RopUse, ShortAssert,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Core.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap    as C
import qualified Koshucode.Baala.Core.Assert    as C


-- ----------------------  Data type

data Resource c = Resource
    { resGlobal    :: Global c           -- ^ Global parameter
    , resImport    :: [Resource c]       -- ^ Importing resources
    , resExport    :: [String]           -- ^ Exporting names
    , resSlot      :: [B.NamedTrees]     -- ^ Global slots
    , resRelmap    :: [C.RelmapSource]   -- ^ Source of relmaps
    , resAssert    :: [ShortAssert c]    -- ^ Assertions of relmaps
    , resJudge     :: [B.Judge c]        -- ^ Affirmative or denial judgements
    , resArticle   :: ([B.Source], [B.Source], [B.Source])  -- ^ Scores of resource
    , resMessage   :: [String]           -- ^ Collection of messages
    , resLastSecNo :: C.SecNo            -- ^ Last section number
    , resSelect    :: C.RelSelect c
    }

instance Show (Resource c) where
    show Resource { resArticle = art }
        = "Resources " ++ show art

instance B.SelectRel Resource where
    selectRel Resource { resSelect = sel } = sel

instance C.GetGlobal Resource where
    getGlobal Resource { resGlobal = g } = g

resIncluded :: Resource c -> [B.Source]
resIncluded Resource { resArticle = (_, _, done) } = done

-- | Resource that has no contents.
resEmpty :: Resource c
resEmpty = Resource
           { resGlobal     = C.global
           , resImport     = []
           , resExport     = []
           , resSlot       = []
           , resRelmap     = []
           , resAssert     = []
           , resJudge      = []
           , resArticle    = ([], [], [])
           , resMessage    = []
           , resLastSecNo  = 0
           , resSelect     = \_ _ -> B.reldee
           }

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
type RopUse          c = C.RopUse'          Resource c
type ShortAssert     c = C.ShortAssert'     Resource c


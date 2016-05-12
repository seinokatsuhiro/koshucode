{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

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
    resIncluded, resInput, resInputPoint, resClass,
    addMessage, addMessages,

    -- * Input stack
    InputStack, resStackMap, resStackTodo, resStackDone,

    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, Intmed,
    ShortAssert, ShortAsserts,
    global,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data                 as D
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relkit          as C
import qualified Koshucode.Baala.Core.Relmap          as C


-- ----------------------  Data type

-- | Relational data resource
data Resource c = Resource
    { resGlobal     :: Global c            -- ^ Global parameter
    , resOption     :: C.Option c          -- ^ Options
    , resImport     :: [Resource c]        -- ^ Importing resources
    , resExport     :: [String]            -- ^ Exporting names
    , resSlot       :: [S.NamedTrees]      -- ^ Global slots
    , resLexmap     :: [C.LexmapClause]    -- ^ Source of relmaps
    , resAssert     :: [ShortAssert c]     -- ^ Assertions of relmaps
    , resJudge      :: [D.Judge c]         -- ^ Affirmative or denial judgements
    , resInputStack :: InputStack          -- ^ Input points
    , resOutput     :: B.IOPoint           -- ^ Output point
    , resEcho       :: [[S.TokenLine]]     -- ^ Echo text
    , resLicense    :: [(C.SecNo, String)] -- ^ License text
    , resMessage    :: [String]            -- ^ Collection of messages
    , resLastSecNo  :: C.SecNo             -- ^ Last section number
    , resSelect     :: C.RelSelect c       -- ^ Relation selector
    }

instance Show (Resource c) where
    show Resource { resInputStack = art }
        = "Resources " ++ show art

instance D.SelectRel Resource where
    selectRel Resource { resSelect = sel } = sel

instance C.GetGlobal Resource where
    getGlobal Resource { resGlobal = g } = g

-- | Resource that has no contents.
instance (D.CContent c) => B.Default (Resource c) where
    def = Resource
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

-- | Abort or resource.
type AbResource c = B.Ab (Resource c)

-- | Included sources, i.e., done-part of input stack.
resIncluded :: Resource c -> [B.CodePiece]
resIncluded Resource { resInputStack = (_, _, done) } = done

resInput :: Resource c -> [B.IOPoint]
resInput = map C.inputPoint . resInputPoint

-- | All input points.
resInputPoint :: Resource c -> [C.InputPoint]
resInputPoint Resource { resInputStack = (todo, ready, done) } = ps where
    ps = todo ++ ready ++ map (ip . B.codeName) done
    ip p = C.InputPoint p []

-- | List of all judgement classes.
resClass :: Resource c -> [D.JudgeClass]
resClass Resource {..} = map (C.assClass . S.shortBody) resAssert

-- | Add single message.
addMessage :: String -> B.Map (Resource c)
addMessage msg res = res { resMessage = msg : resMessage res }

-- | Add messages.
addMessages :: [String] -> B.Map (Resource c)
addMessages msg res = res { resMessage = msg ++ resMessage res }


-- ----------------------  Input stack

-- | Stack for input code: /todo/, /ready/ and /done/.
type InputStack = ([C.InputPoint], [C.InputPoint], [B.CodePiece])

-- | Map to input stack.
resStackMap :: B.Map InputStack -> B.Map (Resource c)
resStackMap f res@Resource {..} = res { resInputStack = f resInputStack }

-- | Add input to todo-part of input stack.
resStackTodo :: C.InputPoint -> B.Map (Resource c)
resStackTodo t = resStackMap $ inputStackTodo t where

-- | Add input to done-part of input stack.
resStackDone :: B.CodePiece -> B.Map (Resource c)
resStackDone d = resStackMap $ inputStackDone d

inputStackTodo :: C.InputPoint -> B.Map InputStack
inputStackTodo t (todo, ready, done) = (t:todo, ready, done)

inputStackDone :: B.CodePiece -> B.Map InputStack
inputStackDone d (todo, ready, done) = (todo, ready, d:done)


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
global = C.global' B.def


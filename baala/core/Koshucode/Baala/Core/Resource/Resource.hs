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
    resIncluded, resInput, resInputPoint,
    resClass, resFeature,
    resMessagesAdd,

    -- * Input queue
    InputQueue, resQueueTodo, resQueueDone,

    -- * Hook
    Assert, ConsRelmap, Global,
    RelkitHook, Relmap, RelmapLinkTable,
    Rop, RopCons, Intmed,
    ShortAssert, ShortAsserts,
    global,
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Type                 as T
import qualified Koshucode.Baala.Data                 as D
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relkit          as C
import qualified Koshucode.Baala.Core.Relmap          as C


-- ----------------------  Data type

-- | Relational data resource
data Resource c = Resource
    { resGlobal     :: Global c            -- ^ __Global:__ Global parameter
    , resLastSecNo  :: C.SecNo             -- ^ __Global:__ Last section number
    , resCacheT     :: D.CacheT S.Chars    -- ^ __Global:__ Term name cache

    , resOption     :: C.Option c          -- ^ __Setting:__ Options
    , resExport     :: [String]            -- ^ __Setting:__ Exporting names

    , resImport     :: [Resource c]        -- ^ __Input:__ Importing resources
    , resInputQueue :: InputQueue          -- ^ __Input:__ Input points
    , resJudge      :: [T.Judge c]         -- ^ __Input:__ Affirmative or denial judgements
    , resDataset    :: D.Dataset c         -- ^ __Input:__ Dataset

    , resSlot       :: [S.GlobalSlot S.Chars]  -- ^ __Calc:__ Global slots
    , resLexmap     :: [C.LexmapClause]        -- ^ __Calc:__ Source of relmaps
    , resAssert     :: [ShortAssert c]         -- ^ __Calc:__ Assertions of relmaps

    , resOutput     :: B.IOPoint               -- ^ __Output:__ Output point
    , resEcho       :: [[S.TokenLine S.Chars]] -- ^ __Output:__ Echo text
    , resLicense    :: [(C.SecNo, String)]     -- ^ __Output:__ License text
    , resMessage    :: [String]                -- ^ __Output:__ Collection of messages
    }

instance Show (Resource c) where
    show Resource { resInputQueue = q }
        = "Resources " ++ show q

instance T.SelectRel Resource where
    selectRel = T.selectRel . resDataset

instance D.GetCops Resource where
    getCops = D.getCops . resGlobal

instance C.GetGlobal' Resource where
    getGlobal' = resGlobal

-- | Resource that has no contents.
instance (D.CContent c) => B.Default (Resource c) where
    def = Resource
           { resGlobal     = global
           , resLastSecNo  = 0
           , resCacheT     = D.cacheT

           , resOption     = C.option
           , resExport     = []

           , resImport     = []
           , resInputQueue = (B.def, [])
           , resJudge      = []
           , resDataset    = B.def

           , resSlot       = []
           , resLexmap     = []
           , resAssert     = []

           , resOutput     = B.IOPointStdout Nothing
           , resEcho       = []
           , resLicense    = []
           , resMessage    = []
           }

-- | Abort or resource.
type AbResource c = B.Ab (Resource c)

-- | Included sources, i.e., done-part of input queue.
resIncluded :: Resource c -> [B.IxIOPoint]
resIncluded Resource { resInputQueue = (_, done) } = done

-- | 'B.IOPoint' of all inputs.
resInput :: Resource c -> [B.IOPoint]
resInput = map C.inputPoint . resInputPoint

-- | All input points.
resInputPoint :: Resource c -> [C.InputPoint S.Chars]
resInputPoint Resource { resInputQueue = (q, done) } = ps where
    ps = B.qTo q ++ map (ip . B.nioPoint) done
    ip p = C.InputPoint p []

-- | List of all judgement classes.
resClass :: Resource c -> [S.JudgeClass]
resClass Resource {..} = map (C.assClass . S.shortBody) resAssert

-- | Get feature from resource.
resFeature :: Resource c -> C.Feature
resFeature res = C.globalFeature $ resGlobal res

-- | Add messages.
resMessagesAdd :: [String] -> O.Map (Resource c)
resMessagesAdd msg res = res { resMessage = msg ++ resMessage res }


-- ----------------------  Input queue

-- | Queue for input code: /todo/, /ready/ and /done/.
type InputQueue = (B.Queue (C.InputPoint S.Chars), [B.IxIOPoint])

-- | Map to input queue.
resQueueMap :: O.Map InputQueue -> O.Map (Resource c)
resQueueMap f res@Resource {..} = res { resInputQueue = f resInputQueue }

-- | Add input to todo-part of input queue.
resQueueTodo :: C.InputPoint S.Chars -> O.Map (Resource c)
resQueueTodo t = resQueueMap todo where
    todo (q, done) = (B.enq t q, done)

-- | Add input to done-part of input queue.
resQueueDone :: B.IxIOPoint -> O.Map (Resource c)
resQueueDone d = resQueueMap push where
    push (q, done) = (q, d:done)


-- ----------------------  Hook

-- | Assertion of relation.
type Assert          c = C.Assert'          Resource c

-- | Relmap construction.
type ConsRelmap      c = C.ConsRelmap'      Resource c

-- | Global parameter.
type Global          c = C.Global'          Resource c

-- | Generic relmap.
type Relmap          c = C.Relmap'          Resource c

-- | Make relkit with data resource.
type RelkitHook      c = C.RelkitHook'      Resource c

-- | Lexmap-to-relmap table.
type RelmapLinkTable c = C.RelmapLinkTable' Resource c

-- | Relmap operator.
type Rop             c = C.Rop'             Resource c

-- | Constructor of relmap operator.
type RopCons         c = C.RopCons'         Resource c

-- | Intermediate relmap.
type Intmed          c = C.Intmed'          Resource c

-- | Assertion with short signs.
type ShortAssert     c = C.ShortAssert'     Resource c

-- | Assertion list with short signs.
type ShortAsserts    c = C.ShortAsserts'    Resource c

-- | Default global parameter.
global :: (D.CContent c) => Global c
global = C.global' B.def


{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Relkit constructors.

module Koshucode.Baala.Core.Relkit.Construct
  ( -- * General constructor
    relkit, relkitId,
    relkitJust, relkitNothing,
    relkitSetSource,

    -- * Flow relkit
    relkitLine, relkitMany, relkitWhole,
    relkitLineAb, relkitManyAb, relkitWholeAb,
    relkitFilterAb,

    -- * Confluent relkit
    relkitConflLine, relkitConflMany, relkitConflWhole,
    relkitConflFilter,

    -- * Source of relation
    relkitConst, relkitConstRel, relkitConstEmpty,
    relkitConstSingleton, relkitConstBody,
    relkitSource,

    -- * Relation reference
    relkitNest, relkitCopy, relkitLocal,
  ) where

import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax              as S
import qualified Koshucode.Baala.Type                as T
import qualified Koshucode.Baala.Core.Relkit.Relkit  as C


instance Monoid (C.Relkit c) where
    mempty = relkitConstRel T.reldee
    mappend (C.Relkit _ _ bo1) (C.Relkit _ ho2 bo2) =
        relkit ho2 $ C.RelkitAppend bo1 bo2

-- ----------------------  General constructor

-- | Construct relkit with output heading
--   and relation-to-relation calculation.
relkit :: Maybe T.Head -> C.RelkitCore c -> C.Relkit c
relkit ho = C.Relkit Nothing ho . B.Codic []

-- | Relkit for identity relmap.
relkitId :: Maybe T.Head -> C.Relkit c
relkitId ho = relkit ho C.RelkitId

-- | Determinated relkit.
relkitJust :: T.Head -> C.RelkitCore c -> C.Relkit c
relkitJust ho = relkit $ Just ho

-- | Indeterminated relkit.
relkitNothing :: C.Relkit c
relkitNothing = relkit Nothing C.RelkitId

-- | Set relkit source.
relkitSetSource :: (B.GetCodePos cp) => cp -> O.Map (C.Relkit c)
relkitSetSource cp (C.Relkit hi ho (B.Codic _ core)) =
    C.Relkit hi ho $ B.codic cp core


-- ----------------------  Flow

-- | Create non-abortble one-to-one (linear) relkit.
relkitLine
    :: Bool               -- ^ Remove duplication or not
    -> T.Head             -- ^ Heading of output relation
    -> C.Flow [c] [c]     -- ^ Flow function which converts tuples of relation
    -> C.Relkit c         -- ^ Result relkit
relkitLine u ho flow = relkitJust ho $ C.RelkitLine u flow

-- | Create non-abortble one-to-many relkit.
relkitMany :: Bool -> T.Head -> C.Flow [c] [[c]] -> C.Relkit c
relkitMany u ho flow = relkitJust ho $ C.RelkitMany u flow

-- | Create non-abortble whole-mapping relkit.
relkitWhole :: Bool -> T.Head -> C.Flow [[c]] [[c]] -> C.Relkit c
relkitWhole u ho flow = relkitJust ho $ C.RelkitWhole u flow

-- | Create abortable one-to-one (linear) relkit.
relkitLineAb :: Bool -> T.Head -> C.FlowAb [c] [c] -> C.Relkit c
relkitLineAb u ho flow = relkitJust ho $ C.RelkitAbLine u (const flow) []

-- | Create abortable one-to-many relkit.
relkitManyAb :: Bool -> T.Head -> C.FlowAb [c] [[c]] -> C.Relkit c
relkitManyAb u ho flow = relkitJust ho $ C.RelkitAbMany u (const flow) []

-- | Create abortable whole-mapping relkit.
relkitWholeAb :: Bool -> T.Head -> C.FlowAb [[c]] [[c]] -> C.Relkit c
relkitWholeAb u ho flow = relkitJust ho $ C.RelkitAbWhole u (const flow) []

-- | Create abortable filtering relkit.
relkitFilterAb
    :: T.Head             -- ^ Heading of output relation
    -> B.AbTest [c]       -- ^ Teset function which determins keeping/omitting tuples
    -> C.Relkit c         -- ^ Result relkit
relkitFilterAb ho test = relkitJust ho $ C.RelkitAbTest test


-- ----------------------  Confluent

-- | Create abortable one-to-one (linear) confluent relkit.
relkitConflLine
    :: Bool               -- ^ Remove duplication or not
    -> T.Head             -- ^ Heading of output relation
    -> C.Confl c [c] [c]  -- ^ Confluent function which merges multiple relations
    -> [C.RelkitBody c]   -- ^ Relkit of subrelmaps
    -> C.Relkit c         -- ^ Result relkit
relkitConflLine u ho confl subs = relkitJust ho $ C.RelkitAbLine u confl subs

-- | Create abortable one-to-many confluent relkit.
relkitConflMany :: Bool -> T.Head -> C.Confl c [c] [[c]] -> [C.RelkitBody c] -> C.Relkit c
relkitConflMany u ho confl subs = relkitJust ho $ C.RelkitAbMany u confl subs

-- | Create abortable whole-mapping confluent relkit.
relkitConflWhole :: Bool -> T.Head -> C.Confl c [[c]] [[c]] -> [C.RelkitBody c] -> C.Relkit c
relkitConflWhole u ho confl subs = relkitJust ho $ C.RelkitAbWhole u confl subs

-- | Create abortable confluent filtering relkit.
relkitConflFilter :: T.Head -> B.AbTest [[c]] -> C.RelkitBody c -> C.Relkit c
relkitConflFilter ho test sub = relkitJust ho $ C.RelkitAbSemi test sub


-- ----------------------  Source of relation

-- | Relkit for constant relmap.
relkitConst :: T.Head -> [[c]] -> C.Relkit c
relkitConst he bo = relkitJust he $ C.RelkitConst bo

-- | Relkit outputs constant relation.
relkitConstRel :: T.Rel c -> C.Relkit c
relkitConstRel (T.Rel he bo) = relkitConst he bo

-- | Relkit for relmap which output empty relation.
relkitConstEmpty :: [S.TermName] -> C.Relkit c
relkitConstEmpty ns = relkitConstBody ns []

-- | Relkit for relmap which outputs singleton relation.
relkitConstSingleton :: [S.TermName] -> [c] -> C.Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

-- | Relkit for constant relmap.
relkitConstBody :: [S.TermName] -> [[c]] -> C.Relkit c
relkitConstBody ns bo = kit where
    he  = T.headFrom ns
    kit = relkitJust he $ C.RelkitConst bo

-- | Relkit for source relmap.
relkitSource :: T.JudgeClass -> [S.TermName] -> C.Relkit c
relkitSource cl ns = relkitJust he kit where
    he  = T.headFrom ns
    kit = C.RelkitSource cl ns


-- ----------------------  Relation reference

-- | Relkit with nested relation index.
relkitNest :: S.Token -> [S.IndexTerm] -> O.Map (C.Relkit c)
relkitNest p nest (C.Relkit _ ho kitb) = relkit ho $ C.RelkitNest p nest kitb

-- | Relkit for copy relmap.
relkitCopy :: S.Token -> String -> O.Map (C.Relkit c)
relkitCopy p n (C.Relkit _ ho kitb) = relkit ho $ C.RelkitCopy p n kitb

-- | Relkit for local relation reference.
relkitLocal :: S.Token -> S.LocalRef -> T.Head -> C.Relkit c
relkitLocal p n he = relkitJust he $ C.RelkitLocal p n


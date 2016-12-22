{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Relkit constructors.

module Koshucode.Baala.Core.Relkit.Construct
  ( -- * General constructor
    relkit, relkitId,
    relkitJust, relkitNothing,
    relkitSetSource,

    -- * Flow and confluent
    relkitLinear,
    relkitAbLinear,
    relkitConfl,

    -- * Source of relation
    relkitConst, relkitConstEmpty,
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
    mempty = relkitConst T.reldee
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


-- ----------------------  Flow and confluent

-- | Create non-abortble linear relkit.
relkitLinear :: T.Head -> Bool -> (C.Flow [c] [c]) -> C.Relkit c
relkitLinear ho uniq f = relkitJust ho $ C.RelkitLinear uniq f

-- | Create abortable linear relkit.
relkitAbLinear :: T.Head -> Bool -> (C.FlowAb [c] [c]) -> C.Relkit c
relkitAbLinear ho uniq f = relkitJust ho $ C.RelkitAbLinear uniq (const f) []

-- | Create (abortable) confluent relkit.
relkitConfl :: T.Head -> Bool -> (C.Confl c [[c]] [[c]]) -> [C.RelkitBody c] -> C.Relkit c
relkitConfl ho uniq f bodies = relkitJust ho $ C.RelkitAbFull uniq f bodies


-- ----------------------  Source of relation

-- | Relkit for constant relmap.
relkitConst :: T.Rel c -> C.Relkit c
relkitConst (T.Rel he bo) = relkitJust he $ C.RelkitConst bo

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
relkitSource p ns = relkitJust he kit where
    he  = T.headFrom ns
    kit = C.RelkitSource p ns


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


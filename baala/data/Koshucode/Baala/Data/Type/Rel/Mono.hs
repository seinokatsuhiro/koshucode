{-# OPTIONS_GHC -Wall #-}

-- | Content type for nested relations.

module Koshucode.Baala.Data.Type.Rel.Mono
  ( Mono (..), RelMono, RelText,
    isMonoTerm, isMonoNest,
    gMonoTerm, gMonoNest,
    pMonoTerm, pMonoNest,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data.Type.Rel.Rel  as D

-- | Content type for monotype relation.
data Mono c
    = MonoTerm c                 -- ^ Terminal content
    | MonoNest (RelMono c)       -- ^ Nested relation
      deriving (Show, Eq, Ord)

-- | Monotype relation.
type RelMono c = D.Rel (Mono c)

-- | Text relation.
type RelText = RelMono String

-- | Test terminal content.
isMonoTerm :: Mono c -> Bool
isMonoTerm (MonoTerm _) = True
isMonoTerm (MonoNest _) = False

-- | Test nested relation.
isMonoNest :: Mono c -> Bool
isMonoNest (MonoTerm _) = False
isMonoNest (MonoNest _) = True

-- | Get terminal content.
gMonoTerm :: Mono c -> c
gMonoTerm (MonoTerm c) = c
gMonoTerm (MonoNest _) = B.bug "gMonoTerm"

-- | Get nested relation.
gMonoNest :: Mono c -> RelMono c
gMonoNest (MonoNest r) = r
gMonoNest (MonoTerm _) = B.bug "gMonoNest"

-- | Put terminal content.
pMonoTerm :: c -> Mono c
pMonoTerm = MonoTerm

-- | Put nested relation.
pMonoNest :: RelMono c -> Mono c
pMonoNest = MonoNest


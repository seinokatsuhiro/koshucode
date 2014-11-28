{-# OPTIONS_GHC -Wall #-}

-- | Content type for nested relations.

module Koshucode.Baala.Base.Data.Mono
  ( Mono (..),
    RelText,
    isMonoType, isMonoNest,
    gMonoType, gMonoNest,
    pMonoNest, pMonoType,
  ) where

import qualified Koshucode.Baala.Base.Data.Rel  as B
import qualified Koshucode.Baala.Base.Abort     as B

data Mono a
    = MonoType a
    | MonoNest (B.Rel (Mono a))
      deriving (Show, Eq, Ord)

type RelText = B.Rel (Mono String)

isMonoNest :: Mono a -> Bool
isMonoNest (MonoType _) = False
isMonoNest (MonoNest _) = True

isMonoType :: Mono a -> Bool
isMonoType (MonoType _) = True
isMonoType (MonoNest _) = False

gMonoType :: Mono a -> a
gMonoType (MonoType a) = a
gMonoType (MonoNest _) = B.bug "gMonoType"

gMonoNest :: Mono a -> B.Rel (Mono a)
gMonoNest (MonoNest r) = r
gMonoNest (MonoType _) = B.bug "gMonoNest"

pMonoType :: a -> Mono a
pMonoType = MonoType

pMonoNest :: B.Rel (Mono a) -> Mono a
pMonoNest = MonoNest


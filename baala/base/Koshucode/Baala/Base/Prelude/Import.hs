{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Import
( -- * Control.Monad
  (Control.Monad.<=<),
  (Control.Monad.>=>),
  Control.Monad.sequence,
  Control.Monad.filterM,

  -- * Data.Map
  lookupMap,

  -- * Data.Maybe
  Data.Maybe.catMaybes,
  Data.Maybe.fromJust,
  Data.Maybe.fromMaybe,

  -- * Data.Monoid
  Data.Monoid.Monoid,
  Data.Monoid.mappend,
  Data.Monoid.mconcat,
  Data.Monoid.mempty,

  -- * Text.PrettyPrint
  Text.PrettyPrint.Doc,
  (Text.PrettyPrint.<>),
  (Text.PrettyPrint.<+>),
  (Text.PrettyPrint.$$),
  Text.PrettyPrint.nest,
) where

import Control.Monad
import Data.Map
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint

-- | Same as 'Map.lookup' in @Data.Map@ module.
lookupMap :: (Ord k) => k -> Data.Map.Map k a -> Maybe a
lookupMap = Data.Map.lookup


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Import
( -- * Control.Monad
  (Control.Monad.<=<),
  (Control.Monad.>=>),
  Control.Monad.sequence,
  Control.Monad.filterM,
  Control.Monad.foldM,
  Control.Monad.when,
  Control.Monad.unless,
  Control.Monad.guard,
  concatMapM,

  -- * Data.List
  Data.List.sort,
  Data.List.transpose,
  Data.List.intercalate,

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

  -- * Data.Tuple
  Data.Tuple.swap,

  -- * Text.PrettyPrint
  Text.PrettyPrint.Doc,
  (Text.PrettyPrint.<>),
  (Text.PrettyPrint.<+>),
  (Text.PrettyPrint.$$),
  Text.PrettyPrint.nest,
  docEmpty,
  docHang,
  docZero,
) where

import Control.Monad
import Data.List
import Data.Tuple
import Data.Map
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = return . concat Control.Monad.<=< mapM f

-- | Same as 'Map.lookup' in @Data.Map@ module.
lookupMap :: (Ord k) => k -> Data.Map.Map k a -> Maybe a
lookupMap = Data.Map.lookup

docEmpty :: Text.PrettyPrint.Doc
docEmpty = Text.PrettyPrint.empty

docHang :: Text.PrettyPrint.Doc -> Int -> Text.PrettyPrint.Doc -> Text.PrettyPrint.Doc
docHang = Text.PrettyPrint.hang

docZero :: String -> Text.PrettyPrint.Doc
docZero = Text.PrettyPrint.zeroWidthText


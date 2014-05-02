{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Import
( -- * Control.Monad
  (Control.Monad.<=<),
  (Control.Monad.>=>),
  Control.Monad.sequence,

  -- * Data.Maybe
  Data.Maybe.catMaybes,
  Data.Maybe.fromJust,

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
import Data.Maybe
import Data.Monoid
import Text.PrettyPrint

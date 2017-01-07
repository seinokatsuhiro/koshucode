{-# OPTIONS_GHC -Wall #-}

-- | Reexport list.

module Koshucode.Baala.Base.Prelude.Import
  ( -- * Control.Exception
    Control.Exception.bracket,

    -- * Control.Monad
    (<.>),
    (Control.Monad.>=>),
    Control.Monad.filterM,
    Control.Monad.foldM,
    Control.Monad.guard,
    Control.Monad.sequence,
    Control.Monad.unless,
    Control.Monad.when,
    --concatMapM,
  
    -- * Data.ByteString
    Bs,
    -- * Data.ByteString.Lazy
    Bz,
    -- * Data.ByteString.Lazy.UTF8
    stringBz, bzString,

    -- * Data.Default
    Data.Default.Default (..),

    -- * Data.List
    Data.List.intercalate,
    Data.List.intersect,
    Data.List.isPrefixOf,
    Data.List.isInfixOf,
    Data.List.isSuffixOf,
    Data.List.sort,
    Data.List.transpose,
    (Data.List.\\),
  
    -- * Data.Map
    lookupMap,
  
    -- * Data.Maybe
    Data.Maybe.catMaybes,
    Data.Maybe.mapMaybe,
    Data.Maybe.fromJust,
    Data.Maybe.fromMaybe,
  
    -- * Data.Tuple
    Data.Tuple.swap,

    -- * System.Exit
    System.Exit.ExitCode (..),
    System.Exit.exitWith,

    -- * System.IO
    System.IO.stdout,
  
    -- * Text.PrettyPrint
    Text.PrettyPrint.Doc,
    (Text.PrettyPrint.<+>),
    (Text.PrettyPrint.$$),
    Text.PrettyPrint.nest,
    docEmpty,
    docHang,
    docZero,
  ) where

import qualified Control.Exception
import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.UTF8
import qualified Data.Default
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Tuple
import qualified System.IO
import qualified System.Exit
import qualified Text.PrettyPrint


-- ----------------------  Control

infixr 1 <.>

-- | Composition of monadic functions.
(<.>) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<.>) = (Control.Monad.<=<)

--concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
--concatMapM f = return . concat Control.Monad.<=< mapM f


-- ----------------------  Data

-- | Strict bytestring.
type Bs = Data.ByteString.ByteString

-- | Lazy bytestring.
type Bz = Data.ByteString.Lazy.ByteString

-- | Convert string into lazy bytestring.
--   This is same as 'Data.ByteString.Lazy.UTF8.fromString'.
stringBz :: String -> Bz
stringBz = Data.ByteString.Lazy.UTF8.fromString

-- | Convert lazy bytestring to string.
--   This is same as 'Data.ByteString.Lazy.UTF8.toString'.
bzString :: Bz -> String
bzString = Data.ByteString.Lazy.UTF8.toString

-- | Same as 'Map.lookup' in @Data.Map@ module.
lookupMap :: (Ord k) => k -> Data.Map.Map k a -> Maybe a
lookupMap = Data.Map.lookup


-- ----------------------  Text

-- | Same as 'Text.PrettyPrint.empty'.
docEmpty :: Text.PrettyPrint.Doc
docEmpty = Text.PrettyPrint.empty

-- | Same as 'Text.PrettyPrint.hang'.
docHang :: Text.PrettyPrint.Doc -> Int -> Text.PrettyPrint.Doc -> Text.PrettyPrint.Doc
docHang = Text.PrettyPrint.hang

-- | Same as 'Text.PrettyPrint.zeroWidthText'.
docZero :: String -> Text.PrettyPrint.Doc
docZero = Text.PrettyPrint.zeroWidthText

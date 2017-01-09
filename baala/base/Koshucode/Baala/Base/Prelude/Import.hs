{-# OPTIONS_GHC -Wall #-}

-- | Reexport list.

module Koshucode.Baala.Base.Prelude.Import
  ( -- * Control.Exception
    Control.Exception.bracket,

    -- * Control.Monad
    (<#.>),
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
    -- * Data.ByteString.UTF8
    stringBs, bsString,
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

    -- * Data.Text
    Tx, stringTx, txString,
    -- * Data.Text.Lazy
    Tz, stringTz, tzString,
  
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
import qualified Data.ByteString.UTF8
import qualified Data.Default
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Tuple
import qualified System.IO
import qualified System.Exit
import qualified Text.PrettyPrint


-- ----------------------  Control

infixr 1 <#.>

-- | Composition of monadic functions.
(<#.>) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<#.>) = (Control.Monad.<=<)

--concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
--concatMapM f = return . concat Control.Monad.<=< mapM f


-- ----------------------  Data

-- | Strict bytestring.
type Bs = Data.ByteString.ByteString

-- | Convert strict bytestring to string.
--   This is same as 'Data.ByteString.UTF8.toString'.
bsString :: Bs -> String
bsString = Data.ByteString.UTF8.toString

-- | Convert string to strict bytestring.
--   This is same as 'Data.ByteString.UTF8.fromString'.
stringBs :: String -> Bs
stringBs = Data.ByteString.UTF8.fromString

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

-- | Strict text.
type Tx = Data.Text.Text

-- | Convert strict text to string.
--   This is same as 'Data.Text.unpack'.
txString :: Tx -> String
txString = Data.Text.unpack

-- | Convert string to strict text.
--   This is same as 'Data.Text.pack'.
stringTx :: String -> Tx
stringTx = Data.Text.pack

-- | Lazy text.
type Tz = Data.Text.Lazy.Text

-- | Convert lazy text to string.
--   This is same as 'Data.Text.Lazy.unpack'.
tzString :: Tz -> String
tzString = Data.Text.Lazy.unpack

-- | Convert string to lazy text.
--   This is same as 'Data.Text.Lazy.pack'.
stringTz :: String -> Tz
stringTz = Data.Text.Lazy.pack

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

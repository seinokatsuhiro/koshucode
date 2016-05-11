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
  
    -- * Data.ByteString.Lazy
    Bz,

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
    disjoint,
    overlap,
  
    -- * Data.Map
    lookupMap,
  
    -- * Data.Maybe
    Data.Maybe.catMaybes,
    Data.Maybe.mapMaybe,
    Data.Maybe.fromJust,
    Data.Maybe.fromMaybe,
  
    -- * Data.Monoid
    Data.Monoid.Monoid,
    Data.Monoid.mappend,
    Data.Monoid.mconcat,
    Data.Monoid.mempty,
  
    -- * Data.Tuple
    Data.Tuple.swap,

    -- * GHC.IO.Encoding
    setLocaleUtf8,

    -- * System.Exit
    System.Exit.ExitCode (..),
    System.Exit.exitWith,
    exitCode,

    -- * System.IO
    System.IO.stdout,
  
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

import qualified Control.Monad
import qualified Data.ByteString.Lazy
import qualified Data.Default
import qualified Data.List
import qualified Data.Map
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Tuple
import qualified GHC.IO.Encoding
import qualified System.IO
import qualified System.Exit
import qualified Text.PrettyPrint


-- ----------------------  Control

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = return . concat Control.Monad.<=< mapM f


-- ----------------------  Data

type Bz = Data.ByteString.Lazy.ByteString

-- | Test two list has no elements in common.
disjoint :: (Eq a) => [a] -> [a] -> Bool
disjoint a b = null (a `Data.List.intersect` b)

-- | Test two list has some common elements.
overlap :: (Eq a) => [a] -> [a] -> Bool
overlap a b = not $ disjoint a b

-- | Same as 'Map.lookup' in @Data.Map@ module.
lookupMap :: (Ord k) => k -> Data.Map.Map k a -> Maybe a
lookupMap = Data.Map.lookup


-- ----------------------  GHC

setLocaleUtf8 :: IO ()
setLocaleUtf8 = GHC.IO.Encoding.setLocaleEncoding GHC.IO.Encoding.utf8_bom

-- ----------------------  System

exitCode :: Int -> System.Exit.ExitCode
exitCode 0 = System.Exit.ExitSuccess
exitCode n = System.Exit.ExitFailure n


-- ----------------------  Text

docEmpty :: Text.PrettyPrint.Doc
docEmpty = Text.PrettyPrint.empty

docHang :: Text.PrettyPrint.Doc -> Int -> Text.PrettyPrint.Doc -> Text.PrettyPrint.Doc
docHang = Text.PrettyPrint.hang

docZero :: String -> Text.PrettyPrint.Doc
docZero = Text.PrettyPrint.zeroWidthText

{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.ContentClass
(
-- * Haskell data
  BoolValue (..)
, IntValue (..)
, StringValue (..)
, ListValue (..)

-- * Koshucode data
, RelValue (..)
, TermsetValue (..)
, Nil (..)

-- * Convinient class
, Value (..)
) where

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Rel



-- ----------------------  Haskell built-in data

class BoolValue v where
    boolValue   :: Bool -> v
    isBoolValue :: v -> Bool

class IntValue v where
    intValue   :: Int -> v
    isIntValue :: v -> Bool

class StringValue v where
    stringValue    :: String -> v
    isStringValue  :: v -> Bool
    theStringValue :: v -> String

class ListValue v where
    listValue    :: [v] -> v
    isListValue  :: v -> Bool
    theListValue :: v -> [v]



-- ----------------------  Data in koshucode

class RelValue v where
    relValue   :: Rel v -> v
    isRelValue :: v -> Bool

class TermsetValue v where
    termsetValue   :: [Named v] -> v
    isTermsetValue :: v -> Bool

-- | Types that can be nil
class Nil v where
    nil   :: v
    isNil :: v -> Bool



-- ----------------------  Convinient class

class (Ord v, Pretty v, Nil v,
       BoolValue v, StringValue v, IntValue v,
       ListValue v, TermsetValue v, RelValue v) => Value v where
    appendContent :: v -> v -> v
    joinContent :: [v] -> v
    joinContent = foldr appendContent nil


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.ValueClass
( BoolValue (..)
, IntValue (..)
, StringValue (..)
, ListValue (..)
, RelValue (..)
, Nil (..)
, Value ()
) where

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Rel


-- ----------------------  Built-in data

class BoolValue v where
    boolValue   :: Bool -> v
    isBoolValue :: v -> Bool

class IntValue v where
    intValue   :: Int -> v
    isIntValue :: v -> Bool

class StringValue v where
    stringValue   :: String -> v
    isStringValue :: v -> Bool

class ListValue v where
    listValue   :: [v] -> v
    isListValue :: v -> Bool


-- ----------------------  Data in koshucode

class RelValue v where
    relValue   :: Rel v -> v
    isRelValue :: v -> Bool

-- | Types that can be nil
class Nil v where
    nil   :: v
    isNil :: v -> Bool

class (Ord v, Pretty v, Nil v, StringValue v) => Value v


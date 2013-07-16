{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Content.Class
(
-- * Primitive content
  PrimitiveContent (),

-- * Haskell data
  BoolValue (..),
  IntValue (..),
  StringValue (..),
  ListValue (..),

-- * Koshu data
  Nil (..),
  SetValue (..),
  TermsetValue (..),
  RelValue (..),

-- * Convinient class
  Value (..),
) where

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Data.Rel



-- ----------------------  Primitive content

class PrimitiveContent a



-- ----------------------  Haskell built-in data

class (PrimitiveContent v) => BoolValue v where
    boolValue   :: Bool -> v
    isBoolValue :: v -> Bool

class (PrimitiveContent v) => IntValue v where
    intValue   :: Int -> v
    isIntValue :: v -> Bool

class (PrimitiveContent v) => StringValue v where
    stringValue    :: String -> v
    isStringValue  :: v -> Bool
    theStringValue :: v -> String

class (PrimitiveContent v) => ListValue v where
    listValue    :: [v] -> v
    isListValue  :: v -> Bool
    theListValue :: v -> [v]



-- ----------------------  Data in koshucode

{-| Types that can be nil -}
class (PrimitiveContent v) => Nil v where
    nil   :: v
    isNil :: v -> Bool

class (PrimitiveContent v) => SetValue v where
    setValue   :: [v] -> v
    isSetValue :: v -> Bool

class (PrimitiveContent v) => TermsetValue v where
    termsetValue   :: [Named v] -> v
    isTermsetValue :: v -> Bool

class (PrimitiveContent v) => RelValue v where
    relValue   :: Rel v -> v
    isRelValue :: v -> Bool



-- ----------------------  Convinient class

class (Ord v, Pretty v, Nil v,
       BoolValue v, StringValue v, IntValue v,
       ListValue v, SetValue v, TermsetValue v,
       RelValue v) => Value v where
    appendContent :: v -> v -> v
    joinContent :: [v] -> v
    joinContent = foldr appendContent nil


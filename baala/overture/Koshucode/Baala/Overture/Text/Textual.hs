{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Textual class.

module Koshucode.Baala.Overture.Text.Textual
  ( Textual (..),
  ) where

import qualified Data.Text                       as Tx
import qualified Data.Text.Lazy                  as Tz
import qualified Koshucode.Baala.Overture.Misc   as O

-- | Text-like value.
class (Eq a) => Textual a where

    -- ----------------------  Monoid

    -- | Empty text.
    --
    --   >>> tEmpty :: String
    --   ""
    --
    tEmpty :: a

    -- | Test text is empty.
    --
    --   >>> tIsEmpty ""
    --   True
    --
    --   >>> tIsEmpty "foo"
    --   False
    --
    tIsEmpty :: a -> Bool
    tIsEmpty = (== tEmpty)

    -- | Append two texts.
    --
    --   >>> tJoin "foo" "bar"
    --   "foobar"
    --
    tJoin :: a -> a -> a

    -- | Append multiple texts.
    --
    --   >>> tJoinAll ["foo", "bar", "baz"]
    --   "foobarbaz"
    --
    tJoinAll :: [a] -> a
    tJoinAll = foldr tJoin tEmpty

    -- ----------------------  Subtext

    -- | Preppend character to text.
    --
    --   >>> tAdd 'g' "foo"
    --   "gfoo"
    --
    tAdd :: Char -> a -> a

    -- | Split text into first character and rest of text.
    --
    --   >>> tCut "foo"
    --   Just ('f',"oo")
    --
    --   >>> tCut ""
    --   Nothing
    --
    tCut :: a -> Maybe (Char, a)

    -- | Take /N/ characters.
    --
    --   >>> tTake 3 "foobar"
    --   "foo"
    --
    tTake :: Int -> a -> a

    -- | Drop /N/ characters.
    --
    --   >>> tDrop 3 "foobar"
    --   "bar"
    --
    tDrop :: Int -> a -> a

    -- ----------------------  Conversion

    -- | Convert textual value to string.
    --
    --   >>> tString "foo"
    --   "foo"
    --
    tString :: a -> String

    -- | Convert string to textual value.
    --
    --   >>> stringT "foo" :: String
    --   "foo"
    --
    stringT :: String -> a

instance Textual String where
    tEmpty       = ""
    tIsEmpty     = null
    tJoin        = (++)
    tAdd         = (:)
    tCut         = O.uncons
    tTake        = take
    tDrop        = drop

    tString      = id
    stringT      = id

-- | Strict text.
instance Textual Tx.Text where
    tEmpty       = Tx.empty
    tIsEmpty     = Tx.null
    tJoin        = Tx.append

    tAdd         = Tx.cons
    tCut         = Tx.uncons
    tTake        = Tx.take
    tDrop        = Tx.drop

    tString      = Tx.unpack
    stringT      = Tx.pack

-- | Lazy text.
instance Textual Tz.Text where
    tEmpty       = Tz.empty
    tIsEmpty     = Tz.null
    tJoin        = Tz.append

    tAdd         = Tz.cons
    tCut         = Tz.uncons
    tTake        = Tz.take . fromIntegral
    tDrop        = Tz.drop . fromIntegral

    tString      = Tz.unpack
    stringT      = Tz.pack


{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Textual class.

module Koshucode.Baala.Overture.Text.Textual
  ( Textual (..),
  ) where

import qualified Data.Text                       as Tx
import qualified Data.Text.Lazy                  as Tz
import qualified Koshucode.Baala.Overture.Infix  as O
import qualified Koshucode.Baala.Overture.Misc   as O

-- | Text-like value.
class (Eq a, Monoid a) => Textual a where

    -- ----------------------  Monoid

    -- | Empty text.
    --
    --   >>> tEmpty :: String
    --   ""
    --
    tEmpty :: a
    tEmpty = mempty

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
    tJoin = mappend

    -- | Append multiple texts.
    --
    --   >>> tJoinAll ["foo", "bar", "baz"]
    --   "foobarbaz"
    --
    tJoinAll :: [a] -> a
    tJoinAll = mconcat

    -- ----------------------  Subtext

    -- | Preppend character to text.
    --
    --   >>> tAdd 'g' "foo"
    --   "gfoo"
    --
    tAdd :: Char -> a -> a

    -- | Preppend two characters to text.
    --
    --   >>> tAdd2 'b' 'a' "foo"
    --   "bafoo"
    --
    tAdd2 :: Char -> Char -> a -> a
    tAdd2 c d = tAdd c . tAdd d

    -- | Preppend two characters to text.
    --
    --   >>> tAdd3 'b' 'a' 'r' "foo"
    --   "barfoo"
    --
    tAdd3 :: Char -> Char ->  Char -> a -> a
    tAdd3 c d e = tAdd c . tAdd2 d e

    -- | Split text into first character and rest of text.
    --
    --   >>> tCut "foo"
    --   Just ('f',"oo")
    --
    --   >>> tCut ""
    --   Nothing
    --
    tCut :: a -> Maybe (Char, a)

    -- | Split one or two characters.
    --
    --   >>> tCut2 "bar"
    --   Just ('b', Just ('a', "r"))
    --
    tCut2 :: a -> Maybe (Char, Maybe (Char, a))
    tCut2 a = tCut O.<$$> tCut a

    -- | Split one, two, or three characters.
    --
    --   >>> tCut3 "bar"
    --   Just ('b', Just ('a', Just ('r', "")))
    --
    --   >>> tCut3 "b"
    --   Just ('b', Nothing)
    --
    tCut3 :: a -> Maybe (Char, Maybe (Char, Maybe (Char, a)))
    tCut3 a = tCut2 O.<$$> tCut a

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

    -- | Drop prefix from text.
    --
    --   >>> tDropPrefix "foo" "foobar"
    --   Just "bar"
    --
    --   >>> tDropPrefix "bar" "foobar"
    --   Nothing
    --
    tDropPrefix :: (Textual b) => b -> a -> Maybe a
    tDropPrefix (tCut -> Just (c, cs)) (tCut -> Just (a, as))
        | c == a       = tDropPrefix cs as
        | otherwise    = Nothing
    tDropPrefix cs as
        | tIsEmpty cs  = Just as
        | otherwise    = Nothing

    -- ----------------------  Creation

    -- | Create text from single character.
    --
    --   >>> charT 'b' :: String
    --   "b"
    --
    charT :: Char -> a
    charT c = tAdd c tEmpty

    -- | Create text from two characters.
    --
    --   >>> char2T 'b' 'a' :: String
    --   "ba"
    --
    char2T :: Char -> Char -> a
    char2T c d = tAdd2 c d tEmpty

    -- | Create text from three characters.
    --
    --   >>> char3T 'b' 'a' 'r' :: String
    --   "bar"
    --
    char3T :: Char -> Char -> Char -> a
    char3T c d e = tAdd3 c d e tEmpty

    -- | /N/-length textual value.
    --
    --   >>> charsT 4 '+' :: String
    --   "++++"
    --
    charsT :: Int -> Char -> a
    charsT n c | n > 0     = tAdd c $ charsT (n - 1) c
               | otherwise = tEmpty

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

    -- | Convert show instance to textual value.
    --
    --   >>> showT (120 :: Int) :: String
    --   "120"
    --
    showT :: (Show b) => b -> a
    showT = stringT . show

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


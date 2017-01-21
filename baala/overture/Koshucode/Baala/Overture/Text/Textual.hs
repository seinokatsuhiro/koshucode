{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Textual class.

module Koshucode.Baala.Overture.Text.Textual
  ( Textual (..),
  ) where

import qualified Data.String                         as Str
import qualified Data.Text                           as Tx
import qualified Data.Text.Encoding                  as Tx
import qualified Data.Text.Encoding.Error            as Err
import qualified Data.Text.Lazy                      as Tz
import qualified Data.Text.Lazy.Encoding             as Tz
import qualified Data.ByteString.UTF8                as Bs
import qualified Data.ByteString.Lazy                as Bz
import qualified Data.ByteString.Lazy.UTF8           as Bz
import qualified Koshucode.Baala.Overture.Infix      as O
import qualified Koshucode.Baala.Overture.Misc       as O
import qualified Koshucode.Baala.Overture.Shorthand  as O

-- | Text-like value.
class (Eq t, Ord t, Monoid t, Str.IsString t) => Textual t where

    -- ----------------------  Monoid

    -- | Empty text.
    --
    --   >>> tEmpty :: String
    --   ""
    --
    tEmpty :: t
    tEmpty = mempty

    -- | Test text is empty.
    --
    --   >>> tIsEmpty ""
    --   True
    --
    --   >>> tIsEmpty "foo"
    --   False
    --
    tIsEmpty :: t -> Bool
    tIsEmpty = (== tEmpty)

    -- | Append two texts.
    --
    --   >>> tJoin "foo" "bar"
    --   "foobar"
    --
    tJoin :: t -> t -> t
    tJoin = mappend

    -- | Append multiple texts.
    --
    --   >>> tJoinAll ["foo", "bar", "baz"]
    --   "foobarbaz"
    --
    tJoinAll :: [t] -> t
    tJoinAll = mconcat

    -- ----------------------  Subtext

    -- | Preppend character to text.
    --
    --   >>> tAdd 'g' "foo"
    --   "gfoo"
    --
    tAdd :: Char -> t -> t

    -- | Preppend two characters to text.
    --
    --   >>> tAdd2 'b' 'a' "foo"
    --   "bafoo"
    --
    tAdd2 :: Char -> Char -> t -> t
    tAdd2 c d = tAdd c . tAdd d

    -- | Preppend two characters to text.
    --
    --   >>> tAdd3 'b' 'a' 'r' "foo"
    --   "barfoo"
    --
    tAdd3 :: Char -> Char ->  Char -> t -> t
    tAdd3 c d e = tAdd c . tAdd2 d e

    -- | Split text into first character and rest of text.
    --
    --   >>> tCut "foo"
    --   Just ('f',"oo")
    --
    --   >>> tCut ""
    --   Nothing
    --
    tCut :: t -> Maybe (Char, t)

    -- | Split one or two characters.
    --
    --   >>> tCut2 "bar"
    --   Just ('b', Just ('a', "r"))
    --
    tCut2 :: t -> Maybe (Char, Maybe (Char, t))
    tCut2 a = tCut O.<$$> tCut a

    -- | Split one, two, or three characters.
    --
    --   >>> tCut3 "bar"
    --   Just ('b', Just ('a', Just ('r', "")))
    --
    --   >>> tCut3 "b"
    --   Just ('b', Nothing)
    --
    tCut3 :: t -> Maybe (Char, Maybe (Char, Maybe (Char, t)))
    tCut3 a = tCut2 O.<$$> tCut a

    -- | Take /N-#1/ characters from /text-#2/.
    --
    --   >>> tTake 3 "foobar"
    --   "foo"
    --
    tTake :: Int -> t -> t

    -- | Drop /N-#1/ characters from /text-#2/.
    --
    --   >>> tDrop 3 "foobar"
    --   "bar"
    --
    tDrop :: Int -> t -> t

    -- | Drop /prefix-#1/ from /text-#2/.
    --
    --   >>> tDropPrefix "foo" "foobar"
    --   Just "bar"
    --
    --   >>> tDropPrefix "bar" "foobar"
    --   Nothing
    --
    tDropPrefix :: (Textual a) => a -> t -> Maybe t
    tDropPrefix (tCut -> Just (c, cs)) (tCut -> Just (a, as))
        | c == a       = tDropPrefix cs as
        | otherwise    = Nothing
    tDropPrefix cs as
        | tIsEmpty cs  = Just as
        | otherwise    = Nothing

    -- | Map /function-#1/ to characters of /text-value-#2/.
    --
    --   >>> tMap fromEnum "bar"
    --   [98,97,114]
    --
    tMap :: (Char -> a) -> t -> [a]
    tMap f = loop where
        loop (tCut -> Just (c, cs)) = f c : loop cs
        loop _ = []

    -- | Divide /textual-value-#2/ to textual list by /delimiter-tester-#1/.
    --
    --   >>> tDivide (== '/') "foo/bar/baz"
    --   ["foo","bar","baz"]
    --
    tDivide :: (Char -> Bool) -> t -> [t]
    tDivide f t0 = loop (0 :: Int) t0 where
        loop n (tCut -> Just (c, t))
            | f c        = tTake n t0 : tDivide f t
            | otherwise  = loop (n + 1) t
        loop n _ = [tTake n t0]

    -- | Length of /textual-value-#1/.
    --
    --   >>> tLength "foo" :: Int
    --   3
    --
    tLength :: (Integral n) => t -> n
    tLength (tCut -> Just (_, t)) = 1 + tLength t
    tLength _ = 0

    -- | Take subtext from /textual-value-#2/ while /tester-#1/ holds,
    --   and pairing the subtext with after.
    --
    --   >>> tWhile (/= ' ') "foo bar baz"
    --   ("foo", " bar baz")
    --
    tWhile :: (Char -> Bool) -> t -> (t, t)
    tWhile f t0 = loop 0 t0 where
        loop n (tCut -> Just (c, t)) | f c = loop (n + 1) t
        loop n t = (tTake n t0, t)

    -- ----------------------  Creation

    -- | Create text from single character.
    --
    --   >>> charT 'b' :: String
    --   "b"
    --
    charT :: Char -> t
    charT c = tAdd c tEmpty

    -- | Create text from two characters.
    --
    --   >>> char2T 'b' 'a' :: String
    --   "ba"
    --
    char2T :: Char -> Char -> t
    char2T c d = tAdd2 c d tEmpty

    -- | Create text from three characters.
    --
    --   >>> char3T 'b' 'a' 'r' :: String
    --   "bar"
    --
    char3T :: Char -> Char -> Char -> t
    char3T c d e = tAdd3 c d e tEmpty

    -- | /N/-length textual value.
    --
    --   >>> charsT 4 '+' :: String
    --   "++++"
    --
    charsT :: Int -> Char -> t
    charsT n c | n > 0     = tAdd c $ charsT (n - 1) c
               | otherwise = tEmpty

    -- ----------------------  Conversion

    -- | Convert textual value to string.
    --
    --   >>> tString "foo"
    --   "foo"
    --
    tString :: t -> String

    -- | Convert string to textual value.
    --
    --   >>> stringT "foo" :: String
    --   "foo"
    --
    stringT :: String -> t

    -- | Convert show instance to textual value.
    --
    --   >>> showT (120 :: Int) :: String
    --   "120"
    --
    showT :: (Show a) => a -> t
    showT = stringT . show

    -- | Convert strict bytestring to textual value.
    bsT :: O.Bs -> t
    bsT = bzT . Bz.fromStrict

    -- | Convert lazy bytestring to textual value.
    bzT :: O.Bz -> t
    bzT = bsT . Bz.toStrict

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
    bsT          = Bs.toString
    bzT          = Bz.toString

-- | Strict text.
instance Textual O.Tx where
    tEmpty       = Tx.empty
    tIsEmpty     = Tx.null
    tJoin        = Tx.append

    tAdd         = Tx.cons
    tCut         = Tx.uncons
    tTake        = Tx.take
    tDrop        = Tx.drop

    tString      = Tx.unpack
    stringT      = Tx.pack
    bsT          = Tx.decodeUtf8With Err.lenientDecode
    bzT          = bsT . Bz.toStrict

-- | Lazy text.
instance Textual O.Tz where
    tEmpty       = Tz.empty
    tIsEmpty     = Tz.null
    tJoin        = Tz.append

    tAdd         = Tz.cons
    tCut         = Tz.uncons
    tTake        = Tz.take . fromIntegral
    tDrop        = Tz.drop . fromIntegral

    tString      = Tz.unpack
    stringT      = Tz.pack
    bsT          = bzT . Bz.fromStrict
    bzT          = Tz.decodeUtf8With Err.lenientDecode


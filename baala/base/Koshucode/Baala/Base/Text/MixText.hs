{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Mix text.

module Koshucode.Baala.Base.Text.MixText
  ( -- * Type
    MixText,

    -- * Constructor
    -- ** Text
    mixBs, mixBz,
    mixTx, mixTz,
    mixString, mixChar,
    -- ** Padding
    mixLeft, mixRight,
    -- ** Space
    mixSpace, mixSep, mixSep2,
    mix0, mix1, mix2, mix3, mix4,
    -- ** Number
    mixOct, mixDec, mixHex, mixSign,
    mixDecZero, mixNumZero,
    -- ** Newline
    mixLine, mixLines,
    mixSoft, mixHard,
    -- ** Other
    mixShow, mixEmpty,

    -- * Convert
    -- ** ByteString
    mixToBuilder,
    mixToBz,
    mixToString,
    mixToFlatString,

    -- ** Mix I/O
    putMix, putMixLn,
    hPutMix, hPutMixLn,
    putMixLines, hPutMixLines,
    writeMix,
  ) where

import Data.Monoid ((<>))
import qualified Control.Exception                    as E
import qualified Numeric                              as N
import qualified Data.ByteString                      as Bs
import qualified Data.ByteString.Builder              as B
import qualified Data.ByteString.Lazy                 as Bz
import qualified Data.ByteString.Lazy.UTF8            as Bu
import qualified Data.Default                         as Def
import qualified Data.Text                            as Tx
import qualified Data.Text.Encoding                   as Tx
import qualified Data.Text.Lazy                       as Tz
import qualified Data.Text.Lazy.Encoding              as Tz
import qualified System.IO                            as IO
import qualified Koshucode.Baala.Base.Text.Utility    as B
import qualified Koshucode.Baala.Base.Text.LineBreak  as B


-- --------------------------------------------  MixText

-- | Text mixable string, text, and bytestring.
data MixText
    = MixAppend   MixText MixText
    | MixBs       Bs.ByteString
    | MixBz       Bz.ByteString
    | MixTx       Tx.Text
    | MixTz       Tz.Text
    | MixString   String
    | MixSpace    Int
    | MixNewline  Bool
    | MixEmpty

-- | Show mix text without line break.
instance Show MixText where
    show mx = "MixText " ++ show (mixToStringDef mx)

-- | Test equality of mix texts slowly.
instance Eq MixText where
    x == y = mixToBzDef x == mixToBzDef y

-- | Compare mix texts slowly.
instance Ord MixText where
    compare x y = mixToBzDef x `compare` mixToBzDef y

-- | Empty mix text.
instance Def.Default MixText where
    def = MixEmpty

-- | Monoid like string.
instance Monoid MixText where
    mempty  = MixEmpty
    mappend = MixAppend


-- --------------------------------------------  Constructor

-- ----------------------  Text

-- | Create mix text from strict bytestring.
mixBs :: Bs.ByteString -> MixText
mixBs = MixBs

-- | Create mix text from lazy bytestring.
mixBz :: Bz.ByteString -> MixText
mixBz = MixBz

-- | Create mix text from strict text.
mixTx :: Tx.Text -> MixText
mixTx = MixTx

-- | Create mix text from lazy text.
mixTz :: Tz.Text -> MixText
mixTz = MixTz

-- | Create mix text from string.
--
--   >>> mixString "abc" <> mixString "def"
--   MixText "abcdef"

mixString :: String -> MixText
mixString = MixString

-- | Create mix text from char.
mixChar :: Char -> MixText
mixChar c = mixString [c]

-- | Put mix text to left edge in given-length char sequence.
--
--   >>> mixLeft '.' 10 $ mixString "abc"
--   MixText "abc......."

mixLeft :: Char -> Int -> MixText -> MixText
mixLeft c n mx = mixString $ B.padRightWith c n $ mixToStringDef mx

-- | Put mix text to right edge in given-length char sequence.
--
--   >>> mixRight '.' 10 $ mixString "abc"
--   MixText ".......abc"

mixRight :: Char -> Int -> MixText -> MixText
mixRight c n mx = mixString $ B.padLeftWith c n $ mixToStringDef mx

-- ----------------------  Space

-- | Create mix text of given-length spaces.
--
--   >>> mixString "|" <> mixSpace 4 <> mixString "|"
--   MixText "|    |"

mixSpace :: Int -> MixText
mixSpace = MixSpace

-- | Infix mix space.
mixSep :: MixText -> MixText -> MixText
mixSep l r = l <> mix1 <> r

mixSep2 :: MixText -> MixText -> MixText
mixSep2 l r = l <> mix2 <> r

-- | Empty space.
mix0 :: MixText
-- | One space.
mix1 :: MixText
-- | Two-length spaces.
mix2 :: MixText
-- | Three-length spaces.
mix3 :: MixText
-- | Four-length spaces.
mix4 :: MixText

mix0 = mixSpace 0
mix1 = mixSpace 1
mix2 = mixSpace 2
mix3 = mixSpace 3
mix4 = mixSpace 4

-- ----------------------  Number

-- | Mix text of octal number.
--
--  >>> mixOct (12 :: Int)
--  MixText "14"

mixOct :: (Integral n, Show n) => n -> MixText
mixOct = mixNum N.showOct

-- | Mix text of decimal number.
--
--  >>> mixDec (12 :: Int)
--  MixText "12"

mixDec :: (Integral n, Show n) => n -> MixText
mixDec = mixNum N.showInt

-- | Mix text of hexadecimal number.
--
--  >>> mixHex (12 :: Int)
--  MixText "c"

mixHex :: (Integral n, Show n) => n -> MixText
mixHex = mixNum N.showHex

mixNum :: (Integral n, Show n) => (n -> ShowS) -> n -> MixText
mixNum f n | n >= 0    = mixString (f n "")
           | otherwise = mixChar '-' <> mixString (f (abs n) "")

-- | Zero-padding decimal number.
--
--   >>> mixDecZero 6 (123 :: Int)
--   MixText "000123"
--
--   >>> mixDecZero 6 (-123 :: Int)
--   MixText "-00123"

mixDecZero :: (Integral n, Show n) => Int -> n -> MixText
mixDecZero = mixNumZero mixDec

mixNumZero :: (Integral n, Show n) => (n -> MixText) -> Int -> n -> MixText
mixNumZero f w n | n >= 0     = mixRight '0' w (f n)
                 | otherwise  = mixChar '-' <> mixRight '0' (w - 1) (f $ abs n)

mixSign :: (Num n, Ord n) => n -> MixText
mixSign n = case compare n 0 of
              GT -> mixChar '+'
              LT -> mixChar '-'
              EQ -> mixChar ' '

-- ----------------------  Newline

-- | Append line break.
mixLine :: MixText -> MixText
mixLine = (<> mixHard)

-- | Append line break to each mix texts.
mixLines :: [MixText] -> MixText
mixLines = mconcat . map mixLine

-- | Soft line break, i.e., suppress at the beginning of line.
mixSoft :: MixText
mixSoft = MixNewline False

-- | Hard line break, i.e., suppress only after auto line break.
mixHard :: MixText
mixHard = MixNewline True

-- ----------------------  Others

mixShow :: (Show a) => a -> MixText
mixShow = mixString . show

-- | Empty mix text.
--
--   >>> mixEmpty
--   MixText ""

mixEmpty :: MixText
mixEmpty = MixEmpty


-- --------------------------------------------  Concatenate

-- | Convert mix text to lazy bytestring builder.
mixToBuilder :: B.LineBreak -> MixText -> B.Builder
mixToBuilder lb mx =
    case mixBuild lb mx (Bw mempty 0) of
      (b, _, _) -> b

-- | Convert mix text to lazy bytestring.
mixToBz :: B.LineBreak -> MixText -> Bz.ByteString
mixToBz lb = B.toLazyByteString . mixToBuilder lb

mixToBzDef :: MixText -> Bz.ByteString
mixToBzDef = mixToBz Def.def

-- | Convert mix text to string.
mixToString :: B.LineBreak -> MixText -> String
mixToString lb = Bu.toString . mixToBz lb

-- | Convert mix text to string without line breaks.
mixToFlatString :: MixText -> String
mixToFlatString = mixToString B.noBreak

mixToStringDef :: MixText -> String
mixToStringDef = mixToString Def.def

-- ----------------------  Put

-- | Print mix text to the standard output.
putMix :: B.LineBreak -> MixText -> IO ()
putMix lb = hPutMix lb IO.stdout

-- | Print mix text and newline to the standard output.
putMixLn :: B.LineBreak -> MixText -> IO ()
putMixLn lb = hPutMixLn lb IO.stdout

-- | Print mix text to the given output handler.
hPutMix :: B.LineBreak -> IO.Handle -> MixText -> IO ()
hPutMix lb h = B.hPutBuilder h . mixToBuilder lb

-- | Print mix text and newline to the given output handler.
hPutMixLn :: B.LineBreak -> IO.Handle -> MixText -> IO ()
hPutMixLn lb h mx = B.hPutBuilder h $ mixToBuilder lb $ mixLine mx

-- | Print mix text lines to the standard output.
putMixLines :: B.LineBreak -> [MixText] -> IO ()
putMixLines lb = hPutMixLines lb IO.stdout

-- | Print mix text lines to the given output handler.
hPutMixLines :: B.LineBreak -> IO.Handle -> [MixText] -> IO ()
hPutMixLines lb h = hPutMixLn lb h . mixLines

-- | Write mix text to a file.
writeMix :: B.LineBreak -> FilePath -> MixText -> IO ()
writeMix lb path mx = E.bracket open IO.hClose body where
    open   = IO.openBinaryFile path IO.WriteMode
    body h = hPutMix lb h mx

-- ----------------------  Build

-- Builder, width of the builder, and next spaces.
type Bld = (B.Builder, Int, Int)

-- Auto break function.
type Auto = Bld -> Int -> B.Builder -> Bld

-- Hard break function.
type Hard = Bld -> Bld

-- Builder and width without next spaces.
pattern Bw b w = (b, w, -1)

-- After autobreak.
pattern Br b w = (b, w, -2)

bsSpaces :: Int -> Bs.ByteString
bsSpaces n = Bs.replicate n 32

mixBuild :: B.LineBreak -> MixText -> Bld -> Bld
mixBuild lb mx p = mixBreak (auto $ B.breakWidth lb) hard p mx where
    auto (Nothing) (b,_,s) _  b2  = Bw (b <> space s <> b2) 0
    auto (Just wd) (b,w,s) w2 b2
        | w + s + w2 > wd         = Br (b <> nl <> ind <> b2) (indw + w2)
        | otherwise               = Bw (b <> space s   <> b2) (w + s + w2)

    hard (Br b 0)                 = Bw (b) 0
    hard (b, _, _)                = Bw (b <> nl) 0

    nl           = continueBuilder lb
    (ind, indw)  = indentBuilder   lb
    space        = B.byteString . bsSpaces

mixBreak :: Auto -> Hard -> Bld -> MixText -> Bld
mixBreak auto hard = (<<) where
    bld << MixAppend x y   = bld << x << y
    bld << MixBs b         = cat bld (bsWidth b)       $ B.byteString b
    bld << MixBz b         = cat bld (bzWidth b)       $ B.lazyByteString b
    bld << MixTx t         = cat bld (txWidth t)       $ Tx.encodeUtf8Builder t
    bld << MixTz t         = cat bld (tzWidth t)       $ Tz.encodeUtf8Builder t
    bld << MixString s     = cat bld (B.stringWidth s) $ B.stringUtf8 s
    bld << MixSpace s'     = space bld s'
    bld << MixNewline b    = nl b bld
    bld << MixEmpty        = bld

    cat bld@(b, w, s) w2 b2
        | s < 0            = Bw (b <> b2) (w + w2)
        | otherwise        = auto bld w2 b2

    space (b, w, s) s'     = (b, w, max s s')

    nl False bld@(_, 0, _) = bld
    nl False bld           = hard bld
    nl True  bld           = hard bld

-- ----------------------  Width

bsWidth :: Bs.ByteString -> Int
bsWidth = Bs.length

bzWidth :: Bz.ByteString -> Int
bzWidth = fromInteger . toInteger . Bz.length

txWidth :: Tx.Text -> Int
txWidth = Tx.length

tzWidth :: Tz.Text -> Int
tzWidth = fromInteger . toInteger . Tz.length


-- --------------------------------------------  LineBreak

continueBuilder :: B.LineBreak -> B.Builder
continueBuilder B.LineBreak { B.breakContinue = nl } = B.stringUtf8 nl

indentBuilder :: B.LineBreak -> (B.Builder, Int)
indentBuilder B.LineBreak { B.breakIndent = i } = (B.stringUtf8 i, B.stringWidth i)


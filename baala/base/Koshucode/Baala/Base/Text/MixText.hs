{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.MixText
  ( -- * Type
    MixText,

    -- * Constructor
    Mix (..),
    -- ** Text
    mixBs, mixBz,
    mixTx, mixTz,
    mixString, mixChar,
    -- ** Padding
    mixLeft, mixRight,
    -- ** Space
    mixSpace, mix0, mix1, mix2, mix3, mix4,
    -- ** Number
    mixOct, mixDec, mixHex, mixSign,
    mixDecZero, mixNumZero,
    -- ** Other
    mixShow, mixNewline,

    -- * To text
    -- ** Mix
    putMix, hPutMix,
    mixToBuilder,
    mixToBz,
    mixToString,

    -- ** Mix list
    putMixList, hPutMixList,
    mixListToBuilder,
  ) where

import Data.Monoid ((<>))
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
import qualified Koshucode.Baala.Base.Text.LineBreak  as B
import qualified Koshucode.Baala.Base.Text.Utility    as B


-- --------------------------------------------  MixText

data MixText
    = MixAppend   MixText MixText
    | MixBs       Bs.ByteString
    | MixBz       Bz.ByteString
    | MixTx       Tx.Text
    | MixTz       Tz.Text
    | MixString   String
    | MixSpace    Int
    | MixNewline
    | MixEmpty

instance Show MixText where
    show mx = "MixText " ++ show (mixToStringDef mx)

instance Eq MixText where
    x == y = mixToBzDef x == mixToBzDef y

instance Ord MixText where
    compare x y = mixToBzDef x `compare` mixToBzDef y

instance Def.Default MixText where
    def = MixEmpty

instance Monoid MixText where
    mempty  = MixEmpty
    mappend = MixAppend


-- --------------------------------------------  Constructor

class Mix a where
    mix :: a -> MixText

instance Mix Bs.ByteString where
    mix = mixBs

instance Mix Bz.ByteString where
    mix = mixBz

instance Mix Tx.Text where
    mix = mixTx

instance Mix Tz.Text where
    mix = mixTz

instance Mix String where
    mix = mixString

instance Mix Char where
    mix = mixChar

instance Mix Int where
    mix = mixSpace

instance Mix [MixText] where
    mix = mconcat

instance Mix () where
    mix _ = MixEmpty

-- ----------------------  Text

mixBs :: Bs.ByteString -> MixText
mixBs = MixBs

mixBz :: Bz.ByteString -> MixText
mixBz = MixBz

mixTx :: Tx.Text -> MixText
mixTx = MixTx

mixTz :: Tz.Text -> MixText
mixTz = MixTz

mixString :: String -> MixText
mixString = MixString

mixChar :: Char -> MixText
mixChar c = mixString [c]

mixLeft :: Char -> Int -> MixText -> MixText
mixLeft c n mx = mixString $ B.padRightWith c n $ mixToStringDef mx

mixRight :: Char -> Int -> MixText -> MixText
mixRight c n mx = mixString $ B.padLeftWith c n $ mixToStringDef mx

-- ----------------------  Space

mixSpace :: Int -> MixText
mixSpace = MixSpace

mix0 :: MixText
mix1 :: MixText
mix2 :: MixText
mix3 :: MixText
mix4 :: MixText

mix0 = mixSpace 0
mix1 = mixSpace 1
mix2 = mixSpace 2
mix3 = mixSpace 3
mix4 = mixSpace 4

-- ----------------------  Number

mixOct :: (Integral n, Show n) => n -> MixText
mixOct = mixNum N.showOct

mixDec :: (Integral n, Show n) => n -> MixText
mixDec = mixNum N.showInt

mixHex :: (Integral n, Show n) => n -> MixText
mixHex = mixNum N.showHex

mixNum :: (Integral n, Show n) => (n -> ShowS) -> n -> MixText
mixNum f n | n >= 0    = mixString (f n "")
           | otherwise = mixChar '-' <> mixString (f (abs n) "")

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

-- ----------------------  Others

mixShow :: (Show a) => a -> MixText
mixShow = mixString . show

mixNewline :: MixText
mixNewline = MixNewline


-- --------------------------------------------  Concatenate

putMix :: B.LineBreak -> MixText -> IO ()
putMix = hPutMix IO.stdout

hPutMix :: IO.Handle -> B.LineBreak -> MixText -> IO ()
hPutMix h lb = B.hPutBuilder h . mixToBuilder lb

mixToBuilder :: B.LineBreak -> MixText -> B.Builder
mixToBuilder lb mx =
    case mixBuild lb mx (BW mempty 0) of
      --(b, 0, _) -> b
      (b, _, _) -> b <> newlineBuilder lb

mixToBz :: B.LineBreak -> MixText -> Bz.ByteString
mixToBz lb = B.toLazyByteString . mixToBuilder lb

mixToBzDef :: MixText -> Bz.ByteString
mixToBzDef = mixToBz Def.def

mixToString :: B.LineBreak -> MixText -> String
mixToString lb = Bu.toString . mixToBz lb

mixToStringDef :: MixText -> String
mixToStringDef = mixToString B.nolb

-- ----------------------  Mix list

putMixList :: B.LineBreak -> [MixText] -> IO ()
putMixList = hPutMixList IO.stdout

hPutMixList :: IO.Handle -> B.LineBreak -> [MixText] -> IO ()
hPutMixList h lb = B.hPutBuilder h . mixListToBuilder lb

mixListToBuilder :: B.LineBreak -> [MixText] -> B.Builder
mixListToBuilder lb = foldr f mempty where
    f mx b = mixToBuilder lb mx <> b

-- ----------------------  Build

-- Builder, width of the builder, and next spaces.
type Bld = (B.Builder, Int, Int)

type Break = Bld -> Int -> B.Builder -> Bld

-- Builder and width without next spaces.
pattern BW b w = (b, w, -1)

bsSpaces :: Int -> Bs.ByteString
bsSpaces n = Bs.replicate n 32

mixBuild :: B.LineBreak -> MixText -> Bld -> Bld
mixBuild lb mx p = mixBreak (brk $ B.breakWidth lb) newline p mx where
    brk (Nothing) (b,_,s) _  b2  = BW (b <> space s <> b2) 0
    brk (Just wd) (b,w,s) w2 b2
        | w + s + w2 > wd        = BW (b <> nl <> ind <> b2) (indw + w2)
        | otherwise              = BW (b <> space s   <> b2) (w + s + w2)

    newline (b,_,_) n b2         = BW (b <> nl <> b2) n

    nl           = continueBuilder lb
    (ind, indw)  = indentBuilder   lb
    space        = B.byteString . bsSpaces

mixBreak :: Break -> Break -> Bld -> MixText -> Bld
mixBreak brk newline = (<<) where
    bld << MixAppend x y   = bld << x << y
    bld << MixBs b         = cat bld (bsWidth b)       $ B.byteString b
    bld << MixBz b         = cat bld (bzWidth b)       $ B.lazyByteString b
    bld << MixTx t         = cat bld (txWidth t)       $ Tx.encodeUtf8Builder t
    bld << MixTz t         = cat bld (tzWidth t)       $ Tz.encodeUtf8Builder t
    bld << MixString s     = cat bld (B.stringWidth s) $ B.stringUtf8 s
    bld << MixSpace s'     = space bld s'
    bld << MixNewline      = newline bld 0 mempty
    bld << MixEmpty        = bld

    space (b, w, s) s'     = (b, w, max s s')
    cat (BW b w) w2 b2     = BW (b <> b2) (w + w2)
    cat bld      w2 b2     = brk bld w2 b2

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

newlineBuilder :: B.LineBreak -> B.Builder
newlineBuilder B.LineBreak { B.breakNewline = nl } = B.stringUtf8 nl

continueBuilder :: B.LineBreak -> B.Builder
continueBuilder B.LineBreak { B.breakContinue = nl } = B.stringUtf8 nl

indentBuilder :: B.LineBreak -> (B.Builder, Int)
indentBuilder B.LineBreak { B.breakIndent = i } = (B.stringUtf8 i, B.stringWidth i)


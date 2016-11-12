{-# OPTIONS_GHC -Wall #-}

-- | Utilities for term contents.

module Koshucode.Baala.Data.Content.Utility
  ( -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,

    -- * Conversion
    contentAp, contentMap,
    contentApText,
    contentMapTextList,

    -- * Encode
    contentString,
    mixBracketList,
    mixBracketSet,
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Data.Type                as D
import qualified Koshucode.Baala.Data.Class               as D


-- ----------------------  Get & Put

-- | Pair of get and put.
type CGetPut a c = (c -> a, a -> c)

-- | 'gText' and 'pText'.
gpText :: (D.CText c) => CGetPut String c
gpText = (D.gText, D.pText)

-- | 'gList' and 'pList'.
gpList :: (D.CList c) => CGetPut [c] c
gpList = (D.gList, D.pList)

-- | 'gSet' and 'pSet'.
gpSet :: (D.CSet c) => CGetPut [c] c
gpSet = (D.gSet, D.pSet)

-- | 'gSetSort' and 'pSet'.
gpSetSort :: (Ord c, D.CSet c) => CGetPut [c] c
gpSetSort = (D.gSetSort, D.pSet)


-- ----------------------  Conversion

-- | Apply function to internal value of content.
--
--   >>> contentAp gText pText reverse (the $ pText "abc") :: BaalaC
--   VText "cba"
--
contentAp :: (c -> a) -> (b -> c') -> (a -> b) -> c -> c'
contentAp get put f = put . f . get

-- | Map function to internal value of content.
contentMap :: (Functor f) => (c -> f a) -> (f b -> c') -> (a -> b) -> c -> c'
contentMap get put f = contentAp get put $ fmap f

-- | Apply function to internal string of text content.
--
--   >>> contentApText reverse (the $ pText "abc")
--   VText "cba"
--
contentApText :: (D.CText c) => O.StringMap -> O.Map c
contentApText = contentAp D.gText D.pText

-- | Map function to characters of internal string of text content.
--
--   >>> contentMapTextList (\c -> pText [c]) (the $ pText "abc")
--   VList [VText "a",VText "b",VText "c"]
--
contentMapTextList :: (D.CList c, D.CText c) => (Char -> c) -> O.Map c
contentMapTextList = contentMap D.gText D.pList


-- ----------------------  Encode

-- | Encode content to string.
--
--   >>> contentString (the $ pText "a")
--   "a"
--
contentString :: (D.CContent c) => c -> String
contentString = B.mixToFlatString. contentStringMix

-- | Convert content to pretty print doc.
contentStringMix :: (D.CContent c) => c -> B.MixText
contentStringMix c
    | D.isCode   c  = B.mixString $ D.gCode c
    | D.isText   c  = B.mixString $ D.gText c
    | D.isTerm   c  = B.mixString $ '/' : D.gTerm c
    | D.isDec    c  = B.mixString $ D.encodeDecimalCompact $ D.gDec c
    | D.isClock  c  = B.mixEncode $ D.gClock c
    | D.isTime   c  = B.mixEncode $ D.gTime c
    | D.isBool   c  = B.mixEncode $ D.gBool c
    | D.isEmpty  c  = B.mixEmpty
    | D.isEnd    c  = B.mixString "(/)"

    | D.isList   c  = mixBracketList $ B.mixJoinBar $ map contentStringMix $ D.gList c
    | D.isSet    c  = mixBracketSet  $ B.mixJoinBar $ map contentStringMix $ D.gSet c 
    | D.isTie    c  = B.mixString "<tie>"
    | D.isRel    c  = B.mixString "<rel>"
    | D.isInterp c  = B.mixString "<interp>"
    | D.isType   c  = B.mixString "<type>"
    | otherwise     = B.mixString "<?>"

-- | Enclose mix text in list brackets.
--
--   >>> mixBracketList $ B.mixString "a"
--   MixText "[ a ]"
--
mixBracketList :: B.MixText -> B.MixText
mixBracketList = B.mixBracketS S.listOpen S.listClose

-- | Enclose mix text in set brackets.
--
--   >>> mixBracketSet $ B.mixString "a"
--   MixText "{ a }"
--
mixBracketSet :: B.MixText -> B.MixText
mixBracketSet = B.mixBracketS S.setOpen S.setClose


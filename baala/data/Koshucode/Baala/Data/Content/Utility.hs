{-# OPTIONS_GHC -Wall #-}

-- | Utilities for term contents.

module Koshucode.Baala.Data.Content.Utility
  ( -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,

    -- * Utility
    contAp, contMap,
    contApTextToText,
    contMapTextToList,
    judgeContString,
    contString,
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
gpText :: (D.CText c) => CGetPut [Char] c
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


-- ----------------------  Utility

-- | Apply function to internal value of content.
--
--   >>> contAp gText pText reverse (pText "abc" :: BaalaC) :: BaalaC
--   VText "cba"
--
contAp :: (a' -> a) -> (b -> b') -> (a -> b) -> a' -> b'
contAp get put f = put . f . get

-- | Map function to internal value of content.
contMap :: (Functor f) => (a' -> f a) -> (f b -> b') -> (a -> b) -> a' -> b'
contMap get put f = contAp get put $ fmap f

contApTextToText :: (D.CText c) => O.Map String -> B.AbMap c
contApTextToText = contAp D.gText D.putText

contMapTextToList :: (D.CList c, D.CText c) => (Char -> c) -> B.AbMap c
contMapTextToList = contMap D.gText D.putList

-- | Convert term content to string value.
judgeContString :: (D.CContent c) => D.Judge c -> D.Judge String
judgeContString = (contString <$>)

-- | Convert content to string value.
--
--   >>> contString (pText "a" :: BaalaC)
--   "a"
--
contString :: (D.CContent c) => c -> String
contString = B.mixToFlatString. contStringMix

-- | Convert content to pretty print doc.
contStringMix :: (D.CContent c) => c -> B.MixText
contStringMix c
    | D.isCode   c  = B.mixString $ D.gCode c
    | D.isText   c  = B.mixString $ D.gText c
    | D.isTerm   c  = B.mixString $ '/' : D.gTerm c
    | D.isDec    c  = B.mixString $ D.encodeDecimalCompact $ D.gDec c
    | D.isClock  c  = B.mixEncode $ D.gClock c
    | D.isTime   c  = B.mixEncode $ D.gTime c
    | D.isBool   c  = B.mixEncode $ D.gBool c
    | D.isEmpty  c  = B.mixEmpty
    | D.isEnd    c  = B.mixString "(/)"

    | D.isList   c  = mixBracketList $ B.mixJoinBar $ map contStringMix $ D.gList c
    | D.isSet    c  = mixBracketSet  $ B.mixJoinBar $ map contStringMix $ D.gSet c 
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


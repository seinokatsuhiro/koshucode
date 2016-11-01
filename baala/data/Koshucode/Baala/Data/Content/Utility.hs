{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Content.Utility
  ( -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,

    -- * Utility
    isMember,
    contAp, contMap,
    contApTextToText,
    contMapTextToList,
    judgeContString,
    contString,
    mixBracketList,
    mixBracketSet,
    contMinimum, contMaximum,
    pTermSet, pTextSet, pTextList,
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

-- | Test membership between element and collection contents.
isMember :: (Eq c, D.CSet c, D.CList c) => c -> c -> Bool
isMember x xs | D.isSet xs  = x `elem` D.gSet xs
isMember x xs | D.isList xs = x `elem` D.gList xs
isMember _ _ = False

-- | Apply function to internal value of content.
contAp :: (c -> a) -> (b -> d) -> (a -> b) -> c -> d
contAp get put f = put . f . get

-- | Map function to internal value of content.
contMap :: (c -> [a]) -> ([b] -> d) -> (a -> b) -> c -> d
contMap get put f = contAp get put $ map f

contApTextToText :: (D.CText c) => O.Map String -> B.AbMap c
contApTextToText = contAp D.gText D.putText

contMapTextToList :: (D.CList c, D.CText c) => (Char -> c) -> B.AbMap c
contMapTextToList = contMap D.gText D.putList

judgeContString :: (D.CContent c) => D.Judge c -> D.Judge String
judgeContString = (contString <$>)

-- | Convert content to string value.
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

mixBracketList :: B.MixText -> B.MixText
mixBracketList = B.mixBracketS S.listOpen S.listClose

mixBracketSet :: B.MixText -> B.MixText
mixBracketSet = B.mixBracketS S.setOpen S.setClose

-- | Minimum content of contents list.
contMinimum :: (Ord c, D.CEnd c) => [c] -> c
contMinimum = B.minimumNull D.end

-- | Maximum content of contents list.
contMaximum :: (Ord c, D.CEmpty c) => [c] -> c
contMaximum = B.maximumNull D.empty

pTermSet :: (D.CTerm c, D.CSet c) => [String] -> c
pTermSet = D.pSet . map D.pTerm

pTextSet :: (D.CText c, D.CSet c) => [String] -> c
pTextSet = D.pSet . map D.pText

pTextList :: (D.CText c, D.CList c) => [String] -> c
pTextList = D.pList . map D.pText


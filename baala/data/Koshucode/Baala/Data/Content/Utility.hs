{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Content.Utility
  ( -- * Generic content
    CContent (..),
    typeOrder,

    -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,

    -- * Utility
    isMember,
    contAp, contMap,
    contApTextToText,
    contMapTextToList,
    contString,
    contMinimum, contMaximum,
    pTermSet, pTextSet, pTextList,
  ) where

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Data.Type                as D
import qualified Koshucode.Baala.Data.Content.Singleton   as D
import qualified Koshucode.Baala.Data.Content.Complex     as D
import qualified Koshucode.Baala.Data.Content.Simple      as D


-- ----------------------  Generic content

class (Ord c, B.Write c, B.MixShortEncode c, D.CTypeOf c,
       D.CEmpty c, D.CEnd c,
       D.CBool c, D.CCode c, D.CText c, D.CClock c, D.CTime c,
       D.CTerm c, D.CDec c, D.CType c, D.CInterp c,
       D.CList c, D.CSet c, D.CTie c, D.CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = B.foldM appendContent D.empty

typeOrder :: (CContent c) => c -> Int
typeOrder c
    -- empty
    | D.isEmpty    c = 1

    -- simple
    | D.isBool     c = 11
    | D.isDec      c = 12
    | D.isClock    c = 13
    | D.isTime     c = 14
    | D.isCode     c = 15
    | D.isTerm     c = 16
    | D.isText     c = 17

    -- complex
    | D.isList     c = 21
    | D.isSet      c = 22
    | D.isTie      c = 23
    | D.isRel      c = 24
    | D.isInterp   c = 25
    | D.isType     c = 27

    -- end
    | D.isEnd      c = 31
    | otherwise      = error "unknown content"


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

contApTextToText :: (D.CText c) => B.Map String -> B.AbMap c
contApTextToText = contAp D.gText D.putText

contMapTextToList :: (D.CList c, D.CText c) => (Char -> c) -> B.AbMap c
contMapTextToList = contMap D.gText D.putList

-- | Convert content to string value.
contString :: (CContent c) => c -> String
contString = show . contDoc

-- | Convert content to pretty print doc.
contDoc :: (CContent c) => c -> B.Doc
contDoc c
    | D.isEmpty  c  = B.doc ""
    | D.isBool   c  = B.writeDoc $ D.gBool c

    | D.isDec    c  = B.doc $ D.encodeDecimalCompact $ D.gDec c
    | D.isClock  c  = B.doc $ show $ D.writeClockBody $ D.gClock c
    | D.isTime   c  = B.doc $ B.writeString c

    | D.isCode   c  = B.doc $ D.gCode c
    | D.isText   c  = B.doc $ D.gText c
    | D.isTerm   c  = B.doc $ '/' : D.gTerm c

    | D.isList   c  = B.docWraps S.listOpen S.listClose $ B.writeBar B.nullShortener $ map contDoc $ D.gList c 
    | D.isSet    c  = B.docWraps S.setOpen  S.setClose  $ B.writeBar B.nullShortener $ map contDoc $ D.gSet c 

    | D.isTie    c  = B.doc "<tie>"
    | D.isRel    c  = B.doc "<rel>"
    | D.isInterp c  = B.doc "<interp>"
    | D.isType   c  = B.doc "<type>"
    | otherwise   = B.doc "<?>"

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


{-# OPTIONS_GHC -Wall #-}

-- | Content encoder.

module Koshucode.Baala.Data.Class.Encode
  ( contentString,
    mixBracketList,
    mixBracketSet,
  ) where

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Type                     as T
import qualified Koshucode.Baala.Data.Class.Complex       as D
import qualified Koshucode.Baala.Data.Class.Content       as D
import qualified Koshucode.Baala.Data.Class.Edge          as D
import qualified Koshucode.Baala.Data.Class.Simple        as D


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
    | D.isTerm   c  = B.mixString $ S.termNameString $ D.gTerm c
    | D.isDec    c  = B.mixString $ T.encodeDecimalCompact $ D.gDec c
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
mixBracketList = S.bracketWith S.bracketList

-- | Enclose mix text in set brackets.
--
--   >>> mixBracketSet $ B.mixString "a"
--   MixText "{ a }"
--
mixBracketSet :: B.MixText -> B.MixText
mixBracketSet = S.bracketWith S.bracketSet


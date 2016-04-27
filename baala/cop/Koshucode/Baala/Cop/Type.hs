{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Type
  ( copsType
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as D
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Cop.Message     as Msg



-- ----------------------
-- $Operators
--
--  [@to-dec@]     Convert to decimal.
--
--  [@to-list@]    Convert to list.
--
--  [@to-set@]     Convert to set.
--
--  [@to-text@]    Convert to text.
--

copsType :: (D.CContent c) => [D.Cop c]
copsType =
    [ D.CopCalc  (D.copNormal "to-dec")       copToDec
    , D.CopCalc  (D.copNormal "to-list")      copToList
    , D.CopCalc  (D.copNormal "to-set")       copToSet
    , D.CopCalc  (D.copNormal "to-text")      copToText
    ]

typeUnmatch :: D.CTypeOf c => [B.Ab c] -> B.Ab c
typeUnmatch _ = Msg.unmatchType ""

copToDec :: (D.CContent c) => D.CopCalc c
copToDec = op where
    op [Right c] | D.isText c  = case D.litDecimal $ D.gText c of
                                   Right n  -> Right $ D.pDec n
                                   Left _   -> Right c
                 | D.isBool c  = case D.gBool c of
                                   True   -> Right $ D.pInt 1
                                   False  -> Right $ D.pInt 0
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copToText :: (D.CContent c) => D.CopCalc c
copToText = op where
    op [Right c] = Right $ D.pText $ show $ toText c
    op xs = typeUnmatch xs

toText :: (D.CContent c) => c -> B.Doc
toText c
    | D.isText   c  = B.doc $ D.gText c
    | D.isDec    c  = B.doc $ D.encodeDecimalCompact $ D.gDec c
    | D.isBool   c  = B.writeDoc $ D.gBool c
    | D.isEmpty  c  = B.doc ""
    | D.isClock  c  = B.doc $ show $ D.writeClockBody $ D.gClock c
    | D.isTime   c  = B.doc $ B.writeString c
    | D.isTerm   c  = B.doc $ '/' : D.gTerm c

    | D.isList   c  = B.docWraps D.listOpen D.listClose $ B.writeBar B.nullShortener $ map toText $ D.gList c 
    | D.isSet    c  = B.docWraps D.setOpen  D.setClose $ B.writeBar B.nullShortener $ map toText $ D.gSet c 
    | D.isTie    c  = B.doc "<tie>"
    | D.isRel    c  = B.doc "<rel>"
    | D.isInterp c  = B.doc "<interp>"
    | D.isType   c  = B.doc "<type>"
    | otherwise     = B.doc "<?>"

copToList :: (D.CContent c) => D.CopCalc c
copToList = op where
    op [Right c] | D.isSet c   = Right $ D.pList $ D.gSet c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copToSet :: (D.CContent c) => D.CopCalc c
copToSet = op where
    op [Right c] | D.isList c  = Right $ D.pSet $ D.gList c
                 | otherwise   = Right c
    op xs = typeUnmatch xs


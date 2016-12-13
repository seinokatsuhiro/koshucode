{-# OPTIONS_GHC -Wall #-}

-- | Content operators on types.

module Koshucode.Baala.Cop.Type
  ( -- * Operator
    copsType,

    -- * Implementation
    copToDec,

    copIsEmpty, copIsEnd,
    copIsBool, copIsDec, copIsClock, copIsTime,
    copIsCode, copIsTerm, copIsText,
  ) where

import qualified Koshucode.Baala.Overture        as O
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

-- | Content operators on types.
copsType :: (D.CContent c) => [D.Cop c]
copsType =
    [ D.CopCalc  (D.copNormal "to-dec")       copToDec
    , D.CopCalc  (D.copNormal "to-list")      copToList
    , D.CopCalc  (D.copNormal "to-set")       copToSet
    , D.CopCalc  (D.copNormal "to-text")      copToText

    , D.CopCalc  (D.copNormal "empty?")       copIsEmpty
    , D.CopCalc  (D.copNormal "bool?")        copIsBool
    , D.CopCalc  (D.copNormal "dec?")         copIsDec
    , D.CopCalc  (D.copNormal "clock?")       copIsClock
    , D.CopCalc  (D.copNormal "time?")        copIsTime
    , D.CopCalc  (D.copNormal "code?")        copIsCode
    , D.CopCalc  (D.copNormal "term?")        copIsTerm
    , D.CopCalc  (D.copNormal "text?")        copIsText
    , D.CopCalc  (D.copNormal "end?")         copIsEnd
    ]

-- | Convert to decimal.
--
--   >>> to-dec "12.5"
--   12.5
--
--   >>> to-dec "a"
--   "a"
--
copToDec :: (D.CContent c) => D.CopCalc c
copToDec = op where
    op [Right c] | D.isText c  = case D.decodeDecimal $ D.gText c of
                                   Right n  -> Right $ D.pDec n
                                   Left _   -> Right c
                 | D.isBool c  = case D.gBool c of
                                   True   -> Right $ D.pInt 1
                                   False  -> Right $ D.pInt 0
                 | otherwise   = Right c
    op xs = Msg.badArg xs

copToText :: (D.CContent c) => D.CopCalc c
copToText = op where
    op [Right c] = Right $ D.pText $ D.contentString c
    op xs = Msg.badArg xs

copToList :: (D.CContent c) => D.CopCalc c
copToList = op where
    op [Right c] | D.isSet c   = Right $ D.pList $ D.gSet c
                 | otherwise   = Right c
    op xs = Msg.badArg xs

copToSet :: (D.CContent c) => D.CopCalc c
copToSet = op where
    op [Right c] | D.isList c  = Right $ D.pSet $ D.gList c
                 | otherwise   = Right c
    op xs = Msg.badArg xs

-- ---------------------------------  Type test

copTestType :: (D.CBool c) => O.Test c -> D.CopCalc c
copTestType test [Right c] | test c  = D.putTrue
copTestType _ _                      = D.putFalse

-- | Test content is the empty.
--
--   >>> empty? ()
--   (+)
--
--   >>> empty? "a"
--   (-)
--
copIsEmpty :: (D.CBool c, D.CEmpty c) => D.CopCalc c
copIsEmpty = copTestType D.isEmpty

-- | Test content is the end.
--
--   >>> end? (/)
--   (+)
--
--   >>> end? "a"
--   (-)
--
copIsEnd :: (D.CBool c, D.CEnd c) => D.CopCalc c
copIsEnd = copTestType D.isEnd

-- | Test content is a boolean.
--
--   >>> bool? (+)
--   (+)
--
--   >>> bool? "a"
--   (-)
--
copIsBool :: (D.CBool c) => D.CopCalc c
copIsBool = copTestType D.isBool

-- | Test content is a decimal number.
--
--   >>> dec? 12.5
--   (+)
--
--   >>> dec? "a"
--   (-)
--
copIsDec :: (D.CBool c, D.CDec c) => D.CopCalc c
copIsDec = copTestType D.isDec

-- | Test content is a clock.
--
--   >>> clock? |12:00|
--   (+)
--
--   >>> clock? "a"
--   (-)
--
copIsClock :: (D.CBool c, D.CClock c) => D.CopCalc c
copIsClock = copTestType D.isClock

-- | Test content is a time.
--
--   >>> time? 2013-04-18
--   (+)
--
--   >>> time? "a"
--   (-)
--
copIsTime :: (D.CBool c, D.CTime c) => D.CopCalc c
copIsTime = copTestType D.isTime

-- | Test content is a code.
--
--   >>> code? 'a
--   (+)
--
--   >>> code? "a"
--   (-)
--
copIsCode :: (D.CBool c, D.CCode c) => D.CopCalc c
copIsCode = copTestType D.isCode

-- | Test content is a term name.
--
--   >>> term? '/a
--   (+)
--
--   >>> term? "a"
--   (-)
--
copIsTerm :: (D.CBool c, D.CTerm c) => D.CopCalc c
copIsTerm = copTestType D.isTerm

-- | Test content is a text.
--
--   >>> text? 12.5
--   (-)
--
--   >>> text? "a"
--   (+)
--
copIsText :: (D.CBool c, D.CText c) => D.CopCalc c
copIsText = copTestType D.isText


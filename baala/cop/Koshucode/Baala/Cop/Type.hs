{-# OPTIONS_GHC -Wall #-}

-- | Content operators on types.

module Koshucode.Baala.Cop.Type
  ( -- * Operator
    copsType,

    -- * Implementation
    -- ** Type conversion
    copToDec,

    -- ** Type test
    -- *** Edge type
    copIsEmpty, copIsEnd,
    -- *** Simple type
    copIsBool, copIsDec, copIsClock, copIsTime,
    copIsCode, copIsTerm, copIsText,
    -- *** Complex type
    copIsList, copIsSet,
    copIsTie, copIsRel, copIsInterp,
    copIsType,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Rop.Base.Message  as Msg



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
    , D.CopCalc  (D.copNormal "list?")        copIsList
    , D.CopCalc  (D.copNormal "set?")         copIsSet
    , D.CopCalc  (D.copNormal "tie?")         copIsTie
    , D.CopCalc  (D.copNormal "rel?")         copIsRel
    , D.CopCalc  (D.copNormal "interp?")      copIsInterp
    , D.CopCalc  (D.copNormal "type?")        copIsType
    , D.CopCalc  (D.copNormal "end?")         copIsEnd
    ]

-- ============================================  Conversion

-- | Convert to decimal.
--
--   >>> to-dec "12.5"
--   12.5
--
--   >>> to-dec "a"
--   "a"
--
copToDec :: (D.CContent c) => D.CopCalc c
copToDec [Right c] = Right $ D.toDec c
copToDec xs = Msg.badArg xs

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

-- ============================================  Type test

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

-- | Test content is a list.
--
--   >>> list? [ 0 | 1 ]
--   (+)
--
--   >>> list? "a"
--   (-)
--
copIsList :: (D.CBool c, D.CList c) => D.CopCalc c
copIsList = copTestType D.isList

-- | Test content is a set.
--
--   >>> set? { 0 | 1 }
--   (+)
--
--   >>> set? "a"
--   (-)
--
copIsSet :: (D.CBool c, D.CSet c) => D.CopCalc c
copIsSet = copTestType D.isSet

-- | Test content is a tie.
--
--   >>> tie? {- /a 0 /b 1 -}
--   (+)
--
--   >>> tie? "a"
--   (-)
--
copIsTie :: (D.CBool c, D.CTie c) => D.CopCalc c
copIsTie = copTestType D.isTie

-- | Test content is a rel.
--
--   >>> rel? {= /a /b [ 0 | 1 ] =}
--   (+)
--
--   >>> rel? "a"
--   (-)
--
copIsRel :: (D.CBool c, D.CRel c) => D.CopCalc c
copIsRel = copTestType D.isRel

-- | Test content is a interp.
--
--   >>> interp? {| /x is equal to /y . |}
--   (+)
--
--   >>> interp? "a"
--   (-)
--
copIsInterp :: (D.CBool c, D.CInterp c) => D.CopCalc c
copIsInterp = copTestType D.isInterp

-- | Test content is a type.
--
--   >>> type? [- text -]
--   (+)
--
--   >>> type? "a"
--   (-)
--
copIsType :: (D.CBool c, D.CType c) => D.CopCalc c
copIsType = copTestType D.isType


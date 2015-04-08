{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Type
  ( copsType
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Message      as Msg



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

copsType :: (C.CContent c) => [C.Cop c]
copsType =
    [ C.CopCalc  (C.copNormal "to-dec")       copToDec
    , C.CopCalc  (C.copNormal "to-list")      copToList
    , C.CopCalc  (C.copNormal "to-set")       copToSet
    , C.CopCalc  (C.copNormal "to-text")      copToText
    ]

typeUnmatch :: C.CTypeOf c => [B.Ab c] -> B.Ab c
typeUnmatch _ = Msg.unmatchType ""

copToDec :: (C.CContent c) => C.CopCalc c
copToDec = op where
    op [Right c] | C.isText c  = case B.litDecimal $ C.gText c of
                                   Right n  -> Right $ C.pDec n
                                   Left _   -> Right c
                 | C.isBool c  = case C.gBool c of
                                   True   -> Right $ C.pDecFromInt 1
                                   False  -> Right $ C.pDecFromInt 0
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copToText :: (C.CContent c) => C.CopCalc c
copToText = op where
    op [Right c] | C.isDec  c  = Right $ C.pText $ B.decimalString $ C.gDec c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copToList :: (C.CContent c) => C.CopCalc c
copToList = op where
    op [Right c] | C.isSet c   = Right $ C.pList $ C.gSet c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copToSet :: (C.CContent c) => C.CopCalc c
copToSet = op where
    op [Right c] | C.isList c  = Right $ C.pSet $ C.gList c
                 | otherwise   = Right c
    op xs = typeUnmatch xs


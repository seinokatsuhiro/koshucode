{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Type
  ( copsType
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Rop.Flat.Message     as Msg



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
    op [Right c] | D.isDec  c  = Right $ D.pText $ D.decimalString $ D.gDec c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

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


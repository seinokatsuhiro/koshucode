{-# OPTIONS_GHC -Wall #-}

-- | Content operators on types.

module Koshucode.Baala.Cop.Type
  ( copsType
    -- $Operators
  ) where

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
    ]

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


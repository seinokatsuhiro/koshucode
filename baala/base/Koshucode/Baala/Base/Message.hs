{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Message
  ( -- * Abortables
    abCode,
    abTree,
  
    -- * Base package
    bug,
    adlib,
    adlibs,
    extraCloseBracket,
    extraOpenBracket,
    notFound,
    notImplemented,
    (<!!>),
  ) where

import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B


-- ----------------------  Abortables

abCode :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCode = B.abortable "code"

abTree :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abTree = B.abortable "tree"


-- ----------------------  Base package

-- | BUG: reason
bug :: String -> B.Ab a
bug reason = Left $ B.abortBecause $ "BUG: " ++ reason

-- | AD-LIB: reason
adlib :: String -> B.Ab a
adlib reason = Left $ B.abortBecause $ "AD-LIB: " ++ reason

adlibs :: [String] -> B.Ab a
adlibs = Left . B.abortLines "AD-LIB"

-- | Extra close bracket
extraCloseBracket :: B.Ab a
extraCloseBracket = Left $ B.abortBecause "Extra close bracket"

-- | Unclosed open bracket
extraOpenBracket :: B.Ab a
extraOpenBracket = Left $ B.abortBecause "Unclosed open bracket"

-- | Not found
notFound :: String -> B.Ab a
notFound = Left . B.abortLine "Not found"

-- | Not implemented
notImplemented :: String -> B.Ab a
notImplemented = Left . B.abortLine "Not implemented"

-- | Lookup association list. This function may abort.
(<!!>) :: [B.Named b] -> String -> B.Ab b
(<!!>) assoc key = loop assoc where
    loop [] = notFound key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs



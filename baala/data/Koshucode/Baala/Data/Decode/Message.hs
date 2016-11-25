{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Decode.Message
  ( -- * Abortable
    abLiteral,
    -- * Message
    nothing,
    oddRelation,
    quoteType,
    reqFlatName,
    reqRelTuple,
    reqTermName,
    unkBracket,
    unkType,
    unkWord,
    -- * Utility
    expectActual,
    expect2Actual,
  ) where

import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S

-- | Abortable scope for literal.
abLiteral :: S.TTree -> O.Map (B.Ab b)
abLiteral = B.abortable "literal"

-- | Nothing
nothing :: B.Ab a
nothing = Left $ B.abortBecause "Nothing"

-- | Odd relation literal
oddRelation :: Int -> Int -> B.Ab a
oddRelation e a  = Left $ B.abortLines "Odd relation literal"
                        $ expectActual (len e) (len a)
    where len n = show n ++ " contents"

-- | Quoted type name
quoteType :: String -> B.Ab a
quoteType = Left . B.abortLine "Quoted type name"

-- | Require flat name
reqFlatName :: S.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = S.tokenContent tok

-- | Require tuple in list
reqRelTuple :: B.Ab a
reqRelTuple = Left $ B.abortBecause "Require tuple in list"

-- | Require term name
reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

-- | Unknown bracket
unkBracket :: B.Ab a
unkBracket = Left $ B.abortBecause "Unknown bracket"

-- | Unknown type name
unkType :: String -> B.Ab a
unkType = Left . B.abortLine "Unknown type name"

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = Left . B.abortLine "Unknown word"

-- | Expect and actual.
expectActual :: String -> String -> [String]
expectActual e a       = [ "Expect " ++ e
                         , "Actual " ++ a ]

-- | Expect (2 lines) and actual.
expect2Actual :: String -> String -> String -> [String]
expect2Actual e1 e2 a  = [ "Expect " ++ e1
                         , "       " ++ e2
                         , "Actual " ++ a ]


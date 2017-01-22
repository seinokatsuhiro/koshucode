{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Decode.Message
  ( -- * Abortable
    abLiteral,
    -- * Message
    nothing,
    missPairTerm,
    oddRelation,
    quoteType,
    reqFlatName,
    reqRelTuple,
    reqTermName,
    unkBracket,
    unkContent,
    unkType,
    -- * Utility
    expectActual,
    expect2Actual,
  ) where

import qualified Koshucode.Baala.Base            as B

-- | Abortable scope for literal.
abLiteral :: (B.GetCodePos cp) => B.Abortable cp b
abLiteral = B.abortable "literal"

-- | Nothing
nothing :: B.Ab a
nothing = B.leftBecause "Nothing"

-- | Missing pairing term
missPairTerm :: B.Ab a
missPairTerm = B.leftBecause "Missing pairing term"

-- | Odd relation literal
oddRelation :: Int -> Int -> B.Ab a
oddRelation e a  = B.leftLines "Odd relation literal"
                        $ expectActual (len e) (len a)
    where len n = show n ++ " contents"

-- | Quoted type name
quoteType :: String -> B.Ab a
quoteType = B.leftLine "Quoted type name"

-- | Require flat name
reqFlatName :: B.Ab a
reqFlatName = B.leftBecause "Require flat name"

-- | Require tuple in list
reqRelTuple :: B.Ab a
reqRelTuple = B.leftBecause "Require tuple in list"

-- | Require term name
reqTermName :: B.Ab a
reqTermName = B.leftBecause "Require term name"

-- | Unknown bracket
unkBracket :: B.Ab a
unkBracket = B.leftBecause "Unknown bracket"

-- | Unknown content
unkContent :: B.Ab a
unkContent = B.leftBecause "Unknown content"

-- | Unknown type name
unkType :: String -> B.Ab a
unkType = B.leftLine "Unknown type name"

-- | Expect and actual.
expectActual :: String -> String -> [String]
expectActual e a       = [ "Expect " ++ e
                         , "Actual " ++ a ]

-- | Expect (2 lines) and actual.
expect2Actual :: String -> String -> String -> [String]
expect2Actual e1 e2 a  = [ "Expect " ++ e1
                         , "       " ++ e2
                         , "Actual " ++ a ]


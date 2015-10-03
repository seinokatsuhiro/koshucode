{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Content.Message
  ( -- * Abortable
    abLiteral,
    abortableTree,
    abortableTrees,
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
    unmatchType,
    -- * Utility
    expectActual,
    expect2Actual,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Data.Token   as D

abLiteral :: D.TTreeTo (B.Map (B.Ab b))
abLiteral = abortableTree "literal"

-- | Same as 'abortable' except for using 'D.TTree'
--   instead of list of 'D.Token'.
abortableTree :: String -> D.TTreeTo (B.Map (B.Ab b))
abortableTree tag = B.abortable tag . B.untree

-- | Same as 'abortable' except for using list of 'D.TTree'
--   instead of list of 'D.Token'.
abortableTrees :: String -> D.TTreesTo (B.Map (B.Ab b))
abortableTrees tag = B.abortable tag . B.untrees

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
reqFlatName :: D.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = D.tokenContent tok

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

unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = Left . B.abortLine "Unknown word"

expectActual :: String -> String -> [String]
expectActual e a       = [ "Expect " ++ e
                         , "Actual " ++ a ]

expect2Actual :: String -> String -> String -> [String]
expect2Actual e1 e2 a  = [ "Expect " ++ e1
                         , "       " ++ e2
                         , "Actual " ++ a ]


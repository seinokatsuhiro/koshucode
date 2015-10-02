{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
  ( module Koshucode.Baala.Base.Message,
    module Koshucode.Baala.Data.Message,
    module Koshucode.Baala.Core.Assert.Message,
    module Koshucode.Baala.Core.Lexmap.Message,
    module Koshucode.Baala.Core.Relkit.Message,
  
    abClause,
  
    dupPrefix,
    dupReplacement,
    emptyLiteral,
    httpError,
    invalidPrefix,
    noFile,
    sameIOPoints,
    unkClause,
    unkCop,
    unresPrefix,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Data as D
import Koshucode.Baala.Base.Message
import Koshucode.Baala.Data.Message
import Koshucode.Baala.Core.Assert.Message
import Koshucode.Baala.Core.Lexmap.Message
import Koshucode.Baala.Core.Relkit.Message


-- ----------------------  Abortables

abClause :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abClause = B.abortable "clause"



-- ----------------------  Core package

-- | Duplicate prefix
dupPrefix :: [String] -> B.Ab a
dupPrefix = Left . B.abortLine "Duplicate prefix" . unwords

-- | Duplicate replacement
dupReplacement :: [String] -> B.Ab a
dupReplacement = Left . B.abortLine "Duplicate replacement" . unwords

-- | Empty literal
emptyLiteral :: B.Ab a
emptyLiteral = Left $ B.abortBecause "Empty literal"

-- | HTTP Error
httpError :: String -> Int -> String -> B.Ab a
httpError url code msg = Left $ B.abortLines "HTTP Error" detail where
    detail | code == 0  = [msg, url]
           | otherwise  = [show code ++ " " ++ msg, url]

-- | Invalid prefix character
invalidPrefix :: [String] -> B.Ab a
invalidPrefix = Left . B.abortLine "Invalid prefix character" . unwords

-- | File not found
noFile :: FilePath -> String -> B.Ab a
noFile "" path = Left $ B.abortLine "File not found" path
noFile cd path = Left $ B.abortLines "File not found" [path, "directory: " ++ cd]

-- | Same I/O points
sameIOPoints :: B.IOPoint -> B.Ab a
sameIOPoints = Left . B.abortLine "Same I/O points" . B.ioPointText

-- | Unknown clause
unkClause :: [String] -> B.Ab a
unkClause = Left . B.abortLines "Unknown clause"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = Left $ B.abortLine "Unresolved prefix"
                       $ "Require short definition : short " ++ pre ++ " ..."


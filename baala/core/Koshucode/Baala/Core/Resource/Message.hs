{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Resource.Message
  ( -- * Abortable
    abClause,
    -- * Message
    dupPrefix,
    dupReplacement,
    httpError,
    invalidPrefix,
    noFile,
    sameIOPoints,
    unkClause,
    unresPrefix,
  ) where

import qualified Koshucode.Baala.Base as B

abClause :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abClause = B.abortable "clause"

-- | Duplicate prefix
dupPrefix :: [String] -> B.Ab a
dupPrefix = Left . B.abortLine "Duplicate prefix" . unwords

-- | Duplicate replacement
dupReplacement :: [String] -> B.Ab a
dupReplacement = Left . B.abortLine "Duplicate replacement" . unwords

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

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = Left $ B.abortLine "Unresolved prefix"
                       $ "Require short definition : short " ++ pre ++ " ..."

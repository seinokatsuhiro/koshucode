{-# OPTIONS_GHC -Wall #-}

-- | Message list.

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

import qualified Koshucode.Baala.Overture  as O
import qualified Koshucode.Baala.Base      as B

-- | Abortable scope for clause.
abClause :: (B.GetCodePos cp) => B.Abortable cp b
abClause = B.abortable "clause"

-- | Duplicate prefix
dupPrefix :: (O.Textual t) => [t] -> B.Ab a
dupPrefix = B.leftLine "Duplicate prefix" . O.tUnwords

-- | Duplicate replacement
dupReplacement :: (O.Textual t) => [t] -> B.Ab a
dupReplacement = B.leftLine "Duplicate replacement" . O.tUnwords

-- | HTTP Error
httpError :: String -> Int -> String -> B.Ab a
httpError url code msg = B.leftLines "HTTP Error" detail where
    detail | code == 0  = [msg, url]
           | otherwise  = [show code ++ " " ++ msg, url]

-- | Invalid prefix character
invalidPrefix :: (O.Textual t) => [t] -> B.Ab a
invalidPrefix = B.leftLine "Invalid prefix character" . O.tUnwords

-- | File not found
noFile :: FilePath -> String -> B.Ab a
noFile "" path = B.leftLine "File not found" path
noFile cd path = B.leftLines "File not found" [path, "directory: " ++ cd]

-- | Same I/O points
sameIOPoints :: B.IOPoint -> B.Ab a
sameIOPoints = B.leftLine "Same I/O points" . B.ioPointText

-- | Unknown clause
unkClause :: [String] -> B.Ab a
unkClause = B.leftLines "Unknown clause"

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = B.leftLine "Unresolved prefix"
                       $ "Require short definition : short " O.++ pre O.++ " ..."

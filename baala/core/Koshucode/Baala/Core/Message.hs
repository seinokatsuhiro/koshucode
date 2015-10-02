{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
  ( module Koshucode.Baala.Base.Message,
    module Koshucode.Baala.Data.Message,
    module Koshucode.Baala.Core.Assert.Message,
    module Koshucode.Baala.Core.Lexmap.Message,
  
    abClause,
    abOption,
    abRelmap,
    abRun,
    abSpecialize,
  
    dupPrefix,
    dupReplacement,
    emptyLiteral,
    httpError,
    invalidPrefix,
    noFile,
    sameIOPoints,
    unkClause,
    unkCop,
    unkNestRel,
    unkNestVar,
    unresPrefix,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Data as D
import Koshucode.Baala.Base.Message
import Koshucode.Baala.Data.Message
import Koshucode.Baala.Core.Assert.Message
import Koshucode.Baala.Core.Lexmap.Message


-- ----------------------  Abortables

abClause :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abClause = B.abortable "clause"

abOption :: D.TTreesTo (B.Map (B.Ab b))
abOption = D.abortableTrees "option"

abRelmap :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRelmap = B.abortable "relmap"

abRun :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRun = B.abortable "run"

abSpecialize :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSpecialize = B.abortable "specialize"


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

-- | Unknown nested relation
unkNestRel :: D.Token -> String -> [String] -> B.Ab a
unkNestRel p n rs = Left $ B.abortLines "Unknown nested relation" $ ref : rs
    where ref = "/" ++ n ++ " in " ++ D.tokenContent p

unkNestVar :: String -> [D.Token] -> [((D.Token, D.Local String), D.Head)] -> B.Ab a
unkNestVar n ls ds = Left $ B.abortLines "Unknown nested relation reference"
                           $ ("search" : map indent dynamic)
                          ++ ("for"    : map indent lexical)
    where lexical = map (text $ D.LocalSymbol n) ls
          dynamic = map f ds
          f ((tk, k), _) = text k tk
          text (D.LocalSymbol k) tk = unwords ["nested relation", quote k, "in", tokenAtPoint tk]
          text (D.LocalNest   k) tk = unwords ["nested relation", term  k, "in", tokenAtPoint tk]
          indent    = ("  " ++)
          term      = ('/' :)

tokenAtPoint :: D.Token -> String
tokenAtPoint tok = unwords ws where
    ws    = [D.tokenContent tok, "at L" ++ line, "C" ++ col]
    cp    = B.codePt tok
    line  = show $ B.codePtLineNo   cp
    col   = show $ B.codePtColumnNo cp

quote :: B.Map String
quote s = "'" ++ s ++ "'"

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = Left $ B.abortLine "Unresolved prefix"
                       $ "Require short definition : short " ++ pre ++ " ..."


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
  ( -- * Base package
    module Koshucode.Baala.Base.Message,
    module Koshucode.Baala.Data.Message,
  
    -- * Abortables
    abAssert,
    abAttr,
    abAttrTrees,
    abClause,
    abLexmap,
    abOption,
    abRelmap,
    abRun,
    abSlot,
    abSlotTree,
    abSpecialize,
  
    -- * Core package
    ambRelmap,
    dupAttr,
    dupPrefix,
    dupReplacement,
    emptyLiteral,
    extraAttr,
    httpError,
    invalidPrefix,
    noFile,
    noSlotName,
    noSlotIndex,
    reqAttr,
    reqAttrName,
    reqGroup,
    sameIOPoints,
    unexpAttr,
    unexpAttr0,
    unexpAttr1,
    unexpAttr2,
    unexpAttr3,
    unexpAttr4,
    unexpAttr1V,
    unexpAttr1Q,
    unkClause,
    unkCop,
    unkNestRel,
    unkNestVar,
    unkOption,
    unkRelmap,
    unresPrefix,
  ) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Base.Message
import Koshucode.Baala.Data.Message


-- ----------------------  Abortables

abAssert :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAssert = B.abortable "assert"

abAttr :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAttr = B.abortable "attr"

abAttrTrees :: B.TTreesTo (B.Map (B.Ab b))
abAttrTrees = B.abortableTrees "attr"

abClause :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abClause = B.abortable "clause"

abLexmap :: B.TTreesTo (B.Map (B.Ab b))
abLexmap = B.abortableTrees "lexmap"

abOption :: B.TTreesTo (B.Map (B.Ab b))
abOption = B.abortableTrees "option"

abRelmap :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRelmap = B.abortable "relmap"

abRun :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRun = B.abortable "run"

abSlot :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSlot = B.abortable "slot"

abSlotTree :: B.TTreeTo (B.Map (B.Ab b))
abSlotTree = B.abortableTree "slot"

abSpecialize :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSpecialize = B.abortable "specialize"


-- ----------------------  Core package

-- | Ambiguous relmaps
ambRelmap :: String -> [d] -> B.Ab a
ambRelmap name ds = Left $ B.abortLine "Ambiguous relmaps"
                         $ name ++ " (" ++ show (length ds) ++ ")"

-- | Unexpected attribute / Duplicate
dupAttr :: [String] -> B.Ab a
dupAttr ns = unexpAttr $ "Duplicate " ++ unwords (map B.showTermName ns)

-- | Duplicate prefix
dupPrefix :: [String] -> B.Ab a
dupPrefix = Left . B.abortLine "Duplicate prefix" . unwords

-- | Duplicate replacement
dupReplacement :: [String] -> B.Ab a
dupReplacement = Left . B.abortLine "Duplicate replacement" . unwords

-- | Empty literal
emptyLiteral :: B.Ab a
emptyLiteral = Left $ B.abortBecause "Empty literal"

-- | Extra attribute
extraAttr :: B.Ab a
extraAttr = Left $ B.abortBecause "Extra attribute"

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

-- | No slot content
noSlotName :: Int -> String -> B.Ab a
noSlotName n name = Left $ B.abortLine "No slot content" $ detail n where
    detail 0 = "Positional attribute @'" ++ name
    detail 1 = "Named attribute -"       ++ name
    detail 2 = "Global slot @@"          ++ name
    detail a = "Unknown slot level "     ++ show a

-- | No slot content
noSlotIndex :: [String] -> Int -> B.Ab a
noSlotIndex xs n = Left $ B.abortLines "No slot content" $
                   ("No index @'" ++ show n ++ " in") : xs

-- | Require attribute
reqAttr :: String -> B.Ab a
reqAttr = Left . B.abortLine "Require attribute"

-- | Require attribute
reqAttrName :: String -> B.Ab a
reqAttrName = Left . B.abortLine "Require attribute name, e.g., -xxx"

-- | Require grouping paren
reqGroup :: B.Ab a
reqGroup = Left $ B.abortBecause "Require grouping parens"

-- | Same I/O points
sameIOPoints :: B.IOPoint -> B.Ab a
sameIOPoints = Left . B.abortLine "Same I/O points" . B.ioPointText

-- | Unexpected attribute
unexpAttr :: String -> B.Ab a
unexpAttr = Left . B.abortLine "Unexpected attribute"

unexpAttr0 :: B.Ab a
unexpAttr0 = unexpAttr "Attributes not required"

unexpAttr1 :: B.Ab a
unexpAttr1 = unexpAttr "Require one attribute"

unexpAttr2 :: B.Ab a
unexpAttr2 = unexpAttr "Require two attributes"

unexpAttr3 :: B.Ab a
unexpAttr3 = unexpAttr "Require three attributes"

unexpAttr4 :: B.Ab a
unexpAttr4 = unexpAttr "Require four attributes"

unexpAttr1V :: B.Ab a
unexpAttr1V = unexpAttr "Require attributes"

unexpAttr1Q :: B.Ab a
unexpAttr1Q = unexpAttr "Require one or two attributes"

-- | Unknown clause
unkClause :: [String] -> B.Ab a
unkClause = Left . B.abortLines "Unknown clause"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown nested relation
unkNestRel :: B.Token -> String -> [String] -> B.Ab a
unkNestRel p n rs = Left $ B.abortLines "Unknown nested relation" $ ref : rs
    where ref = "/" ++ n ++ " in " ++ B.tokenContent p

unkNestVar :: String -> [B.Token] -> [((B.Token, B.Local String), B.Head)] -> B.Ab a
unkNestVar n ls ds = Left $ B.abortLines "Unknown nested relation reference"
                           $ ("search" : map indent dynamic)
                          ++ ("for"    : map indent lexical)
    where lexical = map (text $ B.LocalSymbol n) ls
          dynamic = map f ds
          f ((tk, k), _) = text k tk
          text (B.LocalSymbol k) tk = unwords ["nested relation", quote k, "in", tokenAtPoint tk]
          text (B.LocalNest   k) tk = unwords ["nested relation", term  k, "in", tokenAtPoint tk]
          indent    = ("  " ++)
          term      = ('/' :)

tokenAtPoint :: B.Token -> String
tokenAtPoint tok = unwords ws where
    ws    = [B.tokenContent tok, "at L" ++ line, "C" ++ col]
    cp    = B.codePt tok
    line  = show $ B.codePtLineNo   cp
    col   = show $ B.codePtColumnNo cp

quote :: B.Map String
quote s = "'" ++ s ++ "'"

-- | Unknown option
unkOption :: B.ParaUnmatch String -> B.Ab a
unkOption un = Left $ B.abortLines "Unknown option" detail where
    detail = case un of
               B.ParaOutOfRange n p  -> ["Positional parameter out of range",
                                         "Expect " ++ expect p ++
                                         ", but actural " ++ show n]
               B.ParaUnknown  ns     -> ["Unknown parameter name", unwords ns]
               B.ParaMissing  ns     -> ["Missing parameter name", unwords ns]
               B.ParaMultiple ns     -> ["Repeated parameter name", unwords ns]

    expect (B.ParaPosJust n)     = "just "    ++ show n
    expect (B.ParaPosMin  n)     = "minimum " ++ show n
    expect (B.ParaPosMax  n)     = "maximum " ++ show n
    expect (B.ParaPosRange m n)  = "between " ++ show m ++ " and " ++ show n

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = Left $ B.abortLine "Unresolved prefix"
                       $ "Require short definition : short " ++ pre ++ " ..."


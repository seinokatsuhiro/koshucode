{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Message
  ( -- * Base package
    module Koshucode.Baala.Base.Message,
  
    -- * Abortables
    abAssert,
    abAttr,
    abAttrTrees,
    abClause,
    abCoxBuild,
    abCoxCalc,
    abCoxFill,
    abCoxIrrep,
    abCoxPosition,
    abCoxPrefix,
    abCoxReduce,
    abCoxSyntax,
    abLexmap,
    abLiteral,
    abRelmap,
    abRun,
    abSlot,
    abSlotTree,
    abSpecialize,
  
    -- * Core package
    ambInfixes,
    ambRelmap,
    dupAttr,
    dupPrefix,
    dupReplacement,
    emptyLiteral,
    extraAttr,
    httpError,
    invalidPrefix,
    lackArg,
    noFile,
    noSlotName,
    noSlotIndex,
    nothing,
    oddRelation,
    quoteType,
    reqAttr,
    reqAttrName,
    reqFlatName,
    reqGroup,
    reqTermName,
    unexpAttr,
    unexpAttr0,
    unexpAttr1,
    unexpAttr2,
    unexpAttr3,
    unexpAttr4,
    unexpAttr1V,
    unexpAttr1Q,
    unexpAttrT1,
    unexpAttrT2,
    unkBracket,
    unkClause,
    unkCop,
    unkCox,
    unkGlobalVar,
    unkNestRel,
    unkNestVar,
    unkOption,
    unkRefVar,
    unkRelmap,
    unkShow,
    unkTerm,
    unkType,
    unkWord,
    unmatchType,
    unmatchBlank,
    unresPrefix,
  
    -- * Utility
    detailTermRel,
    expectActual,
    expect2Actual,
  ) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Base.Message


-- ----------------------  Abortables

abAssert :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAssert = B.abortable "assert"

abAttr :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abAttr = B.abortable "attr"

abAttrTrees :: B.TTreesTo (B.Map (B.Ab b))
abAttrTrees = B.abortableTrees "attr"

abClause :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abClause = B.abortable "clause"

abCoxBuild :: B.TTreeTo (B.Map (B.Ab b))
abCoxBuild = B.abortableTree "cox-build"

abCoxCalc :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxCalc = B.abortable "cox-calc"

abCoxFill :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxFill = B.abortable "cox-fill"

abCoxIrrep :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxIrrep = B.abortable "cox-irrep"

abCoxPosition :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxPosition = B.abortable "cox-position"

abCoxPrefix :: B.TTreeTo (B.Map (B.Ab b))
abCoxPrefix = B.abortableTree "cox-prefix"

abCoxReduce :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxReduce = B.abortable "cox-reduce"

abCoxSyntax :: B.TTreeTo (B.Map (B.Ab b))
abCoxSyntax = B.abortableTree "cox-syntax"

abLexmap :: B.TTreesTo (B.Map (B.Ab b))
abLexmap = B.abortableTrees "lexmap"

abLiteral :: B.TTreeTo (B.Map (B.Ab b))
abLiteral = B.abortableTree "literal"

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

-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | Ambiguous relmaps
ambRelmap :: String -> [d] -> B.Ab a
ambRelmap name ds = Left $ B.abortLine "Ambiguous relmaps"
                         $ name ++ " (" ++ show (length ds) ++ ")"

-- | Unexpected attribute / Duplicate
dupAttr :: [String] -> B.Ab a
dupAttr ns = unexpAttr $ "Duplicate " ++ unwords ns

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

-- | Lack of argument
lackArg :: String -> B.Ab a
lackArg = Left . B.abortLine "Lack of argument"

-- | File not found
noFile :: String -> B.Ab a
noFile = Left . B.abortLine "File not found"

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

-- | Nothing
nothing :: B.Ab a
nothing = Left $ B.abortBecause "Nothing"

-- | Odd relation literal
oddRelation :: B.Ab a
oddRelation = Left $ B.abortBecause "Odd relation literal"

-- | Quoted type name
quoteType :: String -> B.Ab a
quoteType = Left . B.abortLine "Quoted type name"

-- | Require attribute
reqAttr :: String -> B.Ab a
reqAttr = Left . B.abortLine "Require attribute"

-- | Require attribute
reqAttrName :: String -> B.Ab a
reqAttrName = Left . B.abortLine "Require attribute name, e.g., -xxx"

-- | Require flat name
reqFlatName :: B.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = B.tokenContent tok

-- | Require grouping paren
reqGroup :: B.Ab a
reqGroup = Left $ B.abortBecause "Require grouping parens"

-- | Require term name
reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

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

unexpAttrT1 :: B.Ab a
unexpAttrT1 = unexpAttr "Require terms and one attribute"

unexpAttrT2 :: B.Ab a
unexpAttrT2 = unexpAttr "Require terms and two attributes"

-- | Unknown bracket
unkBracket :: B.Ab a
unkBracket = Left $ B.abortBecause "Unknown bracket"

-- | Unknown clause
unkClause :: [String] -> B.Ab a
unkClause = Left . B.abortLines "Unknown clause"

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = Left . B.abortLine "Unknown expression"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown global variable
unkGlobalVar :: String -> B.Ab a
unkGlobalVar = Left . B.abortLine "Unknown global variable"

-- | Unknown nested relation
unkNestRel :: B.Token -> String -> [String] -> B.Ab a
unkNestRel p n rs = Left $ B.abortLines "Unknown nested relation" $ ref : rs
    where ref = "/" ++ n ++ " in " ++ B.tokenContent p

unkNestVar :: String -> [B.Token] -> [((B.Token, String), B.Head)] -> B.Ab a
unkNestVar n ls ds = Left $ B.abortLines "Unknown nested relation reference"
                           $ ("search" : map indent dynamic)
                          ++ ("for"    : map indent lexical)
    where lexical = map (text n) ls
          dynamic = map f ds
          f ((tk, k), _) = text k tk
          text k tk = unwords ["nested relation", term k, "in", tokenAtPoint tk]
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

-- | Unknown reference for variable
unkRefVar :: (String, Int) -> [String] -> B.Ab a
unkRefVar (v, k) vs = Left $ B.abortLines "Unknown reference for variable"
                [ "look up " ++ var (v, k)
                , "in " ++ args vs ]

-- | Unknown relmap operator
unkRelmap :: String -> B.Ab a
unkRelmap = Left . B.abortLine "Unknown relmap operator"

-- | Unknown object
unkShow :: (Show x) => x -> B.Ab a
unkShow x = Left $ B.abortLines "Unknown object" $ lines $ show x

-- | Unknown term name
unkTerm :: [B.TermName] -> B.Head -> B.Ab a
unkTerm ns he1 =
    Left $ B.abortLines "Unknown term name"
         $ detailTermRel "Unknown" ns he1

-- | Unknown type name
unkType :: String -> B.Ab a
unkType = Left . B.abortLine "Unknown type name"

-- | Unknown word
unkWord :: String -> B.Ab a
unkWord = Left . B.abortLine "Unknown word"

unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Unmatch blank (bug)
unmatchBlank :: String -> Int -> String -> [String] -> B.Ab a
unmatchBlank v k _ vs =
    Left $ B.abortLines "Unmatch blank (bug)"
           [ "look up " ++ var (v, k)
           , "in " ++ args vs ]

var :: (String, Int) -> String
var (v, k) = v ++ "/" ++ show k

args :: [String] -> String
args vs = unwords $ map var $ zip vs [1..]

-- | Unresolved prefix
unresPrefix :: String -> B.Ab a
unresPrefix pre = Left $ B.abortLine "Unresolved prefix"
                       $ "Require short definition : short " ++ pre ++ " ..."

detailTermRel :: String -> [String] -> B.Head -> [String]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indent ns' ++ ["Input relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map B.showTermName ns
    ns1    = B.linesFrom $ B.headExplain he1

expectActual :: String -> String -> [String]
expectActual e a       = [ "Expect " ++ e
                         , "Actual " ++ a ]

expect2Actual :: String -> String -> String -> [String]
expect2Actual e1 e2 a  = [ "Expect " ++ e1
                         , "       " ++ e2
                         , "Actual " ++ a ]


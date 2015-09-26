{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Message
  ( -- * Base package
    module Koshucode.Baala.Base.Message,
  
    -- * Abortables
    abCoxBuild,
    abCoxCalc,
    abCoxFill,
    abCoxIrrep,
    abCoxPosition,
    abCoxPrefix,
    abCoxReduce,
    abCoxSyntax,
    abLiteral,
  
    -- * Data package
    ambInfixes,
    lackArg,
    nothing,
    oddRelation,
    quoteType,
    reqFlatName,
    reqRelTuple,
    reqTermName,
    unkBracket,
    unkCox,
    unkGlobalVar,
    unkRefVar,
    unkShow,
    unkTerm,
    unkType,
    unkWord,
    unmatchBlank,
    unmatchType,
  
    -- * Utility
    detailTermRel,
    expectActual,
    expect2Actual,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data.Token as D
import qualified Koshucode.Baala.Data.Type  as D
import Koshucode.Baala.Base.Message


-- ----------------------  Abortables

abCoxBuild :: D.TTreeTo (B.Map (B.Ab b))
abCoxBuild = D.abortableTree "cox-build"

abCoxCalc :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxCalc = B.abortable "cox-calc"

abCoxFill :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxFill = B.abortable "cox-fill"

abCoxIrrep :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxIrrep = B.abortable "cox-irrep"

abCoxPosition :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxPosition = B.abortable "cox-position"

abCoxPrefix :: D.TTreeTo (B.Map (B.Ab b))
abCoxPrefix = D.abortableTree "cox-prefix"

abCoxReduce :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abCoxReduce = B.abortable "cox-reduce"

abCoxSyntax :: D.TTreeTo (B.Map (B.Ab b))
abCoxSyntax = D.abortableTree "cox-syntax"

abLiteral :: D.TTreeTo (B.Map (B.Ab b))
abLiteral = D.abortableTree "literal"


-- ----------------------  Data package

-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | Lack of argument
lackArg :: String -> B.Ab a
lackArg = Left . B.abortLine "Lack of argument"

-- | Nothing
nothing :: B.Ab a
nothing = Left $ B.abortBecause "Nothing"

-- | Odd relation literal
oddRelation :: Int -> Int -> B.Ab a
oddRelation e a  = Left $ B.abortLines "Odd relation literal"
                        $ expectActual (len e) (len a)
    where len n = show n ++ " contents"

-- | Require tuple in list
reqRelTuple :: B.Ab a
reqRelTuple = Left $ B.abortBecause "Require tuple in list"

-- | Quoted type name
quoteType :: String -> B.Ab a
quoteType = Left . B.abortLine "Quoted type name"

-- | Require flat name
reqFlatName :: D.Token -> B.Ab a
reqFlatName tok = Left $ B.abortLine "Require flat name" n where
    n = D.tokenContent tok

-- | Require term name
reqTermName :: B.Ab a
reqTermName = Left $ B.abortBecause "Require term name"

-- | Unknown bracket
unkBracket :: B.Ab a
unkBracket = Left $ B.abortBecause "Unknown bracket"

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = Left . B.abortLine "Unknown expression"

-- | Unknown global variable
unkGlobalVar :: String -> B.Ab a
unkGlobalVar = Left . B.abortLine "Unknown global variable"

-- | Unmatch blank (bug)
unmatchBlank :: String -> Int -> String -> [String] -> B.Ab a
unmatchBlank v k _ vs =
    Left $ B.abortLines "Unmatch blank (bug)"
           [ "look up " ++ var (v, k)
           , "in " ++ args vs ]

-- | Unknown reference for variable
unkRefVar :: (String, Int) -> [String] -> B.Ab a
unkRefVar (v, k) vs = Left $ B.abortLines "Unknown reference for variable"
                [ "look up " ++ var (v, k)
                , "in " ++ args vs ]

-- | Unknown object
unkShow :: (Show x) => x -> B.Ab a
unkShow x = Left $ B.abortLines "Unknown object" $ lines $ show x

-- | Unknown term name
unkTerm :: [D.TermName] -> D.Head -> B.Ab a
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


-- ----------------------  Utility

var :: (String, Int) -> String
var (v, k) = v ++ "/" ++ show k

args :: [String] -> String
args vs = unwords $ map var $ zip vs [1..]

expectActual :: String -> String -> [String]
expectActual e a       = [ "Expect " ++ e
                         , "Actual " ++ a ]

expect2Actual :: String -> String -> String -> [String]
expect2Actual e1 e2 a  = [ "Expect " ++ e1
                         , "       " ++ e2
                         , "Actual " ++ a ]

detailTermRel :: String -> [String] -> D.Head -> [String]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indent ns' ++ ["Input relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map D.showTermName ns
    ns1    = B.linesFrom $ D.headExplain he1


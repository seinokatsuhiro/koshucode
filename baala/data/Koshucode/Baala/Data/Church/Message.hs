{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Church.Message
  ( -- * Abortable
    abCoxBuild,
    abCoxCalc,
    abCoxFill,
    abCoxIrrep,
    abCoxPosition,
    abCoxPrefix,
    abCoxReduce,
    abCoxSyntax,

    -- * Message
    ambInfixes,
    lackArg,
    unkCop,
    unkCox,
    unkGlobalVar,
    unkRefVar,
    unkShow,
    unkTerm,
    unmatchBlank,

    -- * Utility
    detailTermRel,
    msgTerms2,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Type            as D


-- --------------------------------------------  Abortable scope

-- | Abortable scope for cox building.
abCoxBuild :: (B.GetCodePos cp) => B.Abortable cp b
abCoxBuild = B.abortable "cox-build"

-- | Abortable scope for cox calculation.
abCoxCalc :: (B.GetCodePos cp) => B.Abortable cp b
abCoxCalc = B.abortable "cox-calc"

-- | Abortable scope for cox filling.
abCoxFill :: (B.GetCodePos cp) => B.Abortable cp b
abCoxFill = B.abortable "cox-fill"

-- | Abortable scope for irreducible expression.
abCoxIrrep :: (B.GetCodePos cp) => B.Abortable cp b
abCoxIrrep = B.abortable "cox-irrep"

-- | Abortable scope for cox position.
abCoxPosition :: (B.GetCodePos cp) => B.Abortable cp b
abCoxPosition = B.abortable "cox-position"

-- | Abortable scope for cox prefix.
abCoxPrefix :: (B.GetCodePos cp) => B.Abortable cp b
abCoxPrefix = B.abortable "cox-prefix"

-- | Abortable scope for cox reduction.
abCoxReduce :: (B.GetCodePos cp) => B.Abortable cp b
abCoxReduce = B.abortable "cox-reduce"

-- | Abortable scope for cox syntax.
abCoxSyntax :: (B.GetCodePos cp) => B.Abortable cp b
abCoxSyntax = B.abortable "cox-syntax"


-- --------------------------------------------  Message

-- | Ambiguous infix operators
ambInfixes :: [String] -> B.Ab a
ambInfixes = Left . B.abortLines "Ambiguous infix operators"

-- | Lack of argument
lackArg :: String -> B.Ab a
lackArg = Left . B.abortLine "Lack of argument"

-- | Unknown content operator
unkCop :: String -> B.Ab a
unkCop = Left . B.abortLine "Unknown content operator"

-- | Unknown expression
unkCox :: String -> B.Ab a
unkCox = Left . B.abortLine "Unknown expression"

-- | Unknown global variable
unkGlobalVar :: String -> B.Ab a
unkGlobalVar = Left . B.abortLine "Unknown global variable"

-- | Unknown reference for variable
unkRefVar :: (String, Int) -> [String] -> B.Ab a
unkRefVar (v, k) vs = Left $ B.abortLines "Unknown reference for variable"
                [ "look up " ++ var (v, k)
                , "in " ++ args vs ]

-- | Unknown object
unkShow :: (Show x) => x -> B.Ab a
unkShow x = Left $ B.abortLines "Unknown object" $ lines $ show x

-- | Unknown term name
unkTerm :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> B.Ab a
unkTerm t1 t2 =
    Left $ B.abortLines "Unknown term name"
         $ msgTerms2 "Unknown" t1 "in the terms" t2

-- | Unmatch blank (bug)
unmatchBlank :: String -> Int -> String -> [String] -> B.Ab a
unmatchBlank v k _ vs =
    Left $ B.abortLines "Unmatch blank (bug)"
           [ "look up " ++ var (v, k)
           , "in " ++ args vs ]


-- ----------------------  Utility

var :: (String, Int) -> String
var (v, k) = v ++ "/" ++ show k

args :: [String] -> String
args vs = unwords $ map var $ zip vs [1..]

-- | Terms and input relation heading.
detailTermRel :: String -> [S.TermName] -> D.Head -> [String]
detailTermRel label ns he1 = detail where
    detail = [label] ++ indentLines ns' ++ ["Input relation"] ++ indentLines ns1
    ns'    = map S.termNameString ns
    ns1    = linesFrom $ D.headExplain he1

linesFrom :: (Show a) => a -> [String]
linesFrom = lines . show

-- | Create message lines from double term names.
msgTerms2 :: (D.GetTermNames t1, D.GetTermNames t2) => String -> t1 -> String -> t2 -> [String]
msgTerms2 s1 t1 s2 t2 = detail where
    detail = [s1] ++ msg1 t1 ++ [s2] ++ msg2 t2
    msg1 = indentLines . B.sort . termNameStrings
    msg2 = indentLines . B.sort . termNameStrings

-- | Indent message lines.
indentLines :: [String] -> [String]
indentLines = map ("  " ++)

-- | Lines of term names.
termNameStrings :: (D.GetTermNames t) => t -> [String]
termNameStrings = msg . D.getTermNames where
    msg [] = ["(no terms)"]
    msg ns = map S.termNameString ns


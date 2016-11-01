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
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Type            as D
import qualified Koshucode.Baala.Syntax.Message       as Msg


-- --------------------------------------------  Abortable scope

-- | Abortable scope for cox building.
abCoxBuild :: S.TTreeTo (B.MapAb b)
abCoxBuild = Msg.abortableTree "cox-build"

-- | Abortable scope for cox calculation.
abCoxCalc :: (B.CodePtr cp) => [cp] -> B.MapAb b
abCoxCalc = B.abortable "cox-calc"

-- | Abortable scope for cox filling.
abCoxFill :: (B.CodePtr cp) => [cp] -> B.MapAb b
abCoxFill = B.abortable "cox-fill"

-- | Abortable scope for irreducible expression.
abCoxIrrep :: (B.CodePtr cp) => [cp] -> B.MapAb b
abCoxIrrep = B.abortable "cox-irrep"

-- | Abortable scope for cox position.
abCoxPosition :: (B.CodePtr cp) => [cp] -> B.MapAb b
abCoxPosition = B.abortable "cox-position"

-- | Abortable scope for cox prefix.
abCoxPrefix :: S.TTreeTo (B.MapAb b)
abCoxPrefix = Msg.abortableTree "cox-prefix"

-- | Abortable scope for cox reduction.
abCoxReduce :: (B.CodePtr cp) => [cp] -> B.MapAb b
abCoxReduce = B.abortable "cox-reduce"

-- | Abortable scope for cox syntax.
abCoxSyntax :: S.TTreeTo (B.MapAb b)
abCoxSyntax = Msg.abortableTree "cox-syntax"


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
unkTerm :: [S.TermName] -> D.Head -> B.Ab a
unkTerm ns he1 =
    Left $ B.abortLines "Unknown term name"
         $ detailTermRel "Unknown" ns he1

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
    detail = [label] ++ indent ns' ++ ["Input relation"] ++ indent ns1
    indent = map ("  " ++)
    ns'    = map S.showTermName ns
    ns1    = B.linesFrom $ D.headExplain he1


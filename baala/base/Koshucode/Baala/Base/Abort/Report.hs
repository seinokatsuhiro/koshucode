{-# OPTIONS_GHC -Wall #-}

-- | Reporting abort reasons.

module Koshucode.Baala.Base.Abort.Report
( -- * Type
  AbortReason (..),
  Ab, AbMap,

  -- * Construct
  abortBecause,
  abortLine,
  abortLines,
  abortTokens,

  -- * Report
  CommandLine,
  abort,
  bug,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B
import qualified Koshucode.Baala.Base.Token   as B



-- ----------------------  Type

data AbortReason = AbortReason
    { abortReason :: String
    , abortDetail :: [String]
    , abortSource :: [(String, B.Token)]
    } deriving (Show, Eq, Ord)

-- | Abortable result, i.e., either of right result or abort reason.
type Ab b = Either AbortReason b

-- | Abortable mapping.
type AbMap b = b -> Ab b



-- ----------------------  Contruct

abortBecause :: String -> AbortReason
abortBecause reason =
    AbortReason
    { abortReason = reason
    , abortDetail = []
    , abortSource = []
    }

abortLine :: String -> String -> AbortReason
abortLine reason detail =
    (abortBecause reason) { abortDetail = [detail] }

abortLines :: String -> [String] -> AbortReason
abortLines reason details =
    (abortBecause reason) { abortDetail = details }

abortTokens :: String -> [B.Token] -> AbortReason
abortTokens reason tokens =
    (abortBecause reason) { abortDetail = map B.tokenContent tokens }



-- ----------------------  Report

-- | Command name and its arguments.
type CommandLine = [String]

-- | Stop program execution abnormally.
abort :: CommandLine -> AbortReason -> IO c
abort cmd a =
  do B.putCommentLines $ abortMessage cmd a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

abortMessage :: CommandLine -> AbortReason -> [String]
abortMessage cmd a = map B.trimRight texts where
    texts  = sandwich "" "" $ B.renderTable " " tab
    tab    = B.alignTable $ title : rule : rows
    title  = [ B.textCell B.Front "ABORTED "
             , B.textCell B.Front $ abortReason a ]
    rule   = [r, r, r]
    r      = B.textRuleCell '-'
    p text = (text, "")
    rows   = concatMap row
             [ ("Detail"  , map p $ abortDetail a)
             , ("Source"  , source $ abortSource a)
             , ("Command" , map p $ cmd)
             ]

    row :: (String, [(String, String)]) -> [[B.Cell]]
    row (_, []) = []
    row (name, xs)
        = let (codes, tags) = unzip xs
          in [[ B.textCell B.Front name
              , B.textBlockCell B.Front codes
              , B.textBlockCell B.Front $ map dots tags ]]

    dots :: B.Map String
    dots ""   = ""
    dots text = ".. " ++ text

    sandwich :: a -> a -> B.Map [a]
    sandwich open close xs = open : xs ++ [close]

source :: [(String, B.Token)] -> [(String, String)]
source = concatMap f . reverse where
    f :: (String, B.Token) -> [(String, String)]
    f (tag, token) = B.tokenPosDisplay tag $ B.tokenPos token

-- | Stop on error @'BUG DISCOVERED'@
bug :: String -> a
bug msg = error $ "BUG DISCOVERED: " ++ msg


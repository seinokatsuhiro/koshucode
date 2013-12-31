{-# OPTIONS_GHC -Wall #-}

{-| Abort utilities -}

module Koshucode.Baala.Base.Abort.Utility
( AbortReasonClass (..),
  CommandLine,
  abortMap,
  bug,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class (Show a) => AbortReasonClass a where
    abortClass   :: a -> String

    abortSymbol  :: a -> String
    abortSymbol  = head . words . show

    abortReason  :: a -> String
    abortReason _ = ""

    abortDetail  :: a -> [String]
    abortDetail _ = []

    abortSource  :: a -> [String]
    abortSource _ = []



-- ----------------------  Function

type CommandLine = [String]

abortMap
    :: (AbortReasonClass a)
    => CommandLine    -- ^ Command line
    -> (b -> IO c)    -- ^ Function
    -> Either a b     -- ^ Argument
    -> IO c           -- ^ Value
abortMap = either . abort

{-| Stop program execution abnormally. -}
abort :: (AbortReasonClass a) => CommandLine -> a -> IO c
abort cmd a =
  do B.putCommentLines $ messageLines cmd a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

messageLines :: (AbortReasonClass a) => CommandLine -> a -> [String]
messageLines cmd a = sandwich "" "" xs where
    xs    = B.renderTable " " $ B.alignTable $ title : rule : rows
    title = [ B.textCell B.Front "ABORTED    "
            , B.textCell B.Front $ bracket $ abortSymbol a ]
    rule  = [ B.textRuleCell '-'
            , B.textRuleCell '-' ]
    rows  = concatMap row
            [ ("Class"   , [abortClass a])
            , ("Reason"  , [abortReason a])
            , ("Detail"  , abortDetail a)
            , ("Source"  , abortSource a)
            , ("Command" , cmd) ]

    row (_, []) = []
    row (name, content)
        = [[ B.textCell B.Front name
           , B.textBlockCell B.Front content ]]

sandwich :: a -> a -> B.Map [a]
sandwich open close xs = open : xs ++ [close]

bracket :: B.Map String
bracket = sandwich '[' ']'

{-| Stop on error @'bug in koshucode'@ -}
bug :: a
bug = error "bug in koshucode"


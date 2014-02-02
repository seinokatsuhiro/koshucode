{-# OPTIONS_GHC -Wall #-}

{-| Class for abort reasons -}

module Koshucode.Baala.Base.Abort.Class
( AbortReasonClass (..),
  CommandLine,
  abortableIO,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B



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

{-| Command name and its arguments. -}
type CommandLine = [String]

{-| Abortable process. -}
abortableIO
    :: (AbortReasonClass a)
    => CommandLine  -- ^ Command line
    -> (b -> IO c)  -- ^ I/O function
    -> Either a b   -- ^ Argument of the function
    -> IO c         -- ^ Result of the function
abortableIO = either . abort

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
            , B.textCell B.Front $ abortReason a ]
    rule  = [ B.textRuleCell '-'
            , B.textRuleCell '-' ]
    rows  = concatMap row
            [ ("Detail"  , abortDetail a)
            , ("Source"  , abortSource a)
            , ("Command" , cmd)
            , ("Symbol"  , [bracket $ abortSymbol a])
            ]

    row (_, []) = []
    row (name, content)
        = [[ B.textCell B.Front name
           , B.textBlockCell B.Front content ]]

sandwich :: a -> a -> B.Map [a]
sandwich open close xs = open : xs ++ [close]

bracket :: B.Map String
bracket = sandwich '[' ']'


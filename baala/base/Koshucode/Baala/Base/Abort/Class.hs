{-# OPTIONS_GHC -Wall #-}

-- | Class for abort reasons

module Koshucode.Baala.Base.Abort.Class
( AbortReasonClass (..),
  CommandLine,
  abortableIO,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B



-- | Class that represents abort reason.
class (Show a) => AbortReasonClass a where
    abortClass   :: a -> String

    abortSymbol  :: a -> String
    abortSymbol  = head . words . show

    abortReason  :: a -> String
    abortReason _ = ""

    abortDetail  :: a -> [String]
    abortDetail _ = []

    abortSource  :: a -> [(String, String)]
    abortSource _ = []

-- | Command name and its arguments.
type CommandLine = [String]

-- | Abortable process.
abortableIO
    :: (AbortReasonClass a)
    => CommandLine  -- ^ Command line
    -> (b -> IO c)  -- ^ I/O function
    -> Either a b   -- ^ Argument of the function
    -> IO c         -- ^ Result of the function
abortableIO = either . abort

-- | Stop program execution abnormally.
abort :: (AbortReasonClass a) => CommandLine -> a -> IO c
abort cmd a =
  do B.putCommentLines $ messageLines cmd a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

messageLines :: (AbortReasonClass a) => CommandLine -> a -> [String]
messageLines cmd a = map B.trimRight texts where
    texts  = sandwich "" "" $ B.renderTable " " tab
    tab    = B.alignTable $ title : rule : rows
    title  = [ B.textCell B.Front "ABORTED "
             , B.textCell B.Front $ abortReason a ]
    rule   = [r, r, r]
    r      = B.textRuleCell '-'
    p text = (text, "")
    rows   = concatMap row
             [ ("Detail"  , map p $ abortDetail a)
             , ("Source"  ,         abortSource a)
             , ("Command" , map p $ cmd)
             , ("Symbol"  , map p $ [abortSymbol a])
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



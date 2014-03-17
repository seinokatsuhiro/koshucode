{-# OPTIONS_GHC -Wall #-}

-- | Class for abort reasons

module Koshucode.Baala.Base.Abort.Class
( AbortReason (..),
  abortSymbolGet,
  AbortBy (..),
  CommandLine,
  abortableIO,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B



class AbortBy a where
    abortBy :: a -> AbortReason

data AbortReason = AbortReason
    { abortSymbol :: String
    , abortReason :: String
    , abortDetail :: [String]
    , abortSource :: [(String, B.Token)]
    } deriving (Show, Eq, Ord)

abortSymbolGet :: (Show a) => a -> String
abortSymbolGet = head . words . show

source :: [(String, B.Token)] -> [(String, String)]
source = concatMap f . reverse where
    f :: (String, B.Token) -> [(String, String)]
    f (tag, token) = B.tokenPosDisplay tag $ B.tokenPos token

-- | Command name and its arguments.
type CommandLine = [String]

-- | Abortable process.
abortableIO
    :: CommandLine  -- ^ Command line
    -> (b -> IO c)  -- ^ I/O function
    -> Either AbortReason b   -- ^ Argument of the function
    -> IO c         -- ^ Result of the function
abortableIO = either . abort

-- | Stop program execution abnormally.
abort :: CommandLine -> AbortReason -> IO c
abort cmd a =
  do B.putCommentLines $ messageLines cmd a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

messageLines :: CommandLine -> AbortReason -> [String]
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
             , ("Source"  , source $ abortSource a)
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



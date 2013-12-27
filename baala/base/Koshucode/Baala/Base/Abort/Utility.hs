{-# OPTIONS_GHC -Wall #-}

{-| Abort utility -}

module Koshucode.Baala.Base.Abort.Utility
( -- * Class and datatype
  AbortReasonClass (..),

  -- * Function
  abort,
  abortMap,
  addAbort,
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

abortMap
    :: (AbortReasonClass a)
    => (b -> IO c)    -- ^ Function
    -> Either a b     -- ^ Argument
    -> IO c           -- ^ Value
abortMap _ (Left  a) = abort a
abortMap f (Right b) = f b

{-| Stop program execution abnormally. -}
abort :: (AbortReasonClass a) => a -> IO c
abort a =
  do B.putCommentLines $ messageLines a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

sandwich :: a -> a -> B.Map [a]
sandwich open close xs = open : xs ++ [close]

bracket :: B.Map String
bracket = sandwich '[' ']'

messageLines :: (AbortReasonClass a) => a -> [String]
messageLines a = sandwich "" "" xs where
    xs    = B.renderTable " " $ B.alignTable $ title : [rule, rule] : rows
    title = [ B.textCell B.Front "ABORTED    ", B.textCell B.Front $ bracket $ abortSymbol a ]
    rule  = B.textRuleCell '-'
    rows  = concatMap row $ messageAssoc a
    row (_, []) = []
    row (name, content)
        = [[ B.textCell B.Front name
           , B.textBlockCell B.Front content ]]

messageAssoc :: (AbortReasonClass a) => a -> [(String, [String])]
messageAssoc a =
    [ ("Class"    , [abortClass a])
    , ("Reason"   , [abortReason a])
    , ("Detail"   , abortDetail a)
    , ("Source"   , abortSource a)
    , ("Command"  , []) ]

addAbort :: (AbortReasonClass a) => a -> B.Map (Either a b)
addAbort _ (Left a) = Left a
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


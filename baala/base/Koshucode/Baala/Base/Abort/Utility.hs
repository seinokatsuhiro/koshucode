{-# OPTIONS_GHC -Wall #-}

{-| Abort utility -}

module Koshucode.Baala.Base.Abort.Utility
( -- * Class and datatype
  AbortReasonClass (..),
  AbortType,
  AbortOrType,

  -- * Function
  abort,
  abortMap,
  addAbort,
  bug,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Syntax  as B
import qualified Koshucode.Baala.Base.Token   as B



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class (Show a) => AbortReasonClass a where
    abortClass   :: a -> String

    abortSymbol  :: a -> String
    abortSymbol  = head . words . show

    abortReason  :: a -> String

    abortDetail  :: a -> [String]
    abortDetail _ = []

    abortExpr    :: a -> [String]
    abortExpr _   = []

    abortRelmap  :: a -> [String]
    abortRelmap _ = []

    abortClause  :: a -> [String]
    abortClause _ = []

{-| Abort reason and source code information. -}
type AbortType a = (a, [B.TokenLine])

{-| Either of (1) right result or (2) abort information. -}
type AbortOrType a b = Either (AbortType a) b



-- ----------------------  Function

abortMap
    :: (AbortReasonClass a)
    => (b -> IO c)       -- ^ Function
    -> AbortOrType a b   -- ^ Argument
    -> IO c              -- ^ Value
abortMap _ (Left  a) = abort a
abortMap f (Right b) = f b

{-| Stop program execution abnormally. -}
abort :: (AbortReasonClass a) => AbortType a -> IO c
abort a =
  do B.putCommentLines $ messageLines a
     B.putCommentLines ["Exit with status 2", ""]
     Sys.exitWith $ Sys.ExitFailure 2

sandwich :: a -> [a] -> [a]
sandwich x xs = x : xs ++ [x]

messageLines :: (AbortReasonClass a) => AbortType a -> [String]
messageLines a = sandwich "" xs where
    xs    = B.renderTable " " $ B.alignTable $ [title] : [rule, rule] : rows
    title = B.textCell B.Front "ABORTED  "
    rule  = B.textRuleCell '-'
    rows  = concatMap row $ messageAssoc a
    row (_, []) = []
    row (name, content)
        = [[ B.textCell B.Front name
           , B.textBlockCell B.Front content ]]

messageAssoc :: (AbortReasonClass a) => AbortType a -> [(String, [String])]
messageAssoc (a, ls) =
    [ ("Class"  , [abortClass a])
    , ("Reason" , [abortSymbol a, abortReason a])
    , ("Detail" , abortDetail a)
    , ("Expr"   , abortExpr a)
    , ("Relmap" , abortRelmap a)
    , ("Clause" , abortClause a)
    , ("Source" , map source ls) ]
    where
      source (B.CodeLine n line _) = show n ++ " " ++ line

addAbort :: (AbortReasonClass a) => AbortType a -> B.Map (AbortOrType a b)
addAbort _ (Left a) = Left a
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


{-# OPTIONS_GHC -Wall #-}

{-| Abort utility -}

module Koshucode.Baala.Base.Abort.Utility
( -- * Class and datatype
  AbortReasonClass (..),
  AbortType,
  AbortOrType,

  -- * Function
  abort,
  abortIO,
  addAbort,
  bug,
) where

import qualified System.Exit as Sys
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Syntax  as B
import qualified Koshucode.Baala.Base.Token   as B



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class AbortReasonClass a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> [String]
    abortSub     :: a -> [String]
    abortSub _   = []

{-| Abort reason and source code information. -}
type AbortType a = (a, [B.Token], [B.CodeLine B.Token])

{-| Either of (1) right result or (2) abort information. -}
type AbortOrType a b = Either (AbortType a) b



-- ----------------------  Function

{-| Stop program execution abnormally. -}
abort :: (AbortReasonClass a) => AbortType a -> IO c
abort a =
  do putStr $ message a
     putStrLn "**  (ステータス 2 で終了します)"
     Sys.exitWith $ Sys.ExitFailure 2

abortIO
    :: (AbortReasonClass a)
    => (b -> IO c)          -- ^ Function
    -> AbortOrType a b      -- ^ Argument
    -> IO c
abortIO _ (Left a)       = abort a
abortIO f (Right output) = f output

message :: (AbortReasonClass a) => AbortType a -> String
message = vline . messageLines where
    vline = unlines . map ("**  " ++)

messageLines :: (AbortReasonClass a) => AbortType a -> [String]
messageLines a = ["", "処理を中断しました", ""] ++ xs ++ [""] where
    xs   = B.renderTable " " $ B.alignTable $ [rule, rule] : rows
    rule = B.textRuleCell '-'
    rows = concatMap row $ messageAssoc a
    row (_, []) = []
    row (name, content)
        = [[ B.textCell B.Front name
           , B.textBlockCell B.Front content ]]

messageAssoc :: (AbortReasonClass a) => AbortType a -> [(String, [String])]
messageAssoc (a, _, cline) =
    [ ("種類", [abortSymbol a])
    , ("概要", [abortTitle a])
    , ("おもな情報", abortMain a)
    , ("補助情報",   abortSub a)
    , ("ソース", source cline) ]
    where
      source :: [B.CodeLine B.Token] -> [String]
      source [] = []
      source ls = map d ls where
          d (B.CodeLine n line _) =
              "(" ++ show n ++ ") " ++ line

addAbort :: (AbortReasonClass a) => AbortType a -> B.Map (AbortOrType a b)
addAbort _ (Left a1) = Left a1
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


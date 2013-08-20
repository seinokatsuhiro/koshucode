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

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax
import qualified Text.PrettyPrint as D



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class AbortReasonClass a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> Doc
    abortSub     :: a -> Doc
    abortSub _   = docEmpty

{-| Abort reason and source code information. -}
type AbortType a = (a, [Token], [CodeLine Token])

{-| Either of (1) right result or (2) abort information. -}
type AbortOrType a b = Either (AbortType a) b



-- ----------------------  Function

{-| Stop program execution abnormally. -}
abort :: (AbortReasonClass a) => AbortType a -> IO ()
abort a =
  do putMessage a
     putStrLn "**  (ステータス 1 で終了します)"
     Sys.exitWith $ Sys.ExitFailure 1

abortIO
    :: (AbortReasonClass a)
    => (b -> IO ())               -- ^ Function
    -> AbortOrType a b            -- ^ Argument
    -> IO ()
abortIO _ (Left a)       = abort a
abortIO f (Right output) = f output

putMessage :: (AbortReasonClass a) => AbortType a -> IO ()
putMessage = putStr . vline . D.renderStyle sty . messageDoc where
    sty      = D.style { D.lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: (AbortReasonClass a) => AbortType a -> Doc
messageDoc (a, toks, cline) =
    docv [ doc ""
         , doc "処理を中断しました"
         , doc ""
         , doc (label "種類") <> doc (abortSymbol a)
         , doc (label "概要") <> doc (abortTitle a)
         , opt (label "おもな情報") $ abortMain a
         , opt (label "補助情報")  $ abortSub a
         , doc ""
         , source cline
         , doc ""
         , token toks
         , doc ""
         , doc ""
         ]

    where
      opt :: String -> Map Doc
      opt lbl x | D.isEmpty x = docEmpty
                | otherwise = doc lbl <> x

      source :: [CodeLine Token] -> Doc
      source [] = doc "?" <> doc "???"
      source ls = docv $ map d $ ls where
          d (CodeLine n line _) =
              (doc $ label $ show n) <> doc line

      token :: [Token] -> Doc
      token = docv . map d where
          d tok = (doc $ label $ "  " ++ show (tokenNumber tok))
                  <> doc (tokenContent tok)

      label :: Map String
      label = padRight 12

addAbort :: (AbortReasonClass a) => AbortType a -> Map (AbortOrType a b)
addAbort _ (Left a1) = Left a1
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


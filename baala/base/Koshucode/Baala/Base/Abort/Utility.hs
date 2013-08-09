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

import Koshucode.Baala.Base.Prelude as Doc
import Koshucode.Baala.Base.Syntax



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class AbortReasonClass a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> Doc
    abortSub     :: a -> Doc
    abortSub _   = Doc.empty

{-| Abort reason and source code information. -}
type AbortType a = (a, [CodeLine], [Token])

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
putMessage = putStr . vline . renderStyle sty . messageDoc where
    sty      = style { lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: (AbortReasonClass a) => AbortType a -> Doc
messageDoc (a, cline, toks) =
    docv [ text ""
         , text "処理を中断しました"
         , text ""
         , text (label "種類") <> text (abortSymbol a)
         , text (label "概要") <> text (abortTitle a)
         , opt  (label "おもな情報") $ abortMain a
         , opt  (label "補助情報") $ abortSub a
         , text ""
         , source cline
         , text ""
         , token toks
         , text ""
         , text ""
         ]

    where
      opt :: String -> Map Doc
      opt lbl x | isEmpty x = empty
                | otherwise = text lbl <> x

      source :: [CodeLine] -> Doc
      source [] = text "?" <> text "???"
      source ls = docv $ map d $ ls where
          d (CodeLine n line _) =
              (text $ label $ show n) <> text line

      token :: [Token] -> Doc
      token = docv . map d where
          d tok = (text $ label $ "  " ++ show (tokenNumber tok))
                  <> text (tokenContent tok)

      label :: Map String
      label = rpad 12

addAbort :: (AbortReasonClass a) => AbortType a -> Map (AbortOrType a b)
addAbort _ (Left a1) = Left a1
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


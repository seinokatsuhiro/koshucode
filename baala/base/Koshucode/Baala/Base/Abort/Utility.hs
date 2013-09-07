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
import qualified Text.PrettyPrint as D



-- ---------------------- Class and datatype

{-| Class that represents abort reason. -}
class AbortReasonClass a where
    abortSymbol  :: a -> String
    abortTitle   :: a -> String
    abortMain    :: a -> B.Doc
    abortSub     :: a -> B.Doc
    abortSub _   = B.docEmpty

{-| Abort reason and source code information. -}
type AbortType a = (a, [B.Token], [B.CodeLine B.Token])

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

messageDoc :: (AbortReasonClass a) => AbortType a -> B.Doc
messageDoc (a, toks, cline) =
    B.docv [ B.doc ""
           , B.doc "処理を中断しました"
           , B.doc ""
           , B.doc (label "種類") B.<> B.doc (abortSymbol a)
           , B.doc (label "概要") B.<> B.doc (abortTitle a)
           , opt (label "おもな情報") $ abortMain a
           , opt (label "補助情報")  $ abortSub a
           , B.doc ""
           , source cline
           , B.doc ""
           , token toks
           , B.doc ""
           , B.doc ""
           ]

    where
      opt :: String -> B.Map B.Doc
      opt lbl x | D.isEmpty x = B.docEmpty
                | otherwise   = B.doc lbl B.<> x

      source :: [B.CodeLine B.Token] -> B.Doc
      source [] = B.doc "?" B.<> B.doc "???"
      source ls = B.docv $ map d $ ls where
          d (B.CodeLine n line _) =
              (B.doc $ label $ show n) B.<> B.doc line

      token :: [B.Token] -> B.Doc
      token = B.docv . map d where
          d tok = (B.doc $ label $ "  " ++ show (B.tokenNumber tok))
                  B.<> B.doc (B.tokenContent tok)

      label :: B.Map String
      label = B.padRight 12

addAbort :: (AbortReasonClass a) => AbortType a -> B.Map (AbortOrType a b)
addAbort _ (Left a1) = Left a1
addAbort _ x = x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


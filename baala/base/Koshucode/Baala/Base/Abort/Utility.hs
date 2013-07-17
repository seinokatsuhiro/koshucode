{-# OPTIONS_GHC -Wall #-}

{-| Abort utility -}

module Koshucode.Baala.Base.Abort.Utility
( bug,
  abort,
  addAbort,
  abortIO,
  AbortP,
  AbortOrP,
) where

import qualified System.Exit as Sys
import qualified Data.Char   as Char

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Abort.Source



{-| Stop program execution abnormally. -}
abort :: (AbortSymbol a) => AbortP a -> IO ()
abort (a, src) = do
  putMessage (a, src)
  putStrLn "**  (ステータス 1 で終了します)"
  Sys.exitWith $ Sys.ExitFailure 1

type AbortP   a   = (a, [SourceLine])
type AbortOrP a b = Either (AbortP a) b

addAbort :: (AbortSymbol a) => AbortP a -> Map (AbortOrP a b)
addAbort a2 (Left _) = Left a2
addAbort _ x = x

abortIO :: (AbortSymbol a) => (b -> IO ()) -> AbortOrP a b -> IO ()
abortIO _ (Left (a, src))  = abort (a, src)
abortIO f (Right output) = f output

putMessage :: (AbortSymbol a) => AbortP a -> IO ()
putMessage = putStr . vline . renderStyle sty . messageDoc where
    sty      = style { lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: (AbortSymbol a) => AbortP a -> Doc
messageDoc (a, src) =
    docv [ text ""
         , text "処理を中断しました"
         , text ""
         , text (label "種類") <> text (abortSymbol a)
         , text (label "概要") <> text (abortTitle a)
         , opt  (label "おもな情報") $ abortMain a
         , opt  (label "補助情報") $ abortSub a
         , text ""
         , source src
         , text ""
         , text ""
         ]

source :: [SourceLine] -> Doc
source = docv . map d where
    d (SourceLine n line _) = (text $ label $ show n) <> text line

label :: String -> String
label = rpad 12

rpad :: Int -> String -> String
rpad n s = s ++ replicate rest ' ' where
    rest = max 0 (n - len)
    len  = sum $ map (size . Char.ord) s where
    size c | c > 255   = 2
           | otherwise = 1

opt :: String -> Doc -> Doc
opt lbl x | isEmpty x = empty
            | otherwise = text lbl <> x

{-| Stop on error ''bug in koshucode'' -}
bug :: a
bug = error "bug in koshucode"


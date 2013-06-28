{-# OPTIONS_GHC -Wall #-}

{-| Abort utility -}

module Koshucode.Baala.Base.Abort.Utility
( (<!!>),
  bug,
  abort,
  addAbort,
  abortIO
) where

import qualified System.Exit as Sys
import qualified Data.Char   as Char
import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Abort.Source
import Koshucode.Baala.Base.Abort.Symbol

{-| Stop program execution abnormally. -}
abort :: (AbortSymbol a) => a -> IO ()
abort a = do
  putMessage a
  putStrLn "(ステータス 1 で終了します)"
  Sys.exitWith $ Sys.ExitFailure 1

addAbort :: (AbortSymbol a) => a -> Either a b -> Either a b
addAbort a2 (Left _) = Left a2
addAbort _ x = x

abortIO :: (AbortSymbol a) => (b -> IO ()) -> Either a b -> IO ()
abortIO _ (Left a)  = abort a
abortIO f (Right output) = f output

putMessage :: (AbortSymbol a) => a -> IO ()
putMessage = putStr . vline . renderStyle sty . messageDoc where
    sty      = style { lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: (AbortSymbol a) => a -> Doc
messageDoc a =
    docv [ text ""
         , text "処理を中断しました"
         , text ""
         , text (label "種類") <> text (abortSymbol a)
         , text (label "概要") <> text (abortTitle a)
         , opt  (label "おもな情報") $ abortMain a
         , opt  (label "補助情報") $ abortSub a
         , text ""
         , src $ abortLines a
         , text ""
         , text ""
         ]

src :: [SourceLine] -> Doc
src = docv . map d where
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

{-| Lookup association list.
    This function may abort on AbortLookup. -}

(<!!>) :: [(String, a)] -> String -> AbortOr a
(<!!>) assoc key = loop assoc where
    loop [] = Left $ AbortLookup [] key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs


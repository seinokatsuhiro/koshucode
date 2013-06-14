{-# OPTIONS_GHC -Wall #-}

-- | Abort utility

module Koshucode.Baala.Base.Prelude.Abort.Utility
( (<!!>),
  bug,
  abort,
  addAbort,
  abortIO
) where
import qualified System.Exit as Sys
import qualified Data.Char   as Char
import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Prelude.Utility
import Koshucode.Baala.Base.Prelude.Abort.Symbol

-- | Stop program execution abnormally.
abort :: Abort -> IO ()
abort a = do
  putMessage a
  putStrLn "(ステータス 1 で終了します)"
  Sys.exitWith $ Sys.ExitFailure 1

addAbort :: Abort -> AbortOr a -> AbortOr a
addAbort a2 (Left _) = Left a2
addAbort _ x = x

abortIO :: (a -> IO ()) -> AbortOr a -> IO ()
abortIO _ (Left a)  = abort a
abortIO f (Right output) = f output

putMessage :: Abort -> IO ()
putMessage = putStr . vline . renderStyle sty . messageDoc where
    sty      = style { lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: Abort -> Doc
messageDoc a =
    docv [ text ""
         , text "処理を中断しました"
         , text ""
         , text (label "種類") <> text (name a)
         , text (label "概要") <> text (abortTitle a)
         , opt  (label "おもな情報") $ abortMain a
         , opt  (label "補助情報") $ abortSub a
         , text ""
         , docv $ abortLines a
         , text ""
         , text ""
         ]

label :: String -> String
label = fill 12

fill :: Int -> String -> String
fill n s = s ++ replicate rest ' ' where
    rest = max 0 (n - len)
    len  = sum $ map (size . Char.ord) s where
    size c | c > 255   = 2
           | otherwise = 1

opt :: String -> Doc -> Doc
opt lbl x | isEmpty x = empty
            | otherwise = text lbl <> x

-- | Stop on error @"bug in koshucode"@
bug :: a
bug = error "bug in koshucode"

-- | Lookup association list
(<!!>) :: [(String, a)] -> String -> AbortOr a
(<!!>) assoc key = loop assoc where
    loop [] = Left $ AbortLookup [] key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Abort
( abort
, addAbort
, abortIO
, (<!!>)
  -- * Abort symbol
, AbortOr
, Abort (..)
) where
import qualified System.Exit as Sys
import qualified Data.Char as Char
import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Prelude.Utility

-- | Stop program execution abnormally.
abort :: Abort -> IO ()
abort reason = do
  putMessage reason
  putStrLn "(ステータス 1 で終了します)"
  Sys.exitWith $ Sys.ExitFailure 1

addAbort :: Abort -> AbortOr a -> AbortOr a
addAbort a2 (Left _) = Left a2
addAbort _ x = x

abortIO :: (a -> IO ()) -> AbortOr a -> IO ()
abortIO _ (Left reason)  = abort reason
abortIO f (Right output) = f output

putMessage :: Abort -> IO ()
putMessage = putStr . vline . renderStyle sty . messageDoc where
    sty      = style { lineLength = 60 }
    vline    = unlines . map ("**  " ++) . lines

messageDoc :: Abort -> Doc
messageDoc reason =
    let label = fill 15
    in docv [ text ""
            , text "処理を中断しました"
            , text ""
            , text (label "種類") <> text (name reason)
            , text (label "概要") <> text (title reason)
            , opt  (label "主な情報") $ mainDoc reason
            , opt  (label "詳しい情報") $ subDoc reason
            , text ""
            , text ""
            ]

subDoc :: Abort -> Doc
subDoc _ = empty

fill :: Int -> String -> String
fill n s = s ++ replicate rest ' ' where
    rest = max 0 (n - len)
    len  = sum $ map (size . Char.ord) s where
    size c | c > 255   = 2
           | otherwise = 1

opt :: String -> Doc -> Doc
opt label x | isEmpty x = empty
            | otherwise = text label <> x

(<!!>) :: [(String, a)] -> String -> AbortOr a
(<!!>) assoc key = loop assoc where
    loop [] = Left $ AbortLookup key
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs



-- ----------------------  Abort symbol

type AbortOr a = Either Abort a

data Abort
    = AbortUnknownRelmap String
    | AbortMalformedOperand String
    | AbortMalformedTerms String
    | AbortLookup String
    | AbortUsage [String]
    | AbortMissingTermName String
      deriving (Show, Eq, Ord)

instance Name Abort where
    name = head . words . show

title :: Abort -> String
title (AbortUnknownRelmap _)    = "未知の演算子"
title (AbortLookup _)           = "項目がない"
title (AbortMalformedOperand _) = "演算子の引数がおかしい"
title (AbortMalformedTerms _)   = "項目名と項目内容の並びがおかしい"
title (AbortUsage _)            = "使用法の間違い"
title (AbortMissingTermName _)  = "項目名ではない記号"

mainDoc :: Abort -> Doc
mainDoc (AbortUnknownRelmap s)    = paragraph s
mainDoc (AbortLookup s)           = paragraph s
mainDoc (AbortMalformedOperand s) = paragraph s
mainDoc (AbortMalformedTerms s)   = paragraph s
mainDoc (AbortUsage ss)           = docv $ map paragraph ss
mainDoc (AbortMissingTermName s)  = paragraph s

paragraph :: String -> Doc
paragraph = fsep . map text . words


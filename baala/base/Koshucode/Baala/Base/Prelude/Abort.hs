{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.Abort
( bug
, abort
, addAbort
, abortIO
, (<!!>)
  -- * Abort symbol
, AbortOr
, Abort (..)
, SourceLine (..)
) where
import qualified System.Exit as Sys
import qualified Data.Char as Char
import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Prelude.Utility
import Data.Generics hiding (empty)

bug :: a
bug = error "bug in koshucode"

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
    = AbortLookup String
    | AbortMalformedOperand String
    | AbortMalformedTerms String
    | AbortMissingTermName String
    | AbortUnknownClause [SourceLine]
    | AbortUnknownRelmap String
    | AbortUsage [String]
      deriving (Show, Eq, Ord)

instance Name Abort where
    name = head . words . show

-- | Line number and content in source code
data SourceLine
    = SourceLine Int String
      deriving (Show, Eq, Ord, Data, Typeable)

title :: Abort -> String
title (AbortLookup _)           = "項目がない"
title (AbortMalformedOperand _) = "演算子の引数がおかしい"
title (AbortMalformedTerms _)   = "項目名と項目内容の並びがおかしい"
title (AbortMissingTermName _)  = "項目名ではない記号"
title (AbortUnknownClause _)    = "未知の構文"
title (AbortUnknownRelmap _)    = "未知の演算子"
title (AbortUsage _)            = "使用法の間違い"

mainDoc :: Abort -> Doc
mainDoc (AbortLookup s)           = paragraph s
mainDoc (AbortMalformedOperand s) = paragraph s
mainDoc (AbortMalformedTerms s)   = paragraph s
mainDoc (AbortMissingTermName s)  = paragraph s
mainDoc (AbortUnknownClause _)    = empty
mainDoc (AbortUnknownRelmap s)    = paragraph s
mainDoc (AbortUsage ss)           = docv $ map paragraph ss

paragraph :: String -> Doc
paragraph = fsep . map text . words

subDoc :: Abort -> Doc
subDoc (AbortUnknownClause src) = srcDoc src
subDoc _ = empty

-- Print source lines
srcDoc :: [SourceLine] -> Doc
srcDoc = docv . map s where
    s (SourceLine n line) = int n <+> text line


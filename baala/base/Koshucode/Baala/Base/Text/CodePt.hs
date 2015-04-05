{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.CodePt
  ( -- * Code point
    CodePt (..),
    codePtZero,
    codePtColumnNo,
    codePtDisplay,
  
    -- * Code pointer
    CodePtr (..),

    -- * Sourced
    Sourced (..),
  ) where

import qualified Data.Generics                      as G
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text.IOPoint  as B


-- ----------------------  CodePt

data CodePt = CodePt
      { codePtSource     :: B.CodePiece    -- ^ Source of code
      , codePtLineNo     :: Int         -- ^ Line number
      , codePtLineText   :: String      -- ^ Line content
      , codePtText       :: String      -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

instance Ord CodePt where
    compare = codePtCompare

codePtCompare :: CodePt -> CodePt -> Ordering
codePtCompare p1 p2 = line `B.mappend` column where
    line   = codePtLineNo p1 `compare` codePtLineNo p2
    column = size p2 `compare` size p1
    size   = length . codePtText

-- | Empty code point, i.e., empty content and zero line number.
codePtZero :: CodePt
codePtZero = CodePt B.codeEmpty 0 "" ""

-- | Column number at which code starts.
codePtColumnNo :: CodePt -> Int
codePtColumnNo CodePt { codePtLineText = line, codePtText = subline }
    = length line - length subline

codePtDisplay :: (String, CodePt) -> [(String, String)]
codePtDisplay (tag, p)
    | lno > 0   = [ (pos, ""), ("> " ++ shorten text, tag) ]
    | otherwise = []
    where
      pos       = show lno ++ " " ++ show cno ++ " " ++ code
      lno       = codePtLineNo p
      cno       = codePtColumnNo p
      code      = B.ioPointText $ B.codeName $ codePtSource p
      text      = codePtText p

      shorten :: B.Map String
      shorten s | length s > 48  = take 45 s ++ "..."
                | otherwise      = s


-- ----------------------  CodePtr

class CodePtr a where
    codePtList :: a -> [CodePt]

    codePt :: a ->  CodePt
    codePt p = B.headOr codePtZero $ codePtList p

instance CodePtr CodePt where
    codePtList cp  = [cp]
    codePt     cp  =  cp


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePtList = source


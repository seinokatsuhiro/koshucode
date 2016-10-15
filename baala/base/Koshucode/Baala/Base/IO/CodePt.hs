{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.IO.CodePt
  ( -- * Code point
    CodePt (..),
    codePtColumnNo,
    codePtDisplay,
  
    -- * Code pointer
    CodePtr (..),

    -- * Sourced
    Sourced (..),
  ) where

import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.IO.IOPoint    as B


-- ----------------------  CodePt

-- | Point of input code.
data CodePt = CodePt
      { codePtSource     :: B.NIOPoint  -- ^ Source of code
      , codePtLineNo     :: Int         -- ^ Line number
      , codePtLineText   :: String      -- ^ Line content
      , codePtText       :: String      -- ^ Text at which begins token
      } deriving (Eq)

-- | Number of input point, line number, and column number,
--   e.g., @\<I1-L6-C14\>@.
instance Show CodePt where
    show cp = "<I" ++ show (B.nioNumber $ codePtSource cp)
              ++ "-L" ++ show (codePtLineNo cp)
              ++ "-C" ++ show (codePtColumnNo cp)
              ++ ">"

instance Ord CodePt where
    compare = codePtCompare

-- | Empty code point, i.e., empty content and zero line number.
instance B.Default CodePt where
    def = CodePt B.def 0 "" ""

codePtCompare :: CodePt -> CodePt -> Ordering
codePtCompare p1 p2 = line B.<> column where
    line   = codePtLineNo p1 `compare` codePtLineNo p2
    column = size p2 `compare` size p1
    size   = length . codePtText

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
      code      = B.ioPointText $ B.nioPoint $ codePtSource p
      text      = codePtText p

      shorten :: O.Map String
      shorten s | length s > 48  = take 45 s ++ "..."
                | otherwise      = s


-- ----------------------  CodePtr

class CodePtr a where
    codePtList :: a -> [CodePt]

    codePt :: a ->  CodePt
    codePt p = B.headNull B.def $ codePtList p

instance CodePtr CodePt where
    codePtList cp  = [cp]
    codePt     cp  =  cp


-- ----------------------  Sourced

data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePtList = source


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Source code information.

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
import qualified Koshucode.Baala.Base.Text          as B
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
--   e.g., @\/1.6.14\/@.
instance Show CodePt where
    show = longSlash

-- | Line number and column number, e.g., @\/6.14\/@.
instance B.PPrint CodePt where
    pprint = B.pprint . shortSlash

-- showAngle :: CodePt -> String
-- showAngle = showCp (\s l c -> "<I" ++ s ++ "-L" ++ l ++ "-C" ++ c ++ ">")

longSlash :: CodePt -> String
longSlash = showCp (\s l c -> "/" ++ s ++ "." ++ l ++ "." ++ c ++ "/")

shortSlash :: CodePt -> String
shortSlash = showCp (\_ l c -> "/" ++ l ++ "." ++ c ++ "/")

showCp :: (String -> String -> String -> String) -> CodePt -> String
showCp f cp = f s l c where
    s = show $ B.nioNumber $ codePtSource cp
    l = show $ codePtLineNo cp
    c = show $ codePtColumnNo cp

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

-- | Create position and line information.
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

      shorten :: O.StringMap
      shorten s | length s > 48  = take 45 s ++ "..."
                | otherwise      = s


-- ----------------------  CodePtr

-- | Type which has code points.
class CodePtr a where
    codePtList :: a -> [CodePt]

    codePt :: a -> CodePt
    codePt p = B.headNull B.def $ codePtList p

instance CodePtr CodePt where
    codePtList cp  = [cp]
    codePt     cp  =  cp

instance (CodePtr cp) => CodePtr [cp] where
    codePtList = concatMap codePtList


-- ----------------------  Sourced

-- | Type with source code information.
data Sourced a =
    Sourced { source    :: [CodePt]
            , unsourced :: a
            } deriving (Show, Eq, Ord)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePtList = source


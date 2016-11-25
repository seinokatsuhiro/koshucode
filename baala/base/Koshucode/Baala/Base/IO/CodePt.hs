{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Source code information.

module Koshucode.Baala.Base.IO.CodePt
  ( -- * Code position
    CodePos (..),
    cpColumnNo,
    cpDisplay,
  
    -- * Code pointer
    CodePtr (..),

    -- * Sourced
    Sourced (..),
  ) where

import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text          as B
import qualified Koshucode.Baala.Base.IO.IOPoint    as B


-- ----------------------  CodePos

-- | Point of input code.
data CodePos = CodePos
      { cpSource     :: B.NIOPoint  -- ^ Source of code
      , cpLineNo     :: Int         -- ^ Line number
      , cpLineText   :: String      -- ^ Line content
      , cpText       :: String      -- ^ Text at which begins token
      } deriving (Eq)

-- | Number of input point, line number, and column number,
--   e.g., @\/1.6.14\/@.
instance Show CodePos where
    show = longSlash

-- | Line number and column number, e.g., @\/6.14\/@.
instance B.PPrint CodePos where
    pprint = B.pprint . shortSlash

-- showAngle :: CodePos -> String
-- showAngle = showCp (\s l c -> "<I" ++ s ++ "-L" ++ l ++ "-C" ++ c ++ ">")

longSlash :: CodePos -> String
longSlash = showCp (\s l c -> "/" ++ s ++ "." ++ l ++ "." ++ c ++ "/")

shortSlash :: CodePos -> String
shortSlash = showCp (\_ l c -> "/" ++ l ++ "." ++ c ++ "/")

showCp :: (String -> String -> String -> String) -> CodePos -> String
showCp f cp = f s l c where
    s = show $ B.nioNumber $ cpSource cp
    l = show $ cpLineNo cp
    c = show $ cpColumnNo cp

instance Ord CodePos where
    compare = cpCompare

-- | Empty code point, i.e., empty content and zero line number.
instance B.Default CodePos where
    def = CodePos B.def 0 "" ""

cpCompare :: CodePos -> CodePos -> Ordering
cpCompare p1 p2 = line B.<> column where
    line   = cpLineNo p1 `compare` cpLineNo p2
    column = size p2 `compare` size p1
    size   = length . cpText

-- | Column number at which code starts.
cpColumnNo :: CodePos -> Int
cpColumnNo CodePos { cpLineText = line, cpText = subline }
    = length line - length subline

-- | Create position and line information.
cpDisplay :: (String, CodePos) -> [(String, String)]
cpDisplay (tag, p)
    | lno > 0   = [ (pos, ""), ("> " ++ shorten text, tag) ]
    | otherwise = []
    where
      pos       = show lno ++ " " ++ show cno ++ " " ++ code
      lno       = cpLineNo p
      cno       = cpColumnNo p
      code      = B.ioPointText $ B.nioPoint $ cpSource p
      text      = cpText p

      shorten :: O.StringMap
      shorten s | length s > 48  = take 45 s ++ "..."
                | otherwise      = s


-- ----------------------  CodePtr

-- | Type which has code points.
class CodePtr a where
    codePtList :: a -> [CodePos]

    codePt :: a -> CodePos
    codePt p = B.headNull B.def $ codePtList p

instance CodePtr CodePos where
    codePtList cp  = [cp]
    codePt     cp  =  cp

instance (CodePtr cp) => CodePtr [cp] where
    codePtList = concatMap codePtList


-- ----------------------  Sourced

-- | Type with source code information.
data Sourced a =
    Sourced { source    :: [CodePos]
            , unsourced :: a
            } deriving (Show, Eq, Ord)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance CodePtr (Sourced a) where
    codePtList = source


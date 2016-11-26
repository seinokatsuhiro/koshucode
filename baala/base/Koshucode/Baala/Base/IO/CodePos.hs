{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Source code information.

module Koshucode.Baala.Base.IO.CodePos
  ( -- * Code position
    CodePos (..),
    cpColumnNo,
  
    -- * Get code positions
    GetCodePos (..),

    -- * Sourced
    Sourced (..),
  ) where

import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text          as B
import qualified Koshucode.Baala.Base.IO.IOPoint    as B


-- ----------------------  CodePos

-- | Position of input code.
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

-- | Empty code position, i.e., empty content and zero line number.
instance B.Default CodePos where
    def = CodePos B.def 0 "" ""

cpCompare :: CodePos -> CodePos -> Ordering
cpCompare p1 p2 = line B.<> column where
    line   = cpLineNo p1 `compare` cpLineNo p2
    column = size p2 `compare` size p1
    size   = length . cpText

-- | Column number at which code starts.
--
--   >>> let cp = CodePos (B.nioFrom "abcdefg") 1 "abcdefg" "abc"
--   >>> cp
--   /0.1.4/
--   >>> cpColumnNo cp
--   4
--
cpColumnNo :: CodePos -> Int
cpColumnNo CodePos { cpLineText = line, cpText = subline }
    = length line - length subline


-- ----------------------  GetCodePos

-- | Type which has code positions.
class GetCodePos a where
    getCPs :: a -> [CodePos]

    getCP :: a -> CodePos
    getCP a = B.headNull B.def $ getCPs a

instance GetCodePos CodePos where
    getCPs cp  = [cp]
    getCP  cp  =  cp

instance (GetCodePos cp) => GetCodePos [cp] where
    getCPs = concatMap getCPs


-- ----------------------  Sourced

-- | Type with source code information.
data Sourced a =
    Sourced { source    :: [CodePos]
            , unsourced :: a
            } deriving (Show, Eq, Ord)

instance Functor Sourced where
    fmap f (Sourced src x) = Sourced src $ f x

instance GetCodePos (Sourced a) where
    getCPs = source


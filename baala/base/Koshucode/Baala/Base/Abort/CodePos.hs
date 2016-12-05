{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Position on code string.

module Koshucode.Baala.Base.Abort.CodePos
  ( -- * Code position
    CodePos (..),
    cpColumnNo,
    cpSplit,

    -- * Get code positions
    GetCodePos (..),

    -- * Codic
    Codic (..),
    codic, noCodic,
  ) where

import qualified Data.List                          as Li
import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text          as B


-- ----------------------  CodePos

-- | Position on input code string.
data CodePos = CodePos
      { cpIndex      :: O.Ix       -- ^ Index of code source
      , cpPath       :: FilePath   -- ^ Path of code source
      , cpLineNo     :: Int        -- ^ Line number
      , cpLineText   :: String     -- ^ Code string line
      , cpText       :: String     -- ^ Current code string
      } deriving (Eq)

-- | Number of input point, line number, and column number,
--   e.g., @\/1.6.14\/@.
instance Show CodePos where
    show = longSlash

-- | Line number and column number, e.g., @\/6.14\/@.
instance B.PPrint CodePos where
    pprint = B.pprint . shortSlash

instance O.GetIx CodePos where
    getIx = cpIndex

instance O.GetIOPath CodePos where
    getIOPath = cpPath

longSlash :: CodePos -> String
longSlash = showCp (\s l c -> "/" ++ s ++ "." ++ l ++ "." ++ c ++ "/")

shortSlash :: CodePos -> String
shortSlash = showCp (\_ l c -> "/" ++ l ++ "." ++ c ++ "/")

showCp :: (String -> String -> String -> String) -> CodePos -> String
showCp f cp = f s l c where
    s = show $ O.getIx cp
    l = show $ cpLineNo cp
    c = show $ cpColumnNo cp

instance Ord CodePos where
    compare = cpCompare

cpCompare :: CodePos -> CodePos -> Ordering
cpCompare p1 p2 = line O.++ column where
    line   = cpLineNo p1 `compare` cpLineNo p2
    column = size p2 `compare` size p1
    size   = length . cpText

-- | Empty code position, i.e., empty content and zero line number.
instance B.Default CodePos where
    def = CodePos { cpIndex    = 0
                  , cpPath     = ""
                  , cpLineNo   = 0
                  , cpLineText = ""
                  , cpText     = "" }

-- | Column number at which code starts.
--
--   >>> let cp = CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   >>> cp
--   /0.1.3/
--   >>> cpColumnNo cp
--   3
--
cpColumnNo :: CodePos -> Int
cpColumnNo CodePos { cpLineText = line, cpText = subline }
    = length line - length subline

-- | Before and after text of code position.
--
--   >>> cpSplit $ CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   ("abc", "defg")
--
cpSplit :: CodePos -> (String, String)
cpSplit CodePos { cpLineText = ln, cpText = t } =
    case stripSuffix t ln of
      Just s  -> (s, t)
      Nothing -> (ln, t)

stripSuffix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripSuffix suf s = reverse <$> Li.stripPrefix (reverse suf) (reverse s)


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


-- ----------------------  Codic

-- | Value with code string.
data Codic a =
    Codic { codicCPs :: [CodePos]    -- ^ Code positions.
          , uncodic  :: a            -- ^ Value.
          } deriving (Show, Eq, Ord)

instance Functor Codic where
    fmap f (Codic cp a) = Codic cp $ f a

instance GetCodePos (Codic a) where
    getCPs = codicCPs

-- | Create value with code position.
codic :: (GetCodePos cp) => cp -> a -> Codic a
codic cp = Codic $ getCPs cp

-- | Create codic value without code string.
noCodic :: a -> Codic a
noCodic = Codic []


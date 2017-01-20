{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Position on code string.

module Koshucode.Baala.Base.Abort.CodePos
  ( -- * Code position
    CodePos (..),
    cpCharNo,
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
    show = showCp posText

instance O.GetIx CodePos where
    getIx = cpIndex

instance O.GetIOPath CodePos where
    getIOPath = cpPath

posText :: String -> String -> String -> String
posText s l c = "/" ++ s ++ "." ++ l ++ "." ++ c ++ "/"

showCp :: (String -> String -> String -> String) -> CodePos -> String
showCp f cp = f s l c where
    s = show $ O.getIx cp
    l = show $ cpLineNo cp
    c = show $ cpCharNo cp

instance Ord CodePos where
    compare = cpCompare

cpCompare :: CodePos -> CodePos -> Ordering
cpCompare p1 p2 = line O.++ char where
    line = cpLineNo p1 `compare` cpLineNo p2
    char = cpTextLength p2 `compare` cpTextLength p1

cpTextLength :: CodePos -> Int
cpTextLength = O.tLength . cpText

-- | Empty code position, i.e., empty content and zero line number.
instance B.Default CodePos where
    def = CodePos { cpIndex    = 0
                  , cpPath     = O.tEmpty
                  , cpLineNo   = 0
                  , cpLineText = O.tEmpty
                  , cpText     = O.tEmpty }

-- | Character position at which code starts.
--
--   >>> let cp = B.def { cpLineText = "abcdefg", cpText = "defg" }
--   >>> cp
--   /0.0.3/
--   >>> cpCharNo cp
--   3
--
cpCharNo :: CodePos -> Int
cpCharNo CodePos { cpLineText = line, cpText = subline }
    = O.tLength line - O.tLength subline

-- | Before and after text of code position.
--
--   >>> cpSplit $ CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   ("abc", "defg")
--
cpSplit :: CodePos -> (String, String)
cpSplit CodePos { cpLineText = ln, cpText = t } =
    case tStripSuffix t ln of
      Just s  -> (s, t)
      Nothing -> (ln, t)

-- >>> tStripSuffix "defg" "abcdefg"
-- Just "abc"
tStripSuffix :: (O.Textual t) => t -> t -> Maybe t
tStripSuffix suf s = stripped where
    stripped = (O.stringT . reverse) <$> Li.stripPrefix suf' s'
    suf'     = reverse $ O.tString suf
    s'       = reverse $ O.tString s


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


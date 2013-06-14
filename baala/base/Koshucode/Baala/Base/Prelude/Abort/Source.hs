{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Source information

module Koshucode.Baala.Base.Prelude.Abort.Source
( SourceLine (..)
) where
import qualified Data.Char as Char
import Koshucode.Baala.Base.Prelude.Pretty
import Data.Generics

-- | Line number and content in source code
data SourceLine
    = SourceLine Int String
      deriving (Show, Eq, Ord, Data, Typeable)

instance Pretty SourceLine where
    doc (SourceLine n line) =
        text (label $ show n) <> text line

label :: String -> String
label = fill 12

fill :: Int -> String -> String
fill n s = s ++ replicate rest ' ' where
    rest = max 0 (n - len)
    len  = sum $ map (size . Char.ord) s where
    size c | c > 255   = 2
           | otherwise = 1


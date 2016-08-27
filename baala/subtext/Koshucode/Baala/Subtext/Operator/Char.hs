{-# OPTIONS_GHC -Wall #-}

-- | Operators for text match.

module Koshucode.Baala.Subtext.Operator.Char
 ( -- * Character
   char, word,
   space, spaces, spaces1,
   digit, letter,
 ) where

import qualified Data.Char                                   as C
import qualified Koshucode.Baala.Subtext.Expr                as S
import qualified Koshucode.Baala.Subtext.Operator.Basic      as S
import qualified Koshucode.Baala.Subtext.Operator.Repeat     as S
import qualified Koshucode.Baala.Subtext.Operator.Combine    as S

-- | Match some character in a list of characters.
char :: [Char] -> S.Expr Char
char = S.list

-- | Match some word in a list of words from space-separated string.
word :: String -> S.Expr Char
word w = S.or (S.list <$> words w)

-- | Match space character.
space :: S.Expr Char
space = S.elem "space" C.isSpace

-- | Match zero-or-more space characters.
spaces :: S.Expr Char
spaces = S.many space

-- | Match one-or-more space characters.
spaces1 :: S.Expr Char
spaces1 = S.many1 space

-- | Match digit characters.
digit :: S.Expr Char
digit = S.elem "digit" C.isDigit

-- | Match letter characters.
letter :: S.Expr Char
letter = S.elem "letter" C.isLetter


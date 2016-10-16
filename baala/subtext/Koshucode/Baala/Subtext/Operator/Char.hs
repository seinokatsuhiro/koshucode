{-# OPTIONS_GHC -Wall #-}

-- | Operators for text match.

module Koshucode.Baala.Subtext.Operator.Char
 ( -- * Type
   CharExpr,

   -- * Character
   char, word,
   space, spaces, spaces1,
   digit, letter,
   ascii, latin1,

   -- * General category
   categorySet, categoryList,
   category,

   -- * Conversion
   asLower,
   asUpper,
 ) where

import qualified Data.Char                                   as C
import qualified Data.Set                                    as Set
import qualified Koshucode.Baala.Overture                    as O
import qualified Koshucode.Baala.Overture.Fn                 as O
import qualified Koshucode.Baala.Subtext.Expr                as T
import qualified Koshucode.Baala.Subtext.Operator.Basic      as T
import qualified Koshucode.Baala.Subtext.Operator.Repeat     as T
import qualified Koshucode.Baala.Subtext.Operator.Combine    as T

-- | Subtext expression for character input.
type CharExpr = T.Expr Char

-- | Match some character in a list of characters.
char :: [Char] -> CharExpr
char = T.list

-- | Match some word in a list of words from space-separated string.
word :: String -> CharExpr
word w = T.or (T.equal <$> words w)

-- | Match space character.
space :: CharExpr
space = T.elem "space" C.isSpace

-- | Match zero-or-more space characters.
spaces :: CharExpr
spaces = T.many space

-- | Match one-or-more space characters.
spaces1 :: CharExpr
spaces1 = T.many1 space

-- | Match digit characters.
digit :: CharExpr
digit = T.elem "digit" C.isDigit

-- | Match letter characters.
letter :: CharExpr
letter = T.elem "letter" C.isLetter

-- | Match first 128 characters of Unicode character set.
ascii :: CharExpr
ascii = T.elem "letter" C.isAscii

-- | Match first 256 characters of Unicode character set.
latin1 :: CharExpr
latin1 = T.elem "letter" C.isLatin1


-- --------------------------------------------  Category

-- | Test unicode general category with category set.
categorySet :: Set.Set C.GeneralCategory -> CharExpr
categorySet cs = T.elem "category" cat where
    cat c = C.generalCategory c `Set.member` cs

-- | Test unicode general category with category list.
categoryList :: [C.GeneralCategory] -> CharExpr
categoryList = categorySet . Set.fromList

-- | Test unicode general category with short names.
--   The argument string is space-separated names.
--   If unknown name is given, returns the name in 'Left' data.
category :: String -> Either String CharExpr
category s =
    do cs <- O.categoryLookup `mapM` words s
       Right $ categorySet $ Set.unions cs


-- --------------------------------------------  Conversion

-- | Convert matched text into lower case.
asLower :: CharExpr -> CharExpr
asLower = T.as (O.Fn "lower" $ map C.toLower)

-- | Convert matched text into upper case.
asUpper :: CharExpr -> CharExpr
asUpper = T.as (O.Fn "upper" $ map C.toUpper)

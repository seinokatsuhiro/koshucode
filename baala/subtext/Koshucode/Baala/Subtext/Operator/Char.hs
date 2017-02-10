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

import qualified Data.Char                                   as Ch
import qualified Data.Set                                    as Set
import qualified Koshucode.Baala.Overture                    as O
import qualified Koshucode.Baala.Overture.Fn                 as O
import qualified Koshucode.Baala.Subtext.Expr                as T
import qualified Koshucode.Baala.Subtext.Operator.Basic      as T
import qualified Koshucode.Baala.Subtext.Operator.Repeat     as T
import qualified Koshucode.Baala.Subtext.Operator.Combine    as T

-- | Subtext expression for character input.
type CharExpr t = T.Expr t Char

-- | Match some character in a list of characters.
char :: [Char] -> CharExpr t
char = T.list

-- | Match some word in a list of words from space-separated string.
word :: (O.Textual t) => t -> CharExpr t
word w = T.or (T.equal <$> O.tWords w)

-- | Match space character.
space :: CharExpr t
space = T.elem "space" Ch.isSpace

-- | Match zero-or-more space characters.
spaces :: CharExpr t
spaces = T.many space

-- | Match one-or-more space characters.
spaces1 :: CharExpr t
spaces1 = T.many1 space

-- | Match digit characters.
digit :: CharExpr t
digit = T.elem "digit" Ch.isDigit

-- | Match letter characters.
letter :: CharExpr t
letter = T.elem "letter" Ch.isLetter

-- | Match first 128 characters of Unicode character set.
ascii :: CharExpr t
ascii = T.elem "letter" Ch.isAscii

-- | Match first 256 characters of Unicode character set.
latin1 :: CharExpr t
latin1 = T.elem "letter" Ch.isLatin1


-- --------------------------------------------  Category

-- | Test unicode general category with category set.
categorySet :: Set.Set Ch.GeneralCategory -> CharExpr t
categorySet cs = T.elem "category" cat where
    cat c = Ch.generalCategory c `Set.member` cs

-- | Test unicode general category with category list.
categoryList :: [Ch.GeneralCategory] -> CharExpr t
categoryList = categorySet . Set.fromList

-- | Test unicode general category with short names.
--   The argument string is space-separated names.
--   If unknown name is given, returns the name in 'Left' data.
category :: String -> Either String (CharExpr t)
category s =
    do cs <- O.categoryLookup `mapM` words s
       Right $ categorySet $ Set.unions cs


-- --------------------------------------------  Conversion

-- | Convert matched text into lower case.
asLower :: (O.Textual t) => CharExpr t -> CharExpr t
asLower = T.as (O.Fn "lower" $ O.map Ch.toLower)

-- | Convert matched text into upper case.
asUpper :: (O.Textual t) => CharExpr t -> CharExpr t
asUpper = T.as (O.Fn "upper" $ O.map Ch.toUpper)

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
   categorySet, category, categoryLookup,
 ) where

import qualified Data.Char                                   as C
import qualified Data.Set                                    as Set
import qualified Koshucode.Baala.Subtext.Expr                as S
import qualified Koshucode.Baala.Subtext.Operator.Basic      as S
import qualified Koshucode.Baala.Subtext.Operator.Repeat     as S
import qualified Koshucode.Baala.Subtext.Operator.Combine    as S

-- | Subtext expression for character input.
type CharExpr = S.Expr Char

-- | Match some character in a list of characters.
char :: [Char] -> CharExpr
char = S.list

-- | Match some word in a list of words from space-separated string.
word :: String -> CharExpr
word w = S.or (S.equal <$> words w)

-- | Match space character.
space :: CharExpr
space = S.elem "space" C.isSpace

-- | Match zero-or-more space characters.
spaces :: CharExpr
spaces = S.many space

-- | Match one-or-more space characters.
spaces1 :: CharExpr
spaces1 = S.many1 space

-- | Match digit characters.
digit :: CharExpr
digit = S.elem "digit" C.isDigit

-- | Match letter characters.
letter :: CharExpr
letter = S.elem "letter" C.isLetter

-- | Match first 128 characters of Unicode character set.
ascii :: CharExpr
ascii = S.elem "letter" C.isAscii

-- | Match first 256 characters of Unicode character set.
latin1 :: CharExpr
latin1 = S.elem "letter" C.isLatin1


-- --------------------------------------------  Category

-- | Test unicode general category with category set.
categorySet :: Set.Set C.GeneralCategory -> CharExpr
categorySet cs = S.elem "category" cat where
    cat c = C.generalCategory c `Set.member` cs

-- | Test unicode general category with short names.
--   The argument string is space-separated names.
--   If unknown name is given, returns the name in 'Left' data.
category :: String -> Either String CharExpr
category s =
    do cs <- categoryLookup `mapM` words s
       Right $ categorySet $ Set.unions cs

-- | Lookup unicode general cateogries by its short name.
--   Short name means two-letter category name
--   (e.g., @"lu"@ for upper case letter, @"zs"@ for space), or
--   one-letter category group (e.g., @"l"@ for letter, @"z"@ for separator).
--
--   >>> categoryLookup "lu"
--   Right (fromList [UppercaseLetter])
--
--   >>> categoryLookup "n"
--   Right (fromList [DecimalNumber, LetterNumber, OtherNumber])
--
--   >>> categoryLookup "lx"
--   Left "lx"

categoryLookup :: String -> Either String (Set.Set C.GeneralCategory)
categoryLookup n =
    case C.toLower <$> n of
      "l"     -> l [ C.UppercaseLetter, C.LowercaseLetter, C.TitlecaseLetter,
                     C.ModifierLetter, C.OtherLetter ]
      "m"     -> l [ C.NonSpacingMark, C.SpacingCombiningMark, C.EnclosingMark ]
      "n"     -> l [ C.DecimalNumber, C.LetterNumber, C.OtherNumber ]
      "p"     -> l [ C.ConnectorPunctuation, C.DashPunctuation,
                     C.OpenPunctuation, C.ClosePunctuation, C.InitialQuote,
                     C.FinalQuote, C.OtherPunctuation ]
      "s"     -> l [ C.MathSymbol, C.CurrencySymbol,
                     C.ModifierSymbol, C.OtherSymbol ]
      "z"     -> l [ C.Space, C.LineSeparator, C.ParagraphSeparator ]
      "c"     -> l [ C.Control, C.Format, C.Surrogate,
                     C.PrivateUse, C.NotAssigned ]

      "lu"    -> s C.UppercaseLetter
      "ll"    -> s C.LowercaseLetter
      "lt"    -> s C.TitlecaseLetter
      "lm"    -> s C.ModifierLetter
      "lo"    -> s C.OtherLetter

      "mn"    -> s C.NonSpacingMark
      "mc"    -> s C.SpacingCombiningMark
      "me"    -> s C.EnclosingMark

      "nd"    -> s C.DecimalNumber
      "nl"    -> s C.LetterNumber
      "no"    -> s C.OtherNumber

      "pc"    -> s C.ConnectorPunctuation
      "pd"    -> s C.DashPunctuation
      "ps"    -> s C.OpenPunctuation
      "pe"    -> s C.ClosePunctuation
      "pi"    -> s C.InitialQuote
      "pf"    -> s C.FinalQuote
      "po"    -> s C.OtherPunctuation

      "sm"    -> s C.MathSymbol
      "sc"    -> s C.CurrencySymbol
      "sk"    -> s C.ModifierSymbol
      "so"    -> s C.OtherSymbol

      "zs"    -> s C.Space
      "zl"    -> s C.LineSeparator
      "zp"    -> s C.ParagraphSeparator

      "cc"    -> s C.Control
      "cf"    -> s C.Format
      "cs"    -> s C.Surrogate
      "co"    -> s C.PrivateUse
      "cn"    -> s C.NotAssigned

      _       -> Left n
    where
      l = Right . Set.fromList
      s = Right . Set.singleton

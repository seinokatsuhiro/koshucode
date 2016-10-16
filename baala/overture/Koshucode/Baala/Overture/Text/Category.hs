{-# OPTIONS_GHC -Wall #-}

-- | Unicode general category.

module Koshucode.Baala.Overture.Text.Category
 ( -- * Category group
   categoryLetter,
   categoryMark,
   categoryNumber,
   categoryPunctuation,
   categorySymbol,
   categorySeparator,
   categoryOther,
   categoryAlpha,
   categorySign,
   categoryOpen,
   categoryClose,

   -- * Category lookup
   categoryLookup,
 ) where

import qualified Data.Char      as C
import qualified Data.Set       as Set


-- --------------------------------------------  Category group

-- | List of letter categories (L).
categoryLetter      :: [C.GeneralCategory]
categoryLetter       = [C.UppercaseLetter, C.LowercaseLetter, C.TitlecaseLetter,
                        C.ModifierLetter, C.OtherLetter]

-- | List of mark categories (M).
categoryMark        :: [C.GeneralCategory]
categoryMark         = [C.NonSpacingMark, C.SpacingCombiningMark, C.EnclosingMark]

-- | List of number categories (N).
categoryNumber      :: [C.GeneralCategory]
categoryNumber       = [C.DecimalNumber, C.LetterNumber, C.OtherNumber]

-- | List of punctuation categories (P).
categoryPunctuation :: [C.GeneralCategory]
categoryPunctuation  = [C.ConnectorPunctuation, C.DashPunctuation,
                        C.OpenPunctuation, C.ClosePunctuation, C.InitialQuote,
                        C.FinalQuote, C.OtherPunctuation]

-- | List of symbol categories (S).
categorySymbol      :: [C.GeneralCategory]
categorySymbol       = [C.MathSymbol, C.CurrencySymbol,
                        C.ModifierSymbol, C.OtherSymbol]

-- | List of separator categories (Z).
categorySeparator   :: [C.GeneralCategory]
categorySeparator    = [C.Space, C.LineSeparator, C.ParagraphSeparator]

-- | List of other categories (C).
categoryOther       :: [C.GeneralCategory]
categoryOther        = [C.Control, C.Format, C.Surrogate,
                        C.PrivateUse, C.NotAssigned]

-- | List of alphabetic categories (L + M).
categoryAlpha       :: [C.GeneralCategory]
categoryAlpha        = categoryLetter ++ categoryMark

-- | List of textual-sign categories (P + S).
categorySign        :: [C.GeneralCategory]
categorySign         = categoryPunctuation ++ categorySymbol

-- | List of open brackets (Ps + Pi).
categoryOpen        :: [C.GeneralCategory]
categoryOpen         = [C.OpenPunctuation, C.InitialQuote]

-- | List of close brackets (Pe + Pf).
categoryClose       :: [C.GeneralCategory]
categoryClose        = [C.ClosePunctuation, C.FinalQuote]


-- --------------------------------------------  Category lookup

-- | Lookup unicode general categories by its short name.
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
      "l"     -> l categoryLetter
      "m"     -> l categoryMark
      "n"     -> l categoryNumber
      "p"     -> l categoryPunctuation
      "s"     -> l categorySymbol
      "z"     -> l categorySeparator
      "c"     -> l categoryOther

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

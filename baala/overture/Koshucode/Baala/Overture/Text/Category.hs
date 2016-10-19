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

import qualified Data.Char      as Ch
import qualified Data.Set       as Set


-- --------------------------------------------  Category group

-- | List of letter categories (L).
categoryLetter      :: [Ch.GeneralCategory]
categoryLetter       = [Ch.UppercaseLetter, Ch.LowercaseLetter, Ch.TitlecaseLetter,
                        Ch.ModifierLetter, Ch.OtherLetter]

-- | List of mark categories (M).
categoryMark        :: [Ch.GeneralCategory]
categoryMark         = [Ch.NonSpacingMark, Ch.SpacingCombiningMark, Ch.EnclosingMark]

-- | List of number categories (N).
categoryNumber      :: [Ch.GeneralCategory]
categoryNumber       = [Ch.DecimalNumber, Ch.LetterNumber, Ch.OtherNumber]

-- | List of punctuation categories (P).
categoryPunctuation :: [Ch.GeneralCategory]
categoryPunctuation  = [Ch.ConnectorPunctuation, Ch.DashPunctuation,
                        Ch.OpenPunctuation, Ch.ClosePunctuation, Ch.InitialQuote,
                        Ch.FinalQuote, Ch.OtherPunctuation]

-- | List of symbol categories (S).
categorySymbol      :: [Ch.GeneralCategory]
categorySymbol       = [Ch.MathSymbol, Ch.CurrencySymbol,
                        Ch.ModifierSymbol, Ch.OtherSymbol]

-- | List of separator categories (Z).
categorySeparator   :: [Ch.GeneralCategory]
categorySeparator    = [Ch.Space, Ch.LineSeparator, Ch.ParagraphSeparator]

-- | List of other categories (C).
categoryOther       :: [Ch.GeneralCategory]
categoryOther        = [Ch.Control, Ch.Format, Ch.Surrogate,
                        Ch.PrivateUse, Ch.NotAssigned]

-- | List of alphabetic categories (L + M).
categoryAlpha       :: [Ch.GeneralCategory]
categoryAlpha        = categoryLetter ++ categoryMark

-- | List of textual-sign categories (P + S).
categorySign        :: [Ch.GeneralCategory]
categorySign         = categoryPunctuation ++ categorySymbol

-- | List of open brackets (Ps + Pi).
categoryOpen        :: [Ch.GeneralCategory]
categoryOpen         = [Ch.OpenPunctuation, Ch.InitialQuote]

-- | List of close brackets (Pe + Pf).
categoryClose       :: [Ch.GeneralCategory]
categoryClose        = [Ch.ClosePunctuation, Ch.FinalQuote]


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

categoryLookup :: String -> Either String (Set.Set Ch.GeneralCategory)
categoryLookup n =
    case Ch.toLower <$> n of
      "l"     -> l categoryLetter
      "m"     -> l categoryMark
      "n"     -> l categoryNumber
      "p"     -> l categoryPunctuation
      "s"     -> l categorySymbol
      "z"     -> l categorySeparator
      "c"     -> l categoryOther

      "lu"    -> s Ch.UppercaseLetter
      "ll"    -> s Ch.LowercaseLetter
      "lt"    -> s Ch.TitlecaseLetter
      "lm"    -> s Ch.ModifierLetter
      "lo"    -> s Ch.OtherLetter

      "mn"    -> s Ch.NonSpacingMark
      "mc"    -> s Ch.SpacingCombiningMark
      "me"    -> s Ch.EnclosingMark

      "nd"    -> s Ch.DecimalNumber
      "nl"    -> s Ch.LetterNumber
      "no"    -> s Ch.OtherNumber

      "pc"    -> s Ch.ConnectorPunctuation
      "pd"    -> s Ch.DashPunctuation
      "ps"    -> s Ch.OpenPunctuation
      "pe"    -> s Ch.ClosePunctuation
      "pi"    -> s Ch.InitialQuote
      "pf"    -> s Ch.FinalQuote
      "po"    -> s Ch.OtherPunctuation

      "sm"    -> s Ch.MathSymbol
      "sc"    -> s Ch.CurrencySymbol
      "sk"    -> s Ch.ModifierSymbol
      "so"    -> s Ch.OtherSymbol

      "zs"    -> s Ch.Space
      "zl"    -> s Ch.LineSeparator
      "zp"    -> s Ch.ParagraphSeparator

      "cc"    -> s Ch.Control
      "cf"    -> s Ch.Format
      "cs"    -> s Ch.Surrogate
      "co"    -> s Ch.PrivateUse
      "cn"    -> s Ch.NotAssigned

      _       -> Left n
    where
      l = Right . Set.fromList
      s = Right . Set.singleton

{-# OPTIONS_GHC -Wall #-}

-- | Unicode general categories.

module Koshucode.Baala.Base.Text.Unicode
  ( GeneralCategoryGroup (..),
    generalCategoryName,
    generalCategoryLetter,
    generalCategoryGroup,
  ) where

import qualified Data.Char as Ch

-- | Major category of the Unicode general categories.
data GeneralCategoryGroup
    = UnicodeLetter          -- ^ Lu Ll Lt Lm Lo
    | UnicodeMark            -- ^ Mn Mc Me
    | UnicodeNumber          -- ^ Nd Nl No
    | UnicodePunctuation     -- ^ Pc Pd Ps Pe Pi Pf Po
    | UnicodeSymbol          -- ^ Sm Sc Sk So
    | UnicodeSeperator       -- ^ Zs Zl Zp
    | UnicodeOther           -- ^ Cc Cf Cs Co Cn
      deriving (Show, Eq, Ord, Bounded)

-- | Name of general category.
--   This function returns one of @letter@, @mark@,
--   @number@, @punct@, @symbol@, @sep@, or @other@.
generalCategoryName :: GeneralCategoryGroup -> String
generalCategoryName UnicodeLetter         = "letter"
generalCategoryName UnicodeMark           = "mark"
generalCategoryName UnicodeNumber         = "number"
generalCategoryName UnicodePunctuation    = "punct"
generalCategoryName UnicodeSymbol         = "symbol"
generalCategoryName UnicodeSeperator      = "sep"
generalCategoryName UnicodeOther          = "other"

-- | One-letter representation of general category.
--   This function returns one of @L@ (letter),
--   @M@ (mark), @N@ (number), @P@ (punctuation),
--   @S@ (symbol), @Z@ (seperator), or @C@ (other).
generalCategoryLetter :: GeneralCategoryGroup -> Char
generalCategoryLetter UnicodeLetter       = 'L'
generalCategoryLetter UnicodeMark         = 'M'
generalCategoryLetter UnicodeNumber       = 'N'
generalCategoryLetter UnicodePunctuation  = 'P'
generalCategoryLetter UnicodeSymbol       = 'S'
generalCategoryLetter UnicodeSeperator    = 'Z'
generalCategoryLetter UnicodeOther        = 'C'

-- | Major general category of character.
generalCategoryGroup :: Char -> GeneralCategoryGroup
generalCategoryGroup c =
    case Ch.generalCategory c of
      Ch.UppercaseLetter        -> UnicodeLetter
      Ch.LowercaseLetter        -> UnicodeLetter
      Ch.TitlecaseLetter        -> UnicodeLetter
      Ch.ModifierLetter         -> UnicodeLetter
      Ch.OtherLetter            -> UnicodeLetter

      Ch.NonSpacingMark         -> UnicodeMark
      Ch.SpacingCombiningMark   -> UnicodeMark
      Ch.EnclosingMark          -> UnicodeMark

      Ch.DecimalNumber          -> UnicodeNumber
      Ch.LetterNumber           -> UnicodeNumber
      Ch.OtherNumber            -> UnicodeNumber

      Ch.ConnectorPunctuation   -> UnicodePunctuation
      Ch.DashPunctuation        -> UnicodePunctuation
      Ch.OpenPunctuation        -> UnicodePunctuation
      Ch.ClosePunctuation       -> UnicodePunctuation
      Ch.InitialQuote           -> UnicodePunctuation
      Ch.FinalQuote             -> UnicodePunctuation
      Ch.OtherPunctuation       -> UnicodePunctuation

      Ch.MathSymbol             -> UnicodeSymbol
      Ch.CurrencySymbol         -> UnicodeSymbol
      Ch.ModifierSymbol         -> UnicodeSymbol
      Ch.OtherSymbol            -> UnicodeSymbol

      Ch.Space                  -> UnicodeSeperator
      Ch.LineSeparator          -> UnicodeSeperator
      Ch.ParagraphSeparator     -> UnicodeSeperator

      Ch.Control                -> UnicodeOther
      Ch.Format                 -> UnicodeOther
      Ch.Surrogate              -> UnicodeOther
      Ch.PrivateUse             -> UnicodeOther
      Ch.NotAssigned            -> UnicodeOther


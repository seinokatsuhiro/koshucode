{-# OPTIONS_GHC -Wall #-}

-- | Unicode general categories.

module Koshucode.Baala.Base.Text.Unicode
  ( MajorGeneralCategory (..),
    majorGeneralCategory,
    toMajorGeneralCategory,
    generalCategoryName,
    generalCategoryLetter,
    generalCategoryMajorLetter,
    generalCategoryMinorLetter,
  ) where

import qualified Data.Char as Ch

-- | Major category of the Unicode general categories.
data MajorGeneralCategory
    = UnicodeLetter          -- ^ Lu Ll Lt Lm Lo
    | UnicodeMark            -- ^ Mn Mc Me
    | UnicodeNumber          -- ^ Nd Nl No
    | UnicodePunctuation     -- ^ Pc Pd Ps Pe Pi Pf Po
    | UnicodeSymbol          -- ^ Sm Sc Sk So
    | UnicodeSeperator       -- ^ Zs Zl Zp
    | UnicodeOther           -- ^ Cc Cf Cs Co Cn
      deriving (Show, Eq, Ord, Bounded)

-- | Major general category of character.
majorGeneralCategory :: Char -> MajorGeneralCategory
majorGeneralCategory = toMajorGeneralCategory . Ch.generalCategory

-- | Convert general category to major category.
toMajorGeneralCategory :: Ch.GeneralCategory -> MajorGeneralCategory
toMajorGeneralCategory Ch.UppercaseLetter        = UnicodeLetter
toMajorGeneralCategory Ch.LowercaseLetter        = UnicodeLetter
toMajorGeneralCategory Ch.TitlecaseLetter        = UnicodeLetter
toMajorGeneralCategory Ch.ModifierLetter         = UnicodeLetter
toMajorGeneralCategory Ch.OtherLetter            = UnicodeLetter

toMajorGeneralCategory Ch.NonSpacingMark         = UnicodeMark
toMajorGeneralCategory Ch.SpacingCombiningMark   = UnicodeMark
toMajorGeneralCategory Ch.EnclosingMark          = UnicodeMark

toMajorGeneralCategory Ch.DecimalNumber          = UnicodeNumber
toMajorGeneralCategory Ch.LetterNumber           = UnicodeNumber
toMajorGeneralCategory Ch.OtherNumber            = UnicodeNumber

toMajorGeneralCategory Ch.ConnectorPunctuation   = UnicodePunctuation
toMajorGeneralCategory Ch.DashPunctuation        = UnicodePunctuation
toMajorGeneralCategory Ch.OpenPunctuation        = UnicodePunctuation
toMajorGeneralCategory Ch.ClosePunctuation       = UnicodePunctuation
toMajorGeneralCategory Ch.InitialQuote           = UnicodePunctuation
toMajorGeneralCategory Ch.FinalQuote             = UnicodePunctuation
toMajorGeneralCategory Ch.OtherPunctuation       = UnicodePunctuation

toMajorGeneralCategory Ch.MathSymbol             = UnicodeSymbol
toMajorGeneralCategory Ch.CurrencySymbol         = UnicodeSymbol
toMajorGeneralCategory Ch.ModifierSymbol         = UnicodeSymbol
toMajorGeneralCategory Ch.OtherSymbol            = UnicodeSymbol

toMajorGeneralCategory Ch.Space                  = UnicodeSeperator
toMajorGeneralCategory Ch.LineSeparator          = UnicodeSeperator
toMajorGeneralCategory Ch.ParagraphSeparator     = UnicodeSeperator

toMajorGeneralCategory Ch.Control                = UnicodeOther
toMajorGeneralCategory Ch.Format                 = UnicodeOther
toMajorGeneralCategory Ch.Surrogate              = UnicodeOther
toMajorGeneralCategory Ch.PrivateUse             = UnicodeOther
toMajorGeneralCategory Ch.NotAssigned            = UnicodeOther


-- --------------------------------------------  Name

-- | Name of general category.
--   This function returns one of @letter@, @mark@,
--   @number@, @punct@, @symbol@, @sep@, or @other@.
generalCategoryName :: MajorGeneralCategory -> String
generalCategoryName UnicodeLetter         = "letter"
generalCategoryName UnicodeMark           = "mark"
generalCategoryName UnicodeNumber         = "number"
generalCategoryName UnicodePunctuation    = "punct"
generalCategoryName UnicodeSymbol         = "symbol"
generalCategoryName UnicodeSeperator      = "sep"
generalCategoryName UnicodeOther          = "other"

-- | Major-minor category symbols like @Lu@.
generalCategoryLetter :: Ch.GeneralCategory -> String
generalCategoryLetter cat =
    [ generalCategoryMajorLetter $ toMajorGeneralCategory cat
    , generalCategoryMinorLetter cat]

-- | One-letter representation of general category.
--   This function returns one of @L@ (letter),
--   @M@ (mark), @N@ (number), @P@ (punctuation),
--   @S@ (symbol), @Z@ (seperator), or @C@ (other).
generalCategoryMajorLetter :: MajorGeneralCategory -> Char
generalCategoryMajorLetter UnicodeLetter             = 'L'
generalCategoryMajorLetter UnicodeMark               = 'M'
generalCategoryMajorLetter UnicodeNumber             = 'N'
generalCategoryMajorLetter UnicodePunctuation        = 'P'
generalCategoryMajorLetter UnicodeSymbol             = 'S'
generalCategoryMajorLetter UnicodeSeperator          = 'Z'
generalCategoryMajorLetter UnicodeOther              = 'C'

-- | One-letter representation of minor general category.
generalCategoryMinorLetter :: Ch.GeneralCategory -> Char
generalCategoryMinorLetter Ch.UppercaseLetter        = 'u'
generalCategoryMinorLetter Ch.LowercaseLetter        = 'l'
generalCategoryMinorLetter Ch.TitlecaseLetter        = 't'
generalCategoryMinorLetter Ch.ModifierLetter         = 'm'
generalCategoryMinorLetter Ch.OtherLetter            = 'o'

generalCategoryMinorLetter Ch.NonSpacingMark         = 'n'
generalCategoryMinorLetter Ch.SpacingCombiningMark   = 's'
generalCategoryMinorLetter Ch.EnclosingMark          = 'e'

generalCategoryMinorLetter Ch.DecimalNumber          = 'd'
generalCategoryMinorLetter Ch.LetterNumber           = 'l'
generalCategoryMinorLetter Ch.OtherNumber            = 'o'

generalCategoryMinorLetter Ch.ConnectorPunctuation   = 'c'
generalCategoryMinorLetter Ch.DashPunctuation        = 'd'
generalCategoryMinorLetter Ch.OpenPunctuation        = 's'
generalCategoryMinorLetter Ch.ClosePunctuation       = 'e'
generalCategoryMinorLetter Ch.InitialQuote           = 'i'
generalCategoryMinorLetter Ch.FinalQuote             = 'f'
generalCategoryMinorLetter Ch.OtherPunctuation       = 'o'

generalCategoryMinorLetter Ch.MathSymbol             = 'm'
generalCategoryMinorLetter Ch.CurrencySymbol         = 'c'
generalCategoryMinorLetter Ch.ModifierSymbol         = 'k'
generalCategoryMinorLetter Ch.OtherSymbol            = 'o'

generalCategoryMinorLetter Ch.Space                  = 's'
generalCategoryMinorLetter Ch.LineSeparator          = 'l'
generalCategoryMinorLetter Ch.ParagraphSeparator     = 'p'

generalCategoryMinorLetter Ch.Control                = 'c'
generalCategoryMinorLetter Ch.Format                 = 'f'
generalCategoryMinorLetter Ch.Surrogate              = 's'
generalCategoryMinorLetter Ch.PrivateUse             = 'o'
generalCategoryMinorLetter Ch.NotAssigned            = 'n'


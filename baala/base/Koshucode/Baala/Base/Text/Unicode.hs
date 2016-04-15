{-# OPTIONS_GHC -Wall #-}

-- | Unicode general categories.

module Koshucode.Baala.Base.Text.Unicode
  ( GeneralCategoryGroup (..),
    generalCategoryGroup,
    generalCategoryName,
    generalCategoryLetter,
    generalCategoryMajorLetter,
    generalCategoryMinorLetter,
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

-- | Major general category of character.
generalCategoryGroup :: Char -> GeneralCategoryGroup
generalCategoryGroup = toMajorCategory . Ch.generalCategory

-- | Convert general category to major category.
toMajorCategory :: Ch.GeneralCategory -> GeneralCategoryGroup
toMajorCategory Ch.UppercaseLetter        = UnicodeLetter
toMajorCategory Ch.LowercaseLetter        = UnicodeLetter
toMajorCategory Ch.TitlecaseLetter        = UnicodeLetter
toMajorCategory Ch.ModifierLetter         = UnicodeLetter
toMajorCategory Ch.OtherLetter            = UnicodeLetter

toMajorCategory Ch.NonSpacingMark         = UnicodeMark
toMajorCategory Ch.SpacingCombiningMark   = UnicodeMark
toMajorCategory Ch.EnclosingMark          = UnicodeMark

toMajorCategory Ch.DecimalNumber          = UnicodeNumber
toMajorCategory Ch.LetterNumber           = UnicodeNumber
toMajorCategory Ch.OtherNumber            = UnicodeNumber

toMajorCategory Ch.ConnectorPunctuation   = UnicodePunctuation
toMajorCategory Ch.DashPunctuation        = UnicodePunctuation
toMajorCategory Ch.OpenPunctuation        = UnicodePunctuation
toMajorCategory Ch.ClosePunctuation       = UnicodePunctuation
toMajorCategory Ch.InitialQuote           = UnicodePunctuation
toMajorCategory Ch.FinalQuote             = UnicodePunctuation
toMajorCategory Ch.OtherPunctuation       = UnicodePunctuation

toMajorCategory Ch.MathSymbol             = UnicodeSymbol
toMajorCategory Ch.CurrencySymbol         = UnicodeSymbol
toMajorCategory Ch.ModifierSymbol         = UnicodeSymbol
toMajorCategory Ch.OtherSymbol            = UnicodeSymbol

toMajorCategory Ch.Space                  = UnicodeSeperator
toMajorCategory Ch.LineSeparator          = UnicodeSeperator
toMajorCategory Ch.ParagraphSeparator     = UnicodeSeperator

toMajorCategory Ch.Control                = UnicodeOther
toMajorCategory Ch.Format                 = UnicodeOther
toMajorCategory Ch.Surrogate              = UnicodeOther
toMajorCategory Ch.PrivateUse             = UnicodeOther
toMajorCategory Ch.NotAssigned            = UnicodeOther


-- --------------------------------------------  Name

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

-- | Major-minor category symbols like @Lu@.
generalCategoryLetter :: Ch.GeneralCategory -> String
generalCategoryLetter cat =
    [ generalCategoryMajorLetter $ toMajorCategory cat
    , generalCategoryMinorLetter cat]

-- | One-letter representation of general category.
--   This function returns one of @L@ (letter),
--   @M@ (mark), @N@ (number), @P@ (punctuation),
--   @S@ (symbol), @Z@ (seperator), or @C@ (other).
generalCategoryMajorLetter :: GeneralCategoryGroup -> Char
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


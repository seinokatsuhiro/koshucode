{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Unicode
  ( GeneralCategoryGroup (..),
    generalCategoryLetter,
    generalCategoryName,
    generalCategoryGroup,
  ) where

import Data.Char

data GeneralCategoryGroup
    = UnicodeLetter          -- ^ Lu Ll Lt Lm Lo
    | UnicodeMark            -- ^ Mn Mc Me
    | UnicodeNumber          -- ^ Nd Nl No
    | UnicodePunctuation     -- ^ Pc Pd Ps Pe Pi Pf Po
    | UnicodeSymbol          -- ^ Sm Sc Sk So
    | UnicodeSeperator       -- ^ Zs Zl Zp
    | UnicodeOther           -- ^ Cc Cf Cs Co Cn
      deriving (Show, Eq, Ord, Bounded)

generalCategoryLetter :: GeneralCategoryGroup -> Char
generalCategoryLetter UnicodeLetter       = 'L'
generalCategoryLetter UnicodeMark         = 'M'
generalCategoryLetter UnicodeNumber       = 'N'
generalCategoryLetter UnicodePunctuation  = 'P'
generalCategoryLetter UnicodeSymbol       = 'S'
generalCategoryLetter UnicodeSeperator    = 'Z'
generalCategoryLetter UnicodeOther        = 'C'

generalCategoryName   :: GeneralCategoryGroup -> String
generalCategoryName   UnicodeLetter       = "letter"
generalCategoryName   UnicodeMark         = "mark"
generalCategoryName   UnicodeNumber       = "number"
generalCategoryName   UnicodePunctuation  = "punct"
generalCategoryName   UnicodeSymbol       = "symbol"
generalCategoryName   UnicodeSeperator    = "seperator"
generalCategoryName   UnicodeOther        = "other"

generalCategoryGroup :: Char -> GeneralCategoryGroup
generalCategoryGroup c =
    case generalCategory c of
      UppercaseLetter        -> UnicodeLetter
      LowercaseLetter        -> UnicodeLetter
      TitlecaseLetter        -> UnicodeLetter
      ModifierLetter         -> UnicodeLetter
      OtherLetter            -> UnicodeLetter

      NonSpacingMark         -> UnicodeMark
      SpacingCombiningMark   -> UnicodeMark
      EnclosingMark          -> UnicodeMark

      DecimalNumber          -> UnicodeNumber
      LetterNumber           -> UnicodeNumber
      OtherNumber            -> UnicodeNumber

      ConnectorPunctuation   -> UnicodePunctuation
      DashPunctuation        -> UnicodePunctuation
      OpenPunctuation        -> UnicodePunctuation
      ClosePunctuation       -> UnicodePunctuation
      InitialQuote           -> UnicodePunctuation
      FinalQuote             -> UnicodePunctuation
      OtherPunctuation       -> UnicodePunctuation

      MathSymbol             -> UnicodeSymbol
      CurrencySymbol         -> UnicodeSymbol
      ModifierSymbol         -> UnicodeSymbol
      OtherSymbol            -> UnicodeSymbol

      Space                  -> UnicodeSeperator
      LineSeparator          -> UnicodeSeperator
      ParagraphSeparator     -> UnicodeSeperator

      Control                -> UnicodeOther
      Format                 -> UnicodeOther
      Surrogate              -> UnicodeOther
      PrivateUse             -> UnicodeOther
      NotAssigned            -> UnicodeOther


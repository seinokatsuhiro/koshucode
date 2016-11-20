{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Syntax.TTree.Bracket
  ( -- * Bracket type
    BracketType (..),
    getBracketType,

    -- * Open and close
    groupOpen,  groupClose,
    openTerm,   closeTerm,
    listOpen,   listClose,
    setOpen,    setClose,
    tieOpen,    tieClose,
    relOpen,    relClose,
    interpOpen, interpClose,
    typeOpen,   typeClose,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Token          as S

-- | Type of bracket.
data BracketType
    = BracketGroup    -- ^ __1.__ Round brackets for grouping:
                      --   @( E ... )@
    | BracketTerm     -- ^ __2.__ Round-single brackets for term path:
                      --   @(- \/P ... -)@
    | BracketList     -- ^ __3.__ Square brackets for lists:
                      --   @[ C | ... ]@
    | BracketSet      -- ^ __4.__ Curely braces for sets:
                      --   @{ C | ... }@
    | BracketTie      -- ^ __5.__ Curely-single braces for ties:
                      --   @{- /N C ... -}@
    | BracketRel      -- ^ __6.__ Curely-double braces for relations:
                      --   @{= /N ... [ C | ... ][ C | ... ] =}@
    | BracketInterp   -- ^ __7.__ Curely-bar braces for data interpretation:
                      --   @{| ... /N ... |}@
    | BracketType     -- ^ __8.__ Square-single brackets for type:
                      --   @[- ... -]@
    | BracketForm     -- ^ __9.__ Round-bar brackets for calculation form:
                      --   @(| V ... | E ... |)@
    | BracketUnknown  -- ^ __10.__ Unknown bracket
      deriving (Show, Eq, Ord)

-- | Bracket type of token.
getBracketType :: B.GetBracketType BracketType S.Token
getBracketType = B.bracketTable
    [ o BracketGroup   groupOpen  groupClose
    , o BracketTerm    openTerm   closeTerm
    , o BracketList    listOpen   listClose
    , o BracketSet     setOpen    setClose
    , o BracketTie     tieOpen    tieClose
    , o BracketRel     relOpen    relClose
    , o BracketInterp  interpOpen interpClose
    , o BracketType    typeOpen   typeClose
    , o BracketForm    "(|"       "|)"
    , (BracketUnknown, S.isOpenToken, S.isCloseToken)
    ] where o t a b = (t, S.isOpenTokenOf a, S.isCloseTokenOf b)

-- | Open group: @"("@
groupOpen :: String
groupOpen = "("

-- | Close group: @")"@
groupClose :: String
groupClose = ")"

-- | Open term path: @"(-"@
openTerm :: String
openTerm = "(-"

-- | Close term path: @"-)"@
closeTerm :: String
closeTerm = "-)"

-- | Open list: @"["@
listOpen :: String
listOpen = "["

-- | Close list: @"]"@
listClose :: String
listClose = "]"

-- | Open set: @"{"@
setOpen :: String
setOpen = "{"

-- | Close set: @"}"@
setClose :: String
setClose = "}"

-- | Open tie: @"{-"@
tieOpen :: String
tieOpen = "{-"

-- | Close tie: @"-}"@
tieClose :: String
tieClose = "-}"

-- | Open relation: @"{="@
relOpen :: String
relOpen = "{="

-- | Close relation: @"=}"@
relClose :: String
relClose = "=}"

-- | Open interpreation: @"{|"@
interpOpen :: String
interpOpen = "{|"

-- | Close interpreation: @"|}"@
interpClose :: String
interpClose = "|}"

-- | Open type: @"[-"@
typeOpen :: String
typeOpen = "[-"

-- | Close type: @"-]"@
typeClose :: String
typeClose = "-]"


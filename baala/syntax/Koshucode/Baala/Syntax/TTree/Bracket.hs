{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Syntax.TTree.Bracket
  ( -- * Bracket type
    BracketType (..),
    getBracketType,
    -- * Open and close
    groupOpen,  groupClose,
    listOpen,   listClose,
    setOpen,    setClose,
    tieOpen,    tieClose,
    relOpen,    relClose,
    interpOpen, interpClose,
    typeOpen,   typeClose,
  ) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax.Token.Token   as S

-- | Type of bracket.
data BracketType
    = BracketGroup    -- ^ Round brackets for grouping: @( E ... )@
    | BracketList     -- ^ Square brackets for lists: @[ C | ... ]@
    | BracketSet      -- ^ Curely braces for sets: @{ C | .... }@
    | BracketTie      -- ^ Curely-single braces for ties: @{- /N C ... -}@
    | BracketRel      -- ^ Curely-double braces for relations: @{= /N ... [ C | ... ][ C | ... ] =}@
    | BracketInterp   -- ^ Curely-bar braces for data interpretation: @{| ... /N ... |}@
    | BracketType     -- ^ Square-single brackets for type: @[- ... -]@
    | BracketForm     -- ^ Round-bar brackets for form with blanks: @(| V ... | E ... |)@
    | BracketUnknown  -- ^ Unknown bracket
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Bracket type of token.
getBracketType :: B.GetBracketType BracketType S.Token
getBracketType = B.bracketTable
    [ o BracketGroup   groupOpen  groupClose
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


{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Syntax.TTree.Bracket
  ( -- * Bracket type
    BracketType (..),
    getBracketType,

    -- * Open and close
    openGroup,  closeGroup,
    openTerm,   closeTerm,
    openList,   closeList,
    openSet,    closeSet,
    openTie,    closeTie,
    openRel,    closeRel,
    openInterp, closeInterp,
    openType,   closeType,
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
    [ o BracketGroup   openGroup  closeGroup
    , o BracketTerm    openTerm   closeTerm
    , o BracketList    openList   closeList
    , o BracketSet     openSet    closeSet
    , o BracketTie     openTie    closeTie
    , o BracketRel     openRel    closeRel
    , o BracketInterp  openInterp closeInterp
    , o BracketType    openType   closeType
    , o BracketForm    "(|"       "|)"
    , (BracketUnknown, S.isOpenToken, S.isCloseToken)
    ] where o t a b = (t, S.isOpenTokenOf a, S.isCloseTokenOf b)

-- | Open group: @"("@
openGroup :: String
openGroup = "("

-- | Close group: @")"@
closeGroup :: String
closeGroup = ")"

-- | Open term path: @"(-"@
openTerm :: String
openTerm = "(-"

-- | Close term path: @"-)"@
closeTerm :: String
closeTerm = "-)"

-- | Open list: @"["@
openList :: String
openList = "["

-- | Close list: @"]"@
closeList :: String
closeList = "]"

-- | Open set: @"{"@
openSet :: String
openSet = "{"

-- | Close set: @"}"@
closeSet :: String
closeSet = "}"

-- | Open tie: @"{-"@
openTie :: String
openTie = "{-"

-- | Close tie: @"-}"@
closeTie :: String
closeTie = "-}"

-- | Open relation: @"{="@
openRel :: String
openRel = "{="

-- | Close relation: @"=}"@
closeRel :: String
closeRel = "=}"

-- | Open interpreation: @"{|"@
openInterp :: String
openInterp = "{|"

-- | Close interpreation: @"|}"@
closeInterp :: String
closeInterp = "|}"

-- | Open type: @"[-"@
openType :: String
openType = "[-"

-- | Close type: @"-]"@
closeType :: String
closeType = "-]"


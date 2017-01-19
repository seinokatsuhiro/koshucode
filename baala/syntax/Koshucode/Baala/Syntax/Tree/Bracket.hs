{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Syntax.Tree.Bracket
  ( -- * Bracket type
    BracketType (..),
    getBracketType,

    -- * Open and close
    bracketWith,
    bracketGroup,
    bracketTerm,
    bracketList,
    bracketSet,
    bracketTie,
    bracketRel,
    bracketInterp,
    bracketType,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Token          as S

-- | Type of bracket.
data BracketType
    = BracketGroup    -- ^ __1.__ Round brackets for grouping:
                      --
                      -- > ( E ... )

    | BracketTerm     -- ^ __2.__ Round-single brackets for term path:
                      --
                      -- > (- /N ... -)

    | BracketList     -- ^ __3.__ Square brackets for lists:
                      --
                      -- > [ C | ... ]

    | BracketSet      -- ^ __4.__ Curely braces for sets:
                      --
                      -- > { C | ... }

    | BracketTie      -- ^ __5.__ Curely-single braces for ties:
                      --
                      -- > {- /N C ... -}

    | BracketRel      -- ^ __6.__ Curely-double braces for relations:
                      --
                      -- > {= /N ... [ C | ... ][ C | ... ] =}

    | BracketInterp   -- ^ __7.__ Curely-bar braces for data interpretation:
                      --
                      -- > {| ... /N ... |}

    | BracketType     -- ^ __8.__ Square-single brackets for type:
                      --
                      -- > [- ... -]

    | BracketForm     -- ^ __9.__ Round-bar brackets for calculation form:
                      --
                      -- > (| V ... | E ... |)

    | BracketUnknown  -- ^ __10.__ Unknown bracket
      deriving (Show, Eq, Ord)

-- | Lower case name of bracket type.
--
--   >>> B.name BracketList
--   "list"
--
instance B.Name BracketType where
    name BracketGroup    = "group"
    name BracketTerm     = "term"
    name BracketList     = "list"
    name BracketSet      = "set"
    name BracketTie      = "tie"
    name BracketRel      = "rel"
    name BracketInterp   = "interp"
    name BracketType     = "type"
    name BracketForm     = "form"
    name BracketUnknown  = "unknown"

-- | Bracket type of token.
getBracketType :: (Eq t, B.IsString t) => B.GetBracket BracketType (S.TToken t)
getBracketType = B.bracketTable
    [ BracketGroup   # bracketGroup
    , BracketTerm    # bracketTerm
    , BracketList    # bracketList
    , BracketSet     # bracketSet
    , BracketTie     # bracketTie
    , BracketRel     # bracketRel
    , BracketInterp  # bracketInterp
    , BracketType    # bracketType
    , BracketForm    # bracketForm
    , (BracketUnknown, (S.isOpenToken, S.isCloseToken))
    ] where br # p = (br, S.isBracketTokenOf p)

-- | Enclose mix text in brackets.
bracketWith :: (B.Bs, B.Bs) -> B.MixText -> B.MixText
bracketWith = B.mixBracketS

-- | Group bracket — @"("@ and @")"@
bracketGroup :: (B.IsString t) => (t, t)
bracketGroup = ( "(", ")" )

-- | Term bracket — @"(-"@ and @"-)"@
bracketTerm :: (B.IsString t) => (t, t)
bracketTerm = ( "(-", "-)" )

-- | List bracket — @"["@ and @"]"@
bracketList :: (B.IsString t) => (t, t)
bracketList = ( "[", "]" )

-- | Set bracket — @"{"@ and @"}"@
bracketSet :: (B.IsString t) => (t, t)
bracketSet = ( "{", "}" )

-- | Tie bracket — @"{-"@ and @"-}"@
bracketTie :: (B.IsString t) => (t, t)
bracketTie = ( "{-", "-}" )

-- | Relation bracket — @"{="@ and @"=}"@
bracketRel :: (B.IsString t) => (t, t)
bracketRel = ( "{=", "=}" )

-- | Interpretation bracket — @"{|"@ and @"|}"@
bracketInterp :: (B.IsString t) => (t, t)
bracketInterp = ( "{|", "|}" )

-- | Type bracket — @"[-"@ and @"-]"@
bracketType :: (B.IsString t) => (t, t)
bracketType = ( "[-", "-]" )

-- | Form bracket — @"(|"@ and @"|)"@
bracketForm :: (B.IsString t) => (t, t)
bracketForm = ( "(|", "|)" )


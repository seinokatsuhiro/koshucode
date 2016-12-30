{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Base.Code.Bracket
  ( -- * Bracket
    Bracket (..),
    GetBracketType,
    bracketTable,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base.Abort            as B
import qualified Koshucode.Baala.Base.List             as B
import qualified Koshucode.Baala.Base.Code.Message     as Msg


-- ============================================  Bracket

-- | Bracket type.
data Bracket b
    = BracketNone       -- ^ None bracket
    | BracketOpen  b    -- ^ Open bracket
    | BracketClose b    -- ^ Close bracket
      deriving (Show, Eq, Ord)

-- ----------------------  Bracket table

-- | Get a bracket type.
type GetBracketType b a = a -> Bracket b

-- | Create 'GetBracketType' functions
-- from a type-open-close table.
--
-- /Example/
--
-- Create bracket/type functions from @()@ and @[]@.
--
--   >>> let bracket n [a, b] = (n, ((== a), (== b)))
--   >>> let pt = bracketTable [ bracket 1 "()", bracket 2 "[]" ]
--
-- Get bracket types for each chars.
-- Types of open brackets are positive integer,
-- and closes are negative.
--
--   >>> map pt "ab(cd[ef])g"
--   [BracketNone, BracketNone,
--    BracketOpen 1, BracketNone, BracketNone,
--     BracketOpen 2, BracketNone, BracketNone, BracketClose 2,
--    BracketClose 1, BracketNone]
--
bracketTable
    :: (Eq a)
    => [(b, (O.Test a, O.Test a))] -- ^ List of (/type/, (/open/, /close/))
    -> GetBracketType b a
bracketTable xs = bracketType where
    bracketTypeTable = map bracketOpen xs ++ map bracketClose xs
    bracketOpen  (n, (isOpen, _))  = (isOpen,  BracketOpen n)
    bracketClose (n, (_, isClose)) = (isClose, BracketClose n)
    bracketType a =
        case B.lookupSatisfy a bracketTypeTable of
          Just p  -> p
          Nothing -> BracketNone


{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Bracket type.

module Koshucode.Baala.Base.Code.Bracket
  ( -- * Bracket
    Bracket (..),
    GetBracketType,
    bracketTable,

    -- * Indent
    indentBranch,
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


-- ============================================  Indent

-- | Indent branching function.
--   This inserts open\/separator\/close elements by following indent.
--
--   >>> let size s = if dropWhile (== '/') s == "" then Just (length s) else Nothing
--   >>> let test s = s `elem` [">", "-"]
--   >>> let toks s = takeWhile (== '/') s : words (dropWhile (== '/') s)
--   >>> let text = Right . unwords . concat
--   >>> let conv ls = text =<< indentBranch size test "(" "|" ")" (toks <$> ls)
--
--   >>> toks "/> bb"
--   ["/",">","bb"]
--
--   >>> conv ["> aa", "/> bb", "//> cc"]
--   Right "> aa ( > bb ( > cc ) )"
--
--   >>> conv ["> aa", "/> bb", "/> cc"]
--   Right "> aa ( > bb | > cc )"
--
--   >>> conv ["> aa", "/> bb", "//- cc", "///cc", "//- dd", "//- ee", "/- ff"]
--   Right "> aa ( > bb ( - cc cc | - dd | - ee ) | - ff )"
--
indentBranch
    :: (a -> Maybe Int)  -- ^ Function getting indent size
    -> O.Test a          -- ^ Testing indent element
    -> a                 -- ^ Open element
    -> a                 -- ^ Separator element
    -> a                 -- ^ Close element
    -> [[a]]             -- ^ Code elements indented
    -> B.Ab [[a]]        -- ^ Code Elements with open\/separator\/close elements
indentBranch size indent open sep close = line 0 0 where
    line lv i (((size -> Just i') : xs@(k : _)) : ls)
         | not $ indent k = next lv       i           xs  ls
         | i' == 0        = next lv       i           xs  ls
         | i' >  i        = next (lv + 1) i' (open  : xs) ls
         | i' <  i        = next (lv - 1) i' (close : sep : xs) ls
         | i' == i        = next lv       i' (sep   : xs) ls
    line lv i (xs : ls)   = next lv i xs ls
    line lv _ []
        | lv == 0         = Right []
        | lv >  0         = Right [replicate lv close]
        | otherwise       = Msg.extraCloseBracketInserted

    next lv' i' xs' ls    = do ls' <- line lv' i' ls
                               Right $ xs' : ls'

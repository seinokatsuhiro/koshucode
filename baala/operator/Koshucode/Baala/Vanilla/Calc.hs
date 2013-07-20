{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Calc
( makeCalc
, makeHeadCalcs
) where
import Koshucode.Baala.Vanilla.Calc.Ripen
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Minimal.OpKit as Kit

-- 1. string
-- 2. list of token
-- 3. tree (include binary operators)
-- 4. tree (no binary operators)
-- 5. formula (include named vars)
-- 6. list of name-and-formula (include named vars)
-- 7. list of name-and-formula (no named vars, but include positional vars)
-- 8. list of name-and-formula (no positional vars, actual values filled)
-- 9. list of name-and-value

-- | Prepare calculation.
--   @makeCalc@ /H E/ is a calculator from
--   an expression /E/ on a head /H/.
--   Terms in /E/ must be terms in /H/.
--   If not in /H/, an error is reported.
makeCalc
    :: Relhead    -- ^ Heading of relation
    -> TokenTree  -- ^ Expression
    -> Calc VContent   -- ^ Calculator
makeCalc h = calc h . vanillaBinary

-- | Prepare named calculators
makeHeadCalcs
    :: Relhead                -- ^ Heading of relation
    -> [TokenTree]            -- ^ List of names and expressions
    -> (Relhead, [Calc VContent])  -- ^ Heading of expressions and its calculators
makeHeadCalcs head1 tree1 = (Relhead head2, calc2) where
    u (Right ps)   = unzip ps
    u (Left _)     = error $ "expect name-expr pairs" ++ show tree1
    (names2, trees2) = u (termTreePairs $ map vanillaBinary tree1)
    head2          = map Term names2
    calc2          = map (calc head1) trees2

calc :: Relhead -> TokenTree -> Calc VContent
calc h = crop calcRipen . mapPosition h

mapPosition :: Relhead -> TokenTree -> TokenTree
mapPosition h = fmap p where
    p (TTermN n ns) = TTermP n $ termLook1 ns (headTerms h)
    p x = x


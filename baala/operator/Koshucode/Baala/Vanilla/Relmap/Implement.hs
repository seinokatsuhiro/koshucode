{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Implement
( vanillaRelmaps
, relMaybe

, hang
, relHang
, selfhang
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  Operators

-- | Implementation of relational operators.
vanillaRelmaps :: [Kit.OpImplement Val]
vanillaRelmaps = Mini.minimalRelmaps ++ vanillaRelmapsAddition

vanillaRelmapsAddition :: [Kit.OpImplement Val]
vanillaRelmapsAddition = Mini.relmaps
    -- Relmap operators in alphabetical order
    [ o "hold"    LikeHold    consHold
    , o "maybe"   LikeMeet    consMaybe
    , o "mmaybe"  LikeMeet    consMMaybe
    , o "rdf"     LikeSource  consRdf
    , o "unhold"  LikeHold    consUnhold
    , o "val"     LikeVal     consVal
    ] where o = (,,)



-- ----------------------  Constructors

consHold :: Kit.OpCons Val
consHold = consHoldFor "hold" (==)

consUnhold :: Kit.OpCons Val
consUnhold = consHoldFor "unhold" (/=)

consHoldFor :: String -> (Val -> Val -> Bool) -> Kit.OpCons Val
consHoldFor op test use = do
  let h = Kit.opHalf use
      opd = Kit.halfOperand h
  term <- opd <!!> "-term"
  Right $ Kit.relmapCalc use op (holdBody test $ TreeB 0 term)

consRdf :: Kit.OpCons Val
consRdf use = do
  let h = Kit.opHalf use
  sign  <- Mini.getWord use "-sign"
  --term  <- opd <!!> "-term"
  [s,o] <- Mini.getTerms use "-term"
  Right $ Kit.RelmapAlias h $
        Kit.relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]

consVal :: Kit.OpCons Val
consVal use = do
  let h = Kit.opHalf use
      opd = Kit.halfOperand h
  term <- opd <!!> "-term"
  Right $ Kit.relmapCalc use "val" (valBody term)



-- ----------------------  Maybe and MMaybe

consMaybe :: Kit.OpCons Val
consMaybe use = Right $ relmapMaybe use

consMMaybe :: Kit.OpCons Val
consMMaybe use = Right $ relmapMMaybe use

-- | like SQL's left join
relmapMaybe :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybe use = Kit.relmapConfl use "maybe" sub ms where
    ms = Kit.opSub use
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = undefined

-- | like SQL's full join
relmapMMaybe :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMMaybe use = Kit.relmapConfl use "mmaybe" sub ms where
    ms = Kit.opSub use
    sub [r2] r1 = Mini.relJoin (relMaybe r1 r2) (relMaybe r2 r1)
    sub _ _     = undefined

-- | like SQL's left join
relMaybe :: (Ord v, Nil v) => Rel v -> Rel v -> Rel v
relMaybe r1 r2 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (headDegree h3 - headDegree h1) nil
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]



-- ----------------------  Nested relation

-- | Hanging relation, like grouping.
hang :: (Ord v, RelValue v)
    => Kit.OpUse v -> String
    -> Kit.Relmap v -> Kit.Relmap v
hang use ns rm2 = Kit.relmapConfl use "hang" (Kit.withP2 relHang ns) [rm2]

relHang :: (Ord v, RelValue v)
    => [String] -> [Rel v] -> Rel v -> Rel v
relHang [n] [r2] r1 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    --side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, arg2)

    h3 = Relhead $ Nest n (headTerms h2) : (headTerms h1)
    b3 = map step args1
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just args2' -> (relValue $ Rel h2 args2') : arg1
                  Nothing     -> []

relHang _ _ _ = undefined

selfhang :: a
selfhang = undefined


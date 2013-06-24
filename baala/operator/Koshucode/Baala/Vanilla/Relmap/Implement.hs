{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Implement
( vanillaOperators

-- * Operators
-- $Operators
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Relmap.Calc
import Koshucode.Baala.Vanilla.Relmap.Naming
import Koshucode.Baala.Vanilla.Relmap.Operand
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  Operators

-- | Implementation of relational operators.
vanillaOperators :: [Kit.OpImplement Val]
vanillaOperators = vanillaOperators' ++ Mini.minimalOperators

vanillaOperators' :: [Kit.OpImplement Val]
vanillaOperators' = Mini.operators
    -- Relmap operators in alphabetical order
    [ o "conf"           LikeSize          consConf
    , o "enclose"        LikeSize          consEnclose
    , o "hang"           LikeMeet          consHang
    , o "hold"           LikeHold          consHold
    , o "maybe"          LikeMeet          consMaybe
    , o "maybe-both"     LikeMeet          consMaybeBoth
    , o "prefix"         LikePrefix        consPrefix
    , o "prefix-change"  LikePrefixChange  consPrefixChange
    , o "rdf"            LikeSource        consRdf
    , o "size"           LikeSize          consSize
    , o "unhold"         LikeHold          consUnhold
    , o "unprefix"       LikeUnprefix      consUnprefix
    , o "val"            LikeVal           consVal
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
  sign  <- Mini.getWord  use "-sign"
  [s,o] <- Mini.getTerms use "-term"
  Right $ Kit.relmapAlias use $
        Kit.relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]

consVal :: Kit.OpCons Val
consVal use = do
  let h = Kit.opHalf use
      opd = Kit.halfOperand h
  term <- opd <!!> "-term"
  Right $ Kit.relmapCalc use "val" (valBody term)



-- ----------------------  maybe

consMaybe :: Kit.OpCons Val
consMaybe use = Right $ relmapMaybe use

relmapMaybe :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybe use = Kit.relmapConfl use "maybe" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = relMaybe r1 r2
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



-- ----------------------  maybe-both

consMaybeBoth :: Kit.OpCons Val
consMaybeBoth use = Right $ relmapMaybeBoth use

-- | like SQL's full join
relmapMaybeBoth :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybeBoth use = Kit.relmapConfl use "mmaybe" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = Mini.relJoin (relMaybe r1 r2) (relMaybe r2 r1)
    sub _ _     = undefined



-- ----------------------  Nested relation

consHang :: Kit.OpCons Val
consHang use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapHang use n

relmapHang :: (Ord v, RelValue v) => OpUse v -> String -> Relmap v
relmapHang use n = Kit.relmapConfl use "hang" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = relHang n r2 r1
    sub _ _     = undefined

-- | Hanging relation, like grouping.
relHang :: (Ord v, RelValue v) => String -> Rel v -> Map (Rel v)
relHang n r2 r1 = Rel h3 b3 where
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



-- ----------------------  Naming

consPrefix :: Kit.OpCons Val
consPrefix use = do
  pre <- Mini.getTerm use "-prefix"
  ns  <- Mini.getTerms use "-term"
  Right $ relmapPrefix use pre ns

consUnprefix :: Kit.OpCons Val
consUnprefix use = do
  pre <- Mini.getTerm use "-prefix"
  Right $ relmapUnprefix use pre

consPrefixChange :: Kit.OpCons Val
consPrefixChange use = do
  new <- Mini.getTerm use "-new"
  old <- Mini.getTerm use "-old"
  Right $ relmapPrefixChange use new old



-- ----------------------  size

consSize :: Kit.OpCons Val
consSize use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapSize use n

relmapSize :: (IntValue v) => OpUse v -> String -> Relmap v
relmapSize use n = Kit.relmapCalc use "size" sub where
    sub _ = relSize n

{-| Change terms names -}
relSize
    :: (IntValue v)
    => String        -- ^ List of term name (/to/, /from/)
    -> Map (Rel v)   -- ^ Relation to relation
relSize n (Rel _ b1) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[intValue $ length b1]]



-- ----------------------  conf

consConf :: Kit.OpCons Val
consConf use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapConf use n

relmapConf :: (StringValue v) => OpUse v -> String -> Relmap v
relmapConf use n = Kit.relmapCalc use "conf" sub where
    sub _ = relConf n

{-| Change terms names -}
relConf
    :: (StringValue v)
    => String        -- ^ Term name
    -> Map (Rel v)   -- ^ Relation to relation
relConf n (Rel h1 _) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[stringValue $ show s]]
    s  = show $ docParen $ doc h1



-- ----------------------  enclose

consEnclose :: Kit.OpCons Val
consEnclose use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapEnclose use n

relmapEnclose :: (RelValue v) => OpUse v -> String -> Relmap v
relmapEnclose use n = Kit.relmapCalc use "enclose" sub where
    sub _ = relEnclose n

{-| Enclose the current relation in a term. -}
relEnclose
    :: (RelValue v)
    => String        -- ^ Term name
    -> Map (Rel v)   -- ^ Relation to relation
relEnclose n r@(Rel h1 _) = Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[relValue r]]



-- ----------------------
-- $Operators
--
-- [@conf@]
--
-- [@enclose@]
--
-- [@hang@]
--
-- [@hold@]
--
-- [@maybe@]
--
-- [@maybe-both@]
--
-- [@prefix@]
--
-- [@prefix-change@]
--
-- [@rdf@]
--
-- [@size@]
--
-- [@unhold@]
--
-- [@unprefix@]
--
-- [@val@]


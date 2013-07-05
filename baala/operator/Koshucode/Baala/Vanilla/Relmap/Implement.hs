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

{-| Implementation of relational operators. -}
vanillaOperators :: [Kit.OpImplement Val]
vanillaOperators = vanillaOperators' ++ Mini.minimalOperators

vanillaOperators' :: [Kit.OpImplement Val]
vanillaOperators' = Mini.operators
    -- Relmap operators in alphabetical order
    [ o "conf"           LikeSize          relopConf
    , o "enclose"        LikeSize          relopEnclose
    , o "hang"           LikeMeet          relopHang
    , o "hold"           LikeHold          relopHold
    , o "maybe"          LikeMeet          relopMaybe
    , o "maybe-both"     LikeMeet          relopMaybeBoth
    , o "prefix"         LikePrefix        relopPrefix
    , o "prefix-change"  LikePrefixChange  relopPrefixChange
    , o "range"          LikeSize          relopRange
    , o "rdf"            LikeSource        relopRdf
    , o "size"           LikeSize          relopSize
    , o "unhold"         LikeHold          relopUnhold
    , o "unprefix"       LikeUnprefix      relopUnprefix
    , o "val"            LikeVal           relopVal
    ] where o = (,,)



-- ----------------------  Constructors

relopRdf :: Kit.Relop Val
relopRdf use = do
  sign  <- Mini.getWord  use "-sign"
  [s,o] <- Mini.getTerms use "-term"
  Right $ Kit.relmapAlias use $
        Kit.relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]



-- ----------------------  maybe

relopMaybe :: Kit.Relop Val
relopMaybe use = Right $ relmapMaybe use

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

relopMaybeBoth :: Kit.Relop Val
relopMaybeBoth use = Right $ relmapMaybeBoth use

-- | like SQL's full join
relmapMaybeBoth :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybeBoth use = Kit.relmapConfl use "mmaybe" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = Mini.relJoin (relMaybe r1 r2) (relMaybe r2 r1)
    sub _ _     = undefined



-- ----------------------  Nested relation

relopHang :: Kit.Relop Val
relopHang use = do
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



-- ----------------------  size

relopSize :: Kit.Relop Val
relopSize use = do
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

relopConf :: Kit.Relop Val
relopConf use = do
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

relopEnclose :: Kit.Relop Val
relopEnclose use = do
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


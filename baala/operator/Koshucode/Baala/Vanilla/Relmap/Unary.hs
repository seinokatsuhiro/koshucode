{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  relopSize, relmapSize, relSize,
  -- * conf
  relopConf, relmapConf, relConf,
  -- * enclose
  relopEnclose, relmapEnclose, relEnclose,
  -- * rank
  relopRank, relmapRank, relRank, limit,
  -- * typename
  relopTypename, relmapTypename, relTypename,
  -- * range
  relopRange, relmapRange,
) where

import qualified Data.List as List

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Order as Kit



-- ----------------------  size

relopSize :: Kit.Relop VContent
relopSize use = do
  n <- Kit.getTerm use "-term"
  Right $ relmapSize use n

relmapSize :: (CInt v) => OpUse v -> String -> Relmap v
relmapSize use n = Kit.relmapCalc use "size" sub where
    sub _ = relSize n

{-| Change terms names -}
relSize
    :: (CInt v)
    => String          -- ^ List of term name (/to/, /from/)
    -> AbMap (Rel v)   -- ^ Relation to relation
relSize n (Rel _ b1) = Right $ Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[putInt $ length b1]]



-- ----------------------  conf

relopConf :: Kit.Relop VContent
relopConf use = do
  n <- Kit.getTerm use "-term"
  Right $ relmapConf use n

relmapConf :: (CText v) => OpUse v -> String -> Relmap v
relmapConf use n = Kit.relmapCalc use "conf" sub where
    sub _ = relConf n

{-| Change terms names -}
relConf
    :: (CText v)
    => String          -- ^ Term name
    -> AbMap (Rel v)   -- ^ Relation to relation
relConf n (Rel h1 _) = Right $ Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[putText $ show s]]
    s  = show $ docParen $ doc h1



-- ----------------------  enclose

relopEnclose :: Kit.Relop VContent
relopEnclose use = do
  n <- Kit.getTerm use "-term"
  Right $ relmapEnclose use n

relmapEnclose :: (CRel v) => OpUse v -> String -> Relmap v
relmapEnclose use n = Kit.relmapCalc use "enclose" sub where
    sub _ = relEnclose n

{-| Enclose the current relation in a term. -}
relEnclose
    :: (CRel v)
    => String          -- ^ Term name
    -> AbMap (Rel v)   -- ^ Relation to relation
relEnclose n r@(Rel h1 _) = Right $ Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[putRel r]]



-- ----------------------  rank

relopRank :: Kit.Relop VContent
relopRank use =
    do n  <- Kit.getTerm  use "-add"
       ns <- Kit.getTerms use "-order"
       Right $ relmapRank use n ns

relmapRank :: (CInt c, Ord c) => OpUse c -> String -> [String] -> Relmap c
relmapRank use n ns = Kit.relmapCalc use "rank" sub where
    sub _ = relRank n ns

relRank :: (CInt c, Ord c) => String -> [String] -> AbMap (Rel c)
relRank n ns (Rel h1 b1) = Right $ Rel h2 b2 where
    h2   = headFrom [n] `mappend` h1
    b2   = zipWith (:) (map putInt [1..]) b1'
    b1'  = Kit.sortByName ords (headNames h1) b1
    ords = map Kit.Asc ns

-- | Keep leading tuples.
limit :: (Ord v) => Kit.OpUse v -> Int -> String -> Kit.Relmap v
limit use c ns = Kit.relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord v) => Int -> String -> a -> AbMap (Rel v)
limit2 c ns _ (Rel h1 b1) = Right $ Rel h1 b2 where
    b2   = List.take c $ Kit.sortByName ords (headNames h1) b1
    ords = Kit.orders ns



-- ----------------------  typename

relopTypename :: (CContent c) => Relop c
relopTypename use = do
  (n, p) <- Kit.getTermPair use "-term"
  Right $ relmapTypename use n p

relmapTypename :: (CContent c) => OpUse c -> String -> String -> Relmap c
relmapTypename use n p = Kit.relmapCalc use "typename" sub where
    sub _ = relTypename n p

{-| Get typename. -}
relTypename
    :: (CContent c)
    => String
    -> String
    -> AbMap (Rel c)
relTypename n p (Rel h1 b1) = Right $ Rel h2 b2 where
    h2 = Kit.headFrom [n] `mappend` h1
    b2 = map f b1
    pos = h1 `posOf` [[p]]
    f cs1 = let [c] = csPick pos cs1
            in putText (typename c) : cs1



-- ----------------------  range

relopRange :: Kit.Relop VContent
relopRange use = do
  term <- Kit.getTerm use "-term"
  low  <- Kit.getInt  use "-from"
  high <- Kit.getInt  use "-to"
  Right $ relmapRange use term low high

relmapRange :: (CInt v) => OpUse v -> String -> Int -> Int -> Relmap v
relmapRange use n low high = Kit.relmapCalc use "range" sub where
    sub _ r1 = relRange n low high r1

relRange :: (CInt v) => String -> Int -> Int -> AbMap (Rel v)
relRange n low high (Rel h1 b1) = Right $ Rel h2 b2 where
    h2   = Kit.mappend (Kit.headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map putInt [low .. high]
    g xs = map (: xs) ys

{-
divide :: String -> Kit.Relmap VContent
divide ns2 = Kit.flow "divide" $ Kit.withP divide2 ns2

divide2 :: [String] -> Rel VContent -> Rel VContent
divide2 ns2 (Rel h1 b1) = Rel h2 b2 where
    h2  = Kit.mappend (Kit.headFrom ns2) h1
    b2  = concatMap g b1

    p = ns2 `Kit.look` names (headTerms h1)
    g | p `Kit.like` "--vv"  = Kit.ap f
      | otherwise          = const []

    [_,_,x,y] = p
    f the arg = if the y == putInt 0
                then []
                else [binv quot (the x) (the y) :
                      binv rem  (the x) (the y) : arg]
-}


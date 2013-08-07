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
  -- * RDF
  relopRdf
) where

import qualified Data.List as List
import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Builtin
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Order
import qualified Koshucode.Baala.Builtin as Kit
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  size

relopSize :: RopCons VContent
relopSize use = do
  n <- getTerm use "-term"
  Right $ relmapSize use n

relmapSize :: (CDec c) => RopUse c -> String -> Relmap c
relmapSize use n = relmapCalc use "size" sub where
    sub _ = relSize n

{-| Change terms names -}
relSize
    :: (CDec c)
    => String          -- ^ List of term name (/to/, /from/)
    -> AbMap (Rel c)   -- ^ Relation to relation
relSize n (Rel _ b1) = Right $ Rel h2 b2 where
    h2 = headFrom [n]
    b2 = [[putDecFromInt $ length b1]]



-- ----------------------  conf

relopConf :: RopCons VContent
relopConf use = do
  n <- getTerm use "-term"
  Right $ relmapConf use n

relmapConf :: (CText c) => RopUse c -> String -> Relmap c
relmapConf use n = relmapCalc use "conf" sub where
    sub _ = relConf n

{-| Change terms names -}
relConf
    :: (CText c)
    => String          -- ^ Term name
    -> AbMap (Rel c)   -- ^ Relation to relation
relConf n (Rel h1 _) = Right $ Rel h2 b2 where
    h2 = headFrom [n]
    b2 = [[putText $ show s]]
    s  = show $ docParen $ doc h1



-- ----------------------  enclose

relopEnclose :: RopCons VContent
relopEnclose use = do
  n <- getTerm use "-term"
  Right $ relmapEnclose use n

relmapEnclose :: (CRel c) => RopUse c -> String -> Relmap c
relmapEnclose use n = relmapCalc use "enclose" sub where
    sub _ = relEnclose n

{-| Enclose the current relation in a term. -}
relEnclose
    :: (CRel c)
    => String          -- ^ Term name
    -> AbMap (Rel c)   -- ^ Relation to relation
relEnclose n r@(Rel h1 _) = Right $ Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[putRel r]]



-- ----------------------  rank

relopRank :: RopCons VContent
relopRank use =
    do n  <- getTerm  use "-add"
       ns <- getTerms use "-order"
       Right $ relmapRank use n ns

relmapRank :: (CDec c, Ord c) => RopUse c -> String -> [String] -> Relmap c
relmapRank use n ns = relmapCalc use "rank" sub where
    sub _ = relRank n ns

relRank :: (CDec c, Ord c) => String -> [String] -> AbMap (Rel c)
relRank n ns (Rel h1 b1) = Right $ Rel h2 b2 where
    h2   = headFrom [n] `mappend` h1
    b2   = zipWith (:) (map putDecFromInt [1..]) b1'
    b1'  = sortByName ords (headNames h1) b1
    ords = map Asc ns

-- | Keep leading tuples.
limit :: (Ord c) => RopUse c -> Int -> String -> Relmap c
limit use c ns = relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord c) => Int -> String -> a -> AbMap (Rel c)
limit2 c ns _ (Rel h1 b1) = Right $ Rel h1 b2 where
    b2   = List.take c $ sortByName ords (headNames h1) b1
    ords = orders ns



-- ----------------------  typename

relopTypename :: (CContent c) => RopCons c
relopTypename use = do
  (n, p) <- getTermPair use "-term"
  Right $ relmapTypename use n p

relmapTypename :: (CContent c) => RopUse c -> String -> String -> Relmap c
relmapTypename use n p = relmapCalc use "typename" sub where
    sub _ = relTypename n p

{-| Get typename. -}
relTypename
    :: (CContent c)
    => String
    -> String
    -> AbMap (Rel c)
relTypename n p (Rel h1 b1) = Right $ Rel h2 b2 where
    h2 = headFrom [n] `mappend` h1
    b2 = map f b1
    pos = h1 `posOf` [[p]]
    f cs1 = let [c] = csPick pos cs1
            in putText (typename c) : cs1



-- ----------------------  range

relopRange :: RopCons VContent
relopRange use = do
  term <- getTerm use "-term"
  low  <- getInt  use "-from"
  high <- getInt  use "-to"
  Right $ relmapRange use term low high

relmapRange :: (CDec c) => RopUse c -> String -> Int -> Int -> Relmap c
relmapRange use n low high = relmapCalc use "range" sub where
    sub _ r1 = relRange n low high r1

relRange :: (CDec c) => String -> Int -> Int -> AbMap (Rel c)
relRange n low high (Rel h1 b1) = Right $ Rel h2 b2 where
    h2   = mappend (headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map putDecFromInt [low .. high]
    g xs = map (: xs) ys

{-
divide :: String -> Relmap VContent
divide ns2 = flow "divide" $ withP divide2 ns2

divide2 :: [String] -> Rel VContent -> Rel VContent
divide2 ns2 (Rel h1 b1) = Rel h2 b2 where
    h2  = mappend (headFrom ns2) h1
    b2  = concatMap g b1

    p = ns2 `look` names (headTerms h1)
    g | p `like` "--vv"  = ap f
      | otherwise          = const []

    [_,_,x,y] = p
    f the arg = if the y == putDecFromInt 0
                then []
                else [binv quot (the x) (the y) :
                      binv rem  (the x) (the y) : arg]
-}


-- ----------------------  RDF

relopRdf :: RopCons VContent
relopRdf use = do
  sign  <- Kit.getWord  use "-sign"
  [s,o] <- Kit.getTerms use "-term"
  Right $ relmapAlias use $
        relmapSource use sign ["/s", "/o"] `mappend`
        Mini.relmapRename use [(s,"/s"), (o,"/o")]


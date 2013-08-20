{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  ropConsSize, relmapSize, relSize,
  -- * conf
  ropConsConf, relmapConf, relConf,
  -- * enclose
  ropConsEnclose, relmapEnclose, relEnclose,
  -- * rank
  ropConsRank, relmapRank, relRank, limit,
  -- * typename
  ropConsTypename, relmapTypename, relTypename,
  -- * range
  ropConsRange, relmapRange,
  -- * RDF
  ropConsRdf
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import qualified Koshucode.Baala.Minimal as Mini
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Order



-- ----------------------  size

ropConsSize :: C.RopCons VContent
ropConsSize use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapSize use n

relmapSize :: (C.CDec c) => C.RopUse c -> String -> C.Relmap c
relmapSize use n = C.relmapCalc use "size" sub where
    sub _ = relSize n

{-| Change terms names -}
relSize
    :: (C.CDec c)
    => String              -- ^ List of term name (/to/, /from/)
    -> B.AbMap (B.Rel c)   -- ^ Relation to relation
relSize n (B.Rel _ b1) = Right $ B.Rel h2 b2 where
    h2 = B.headFrom [n]
    b2 = [[C.putDecFromInt $ length b1]]



-- ----------------------  conf

ropConsConf :: C.RopCons VContent
ropConsConf use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapConf use n

relmapConf :: (C.CText c) => C.RopUse c -> String -> C.Relmap c
relmapConf use n = C.relmapCalc use "conf" sub where
    sub _ = relConf n

{-| Change terms names -}
relConf
    :: (C.CText c)
    => String              -- ^ Term name
    -> B.AbMap (B.Rel c)   -- ^ Relation to relation
relConf n (B.Rel h1 _) = Right $ B.Rel h2 b2 where
    h2 = B.headFrom [n]
    b2 = [[C.putText $ show s]]
    s  = show $ B.docWraps "(" ")" h1



-- ----------------------  enclose

ropConsEnclose :: C.RopCons VContent
ropConsEnclose use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> String -> C.Relmap c
relmapEnclose use n = C.relmapCalc use "enclose" sub where
    sub _ = relEnclose n

{-| Enclose the current relation in a term. -}
relEnclose
    :: (C.CRel c)
    => String              -- ^ Term name
    -> B.AbMap (B.Rel c)   -- ^ Relation to relation
relEnclose n r@(B.Rel h1 _) = Right $ B.Rel h2 b2 where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    b2 = [[C.putRel r]]



-- ----------------------  rank

ropConsRank :: C.RopCons VContent
ropConsRank use =
    do n  <- Builtin.getTerm  use "-add"
       ns <- Builtin.getTerms use "-order"
       Right $ relmapRank use n ns

relmapRank :: (C.CDec c, Ord c) => C.RopUse c -> String -> [String] -> C.Relmap c
relmapRank use n ns = C.relmapCalc use "rank" sub where
    sub _ = relRank n ns

relRank :: (C.CDec c, Ord c) => String -> [String] -> B.AbMap (B.Rel c)
relRank n ns (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2   = B.headFrom [n] `Builtin.mappend` h1
    b2   = zipWith (:) (map C.putDecFromInt [1..]) b1'
    b1'  = sortByName ords (B.headNames h1) b1
    ords = map Asc ns

-- | Keep leading tuples.
limit :: (Ord c) => C.RopUse c -> Int -> String -> C.Relmap c
limit use c ns = C.relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord c) => Int -> String -> a -> B.AbMap (B.Rel c)
limit2 c ns _ (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
    b2   = List.take c $ sortByName ords (B.headNames h1) b1
    ords = orders ns



-- ----------------------  typename

ropConsTypename :: (C.CContent c) => C.RopCons c
ropConsTypename use = do
  (n, p) <- Builtin.getTermPair use "-term"
  Right $ relmapTypename use n p

relmapTypename :: (C.CContent c) => C.RopUse c -> String -> String -> C.Relmap c
relmapTypename use n p = C.relmapCalc use "typename" sub where
    sub _ = relTypename n p

{-| Get typename. -}
relTypename
    :: (C.CContent c)
    => String
    -> String
    -> B.AbMap (B.Rel c)
relTypename n p (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2 = B.headFrom [n] `Builtin.mappend` h1
    b2 = map f b1
    pos = h1 `B.posOf` [[p]]
    f cs1 = let [c] = B.csPick pos cs1
            in C.putText (C.typename c) : cs1



-- ----------------------  range

ropConsRange :: C.RopCons VContent
ropConsRange use =
  do term <- Builtin.getTerm use "-term"
     low  <- Builtin.getInt  use "-from"
     high <- Builtin.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> String -> Int -> Int -> C.Relmap c
relmapRange use n low high = C.relmapCalc use "range" sub where
    sub _ r1 = relRange n low high r1

relRange :: (C.CDec c) => String -> Int -> Int -> B.AbMap (B.Rel c)
relRange n low high (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2   = Builtin.mappend (B.headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map C.putDecFromInt [low .. high]
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

ropConsRdf :: C.RopCons VContent
ropConsRdf use =
    do sign  <- Builtin.getWord  use "-sign"
       [s,o] <- Builtin.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `Builtin.mappend`
             Mini.relmapRename use [(s,"/s"), (o,"/o")]


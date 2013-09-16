{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  ropConsSize, relmapSize, relSize,
  -- * conf
  --ropConsConf, relmapConf, relConf,
  -- * enclose
  ropConsEnclose, relmapEnclose, relEnclose,
  -- * rank
  ropConsRank, relmapRank, relRank, -- limit,
  -- * typename
  ropConsTypename, relmapTypename, relTypename,
  -- * range
  ropConsRange, relmapRange,
  -- * member
  -- $member
  ropConsMember, relmapMember, relMember,
  -- * RDF
  ropConsRdf
) where

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
relmapSize use n = C.relmapCalc use "size" sub gen where
    sub _ = relSize n
    gen _ _ = relgenSize n

relgenSize :: (C.CDec c) => String -> B.Ab (C.Relgen c)
relgenSize n = Right $ C.Relgen h2 (C.RelgenBody f) where
    h2 = B.headFrom [n]
    f b1 = [[C.putDecFromInt $ length b1]]

{-| Change terms names -}
relSize
    :: (C.CDec c)
    => String              -- ^ List of term name (/to/, /from/)
    -> B.AbMap (B.Rel c)   -- ^ Relation to relation
relSize n (B.Rel _ b1) = Right $ B.Rel h2 b2 where
    h2 = B.headFrom [n]
    b2 = [[C.putDecFromInt $ length b1]]



-- ----------------------  conf

-- ropConsConf :: C.RopCons VContent
-- ropConsConf use =
--   do n <- Builtin.getTerm use "-term"
--      Right $ relmapConf use n

-- relmapConf :: (C.CText c) => C.RopUse c -> String -> C.Relmap c
-- relmapConf use n = C.relmapCalc use "conf" sub where
--     sub _ = relConf n

-- {-| Change terms names -}
-- relConf
--     :: (C.CText c)
--     => String              -- ^ Term name
--     -> B.AbMap (B.Rel c)   -- ^ Relation to relation
-- relConf n (B.Rel h1 _) = Right $ B.Rel h2 b2 where
--     h2 = B.headFrom [n]
--     b2 = [[C.putText $ show s]]
--     s  = show $ B.docWraps "(" ")" h1



-- ----------------------  enclose

ropConsEnclose :: C.RopCons VContent
ropConsEnclose use =
  do n <- Builtin.getTerm use "-term"
     Right $ relmapEnclose use n

relmapEnclose :: (C.CRel c) => C.RopUse c -> String -> C.Relmap c
relmapEnclose use n = C.relmapCalc use "enclose" sub gen where
    sub _ = relEnclose n
    gen _ = relgenEnclose n

relgenEnclose
    :: (C.CRel c) => String -> B.Relhead -> B.Ab (C.Relgen c)
relgenEnclose n h1 = Right $ C.Relgen h2 (C.RelgenBody f) where
    h2 = B.Relhead [B.Nest n $ B.headTerms h1]
    f b1 = [[C.putRel $ B.Rel h1 b1]]

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
relmapRank use n ns = C.relmapCalc use "rank" sub gen where
    sub _ = relRank n ns
    gen _ = relgenRank n ns

relgenRank
  :: (Ord c, C.CDec c) =>
     String -> [String] -> B.Relhead -> B.Ab (C.Relgen c)
relgenRank n ns h1 = Right $ C.Relgen h2 $ C.RelgenBody g2 where
    h2    = n `B.headCons` h1
    g2 b1 = let b1' = sortByName ords (B.headNames h1) b1
            in zipWith (:) (map C.putDecFromInt [1..]) b1'
    ords  = map Asc ns

relRank :: (C.CDec c, Ord c) => String -> [String] -> B.AbMap (B.Rel c)
relRank n ns (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2   = B.headFrom [n] `Builtin.mappend` h1
    b2   = zipWith (:) (map C.putDecFromInt [1..]) b1'
    b1'  = sortByName ords (B.headNames h1) b1
    ords = map Asc ns

-- -- | Keep leading tuples.
-- limit :: (Ord c) => C.RopUse c -> Int -> String -> C.Relmap c
-- limit use c ns = C.relmapCalc use "limit" (limit2 c ns)

-- limit2 :: (Ord c) => Int -> String -> a -> B.AbMap (B.Rel c)
-- limit2 c ns _ (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
--     b2   = List.take c $ sortByName ords (B.headNames h1) b1
--     ords = orders ns



-- ----------------------  typename

ropConsTypename :: (C.CContent c) => C.RopCons c
ropConsTypename use = do
  (n, p) <- Builtin.getTermPair use "-term"
  Right $ relmapTypename use n p

relmapTypename :: (C.CContent c) => C.RopUse c -> String -> String -> C.Relmap c
relmapTypename use n p = C.relmapCalc use "typename" sub gen where
    sub _ = relTypename n p
    gen _ = relgenTypename n p

relgenTypename
  :: (C.CText c) =>
     String -> String -> B.Relhead -> B.Ab (C.Relgen c)
relgenTypename n p h1 = Right $ C.Relgen h2 (C.RelgenBody g2) where
    h2    = n `B.headCons` h1
    g2 b1 = map f b1
    pos   = h1 `B.posFlat` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1

{-| Get typename. -}
relTypename
    :: (C.CContent c)
    => String
    -> String
    -> B.AbMap (B.Rel c)
relTypename n p (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2 = B.headFrom [n] `Builtin.mappend` h1
    b2 = map f b1
    pos = h1 `B.posFlat` [p]
    f cs1 = let [c] = B.posPick pos cs1
            in C.putText (C.typename c) : cs1



-- ----------------------  range

ropConsRange :: C.RopCons VContent
ropConsRange use =
  do term <- Builtin.getTerm use "-term"
     low  <- Builtin.getInt  use "-from"
     high <- Builtin.getInt  use "-to"
     Right $ relmapRange use term low high

relmapRange :: (C.CDec c) => C.RopUse c -> String -> Int -> Int -> C.Relmap c
relmapRange use term low high = C.relmapCalc use "range" sub gen where
    sub _ r1 = relRange term low high r1
    gen _ = relgenRange term low high

relgenRange
  :: (C.CDec c) =>
     String -> Int -> Int -> B.Relhead -> B.Ab (C.Relgen c)
relgenRange term low high h1 = Right $ C.Relgen h2 (C.RelgenBody g2) where
    h2    = term `B.headCons` h1
    g2 b1 = concatMap g b1
    ys    = map C.putDecFromInt [low .. high]
    g xs  = map (: xs) ys

relRange :: (C.CDec c) => String -> Int -> Int -> B.AbMap (B.Rel c)
relRange n low high (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2   = Builtin.mappend (B.headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map C.putDecFromInt [low .. high]
    g xs = map (: xs) ys



-- ----------------------  member

{- $member

   Membership.
   Relmap @member@ @\/x@ @\/xs@ means
   @\/x@ is a member of @\/xs@.

   - Input relation has both @\/x@ and @\/xs@,
     check content of @\/x@ is acturally the member of @\/xs@.

   - Input relation has @\/xs@ and not @\/x@,
     add term @\/x@ as member of @\/xs@.
-}  

ropConsMember :: C.RopCons VContent
ropConsMember use =
  do x    <- Builtin.getTerm use "-1"
     xs   <- Builtin.getTerm use "-2"
     Right $ relmapMember use x xs

relmapMember :: C.RopUse VContent -> String -> String -> C.Relmap VContent
relmapMember use x xs = C.relmapCalc use "member" sub gen where
    sub _ r = relMember x xs r
    gen _ = relgenMember x xs

relgenMember :: String -> String -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMember x xs h1 = r2 where
    r2 | xHere && xsHere     = relgenMemberCheck  xPos xsPos h1
       | not xHere && xsHere = relgenMemberExpand x    xsPos h1
       | otherwise           = Left $ B.AbortNoTerms [x, xs]
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relgenMemberCheck :: B.TermPos -> B.TermPos -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMemberCheck xPos xsPos h1 = Right $ C.Relgen h1 (C.RelgenBody g2) where
    g2 b1  =  filter f b1
    f cs   =  let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
              in xCont `isMember` xsCont

relgenMemberExpand :: String -> B.TermPos -> B.Relhead -> B.Ab (C.Relgen VContent)
relgenMemberExpand x xsPos h1 = Right $ C.Relgen h2 (C.RelgenBody g2) where
    h2     =  B.headCons x h1
    g2 b1  =  concatMap f b1
    f cs   =  let [xsCont] = B.posPick [xsPos] cs
              in case xsCont of
                   VSet  xs  -> map (: cs) xs
                   VList xs  -> map (: cs) xs
                   _         -> [xsCont : cs]

relMember :: String -> String -> B.AbMap (B.Rel VContent)
relMember x xs r1@(B.Rel h1 _) = r2 where
    r2 | xHere && xsHere     = relMemberCheck  xPos xsPos r1
       | not xHere && xsHere = relMemberExpand x    xsPos r1
       | otherwise           = Left $ B.AbortNoTerms [x, xs]
    ([xPos, xsPos], [xHere, xsHere])
        = h1 `B.posHere` [x, xs]

relMemberCheck :: B.TermPos -> B.TermPos -> B.AbMap (B.Rel VContent)
relMemberCheck xPos xsPos (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
    b2        =  filter f b1
    f cs      =  let [xCont, xsCont] = B.posPick [xPos, xsPos] cs
                 in xCont `isMember` xsCont

relMemberExpand :: String -> B.TermPos -> B.AbMap (B.Rel VContent)
relMemberExpand x xsPos (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
    h2        =  B.headCons x h1
    b2        =  concatMap f b1
    f cs      =  let [xsCont] = B.posPick [xsPos] cs
                 in case xsCont of
                      VSet  xs  -> map (: cs) xs
                      VList xs  -> map (: cs) xs
                      _         -> [xsCont : cs]

-- relMemberCollect :: [B.TermPos] -> B.TermPos -> String -> B.AbMap (B.Rel VContent)
-- relMemberCollect []      = relMemberCollectAll
-- relMemberCollect eachPos = relMemberCollectEach eachPos

-- relMemberCollectAll :: B.TermPos -> String -> B.AbMap (B.Rel VContent)
-- relMemberCollectAll xPos xs (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
--     h2        =  B.headCons xs h1
--     b2        =  map (xsCont :) b1
--     xsCont    =  VSet . B.unique $ concatMap xPick b1
--     xPick     =  B.posPick [xPos]

-- relMemberCollectEach :: [B.TermPos] -> B.TermPos -> String -> B.AbMap (B.Rel VContent)
-- relMemberCollectEach eachPos xPos xs (B.Rel h1 b1) = Right $ B.Rel h2 b2 where
--     h2        =  B.headCons xs h1
--     b2        =  concatMap xsCont $ Map.elems eachMap
--     xsCont b  =  map ((VSet . B.unique $ concatMap xPick b) :) b
--     xPick     =  B.posPick [xPos]
--     eachMap   =  B.gatherToMap $ map kv b1
--     eachPick  =  B.posPick eachPos
--     kv cs     =  (eachPick cs, cs)



-- ----------------------  RDF

ropConsRdf :: C.RopCons VContent
ropConsRdf use =
    do sign  <- Builtin.getWord  use "-sign"
       [s,o] <- Builtin.getTerms use "-term"
       Right $ C.relmapAlias use $
             C.relmapSource use sign ["/s", "/o"] `Builtin.mappend`
             Mini.relmapRename use [(s,"/s"), (o,"/o")]


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Link and run relmaps.

module Koshucode.Baala.Core.Relmap.Run
( relkitLink,
  relkitRun,
  fixedRelation,
  bmapAlign,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap.Operator as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C
import qualified Koshucode.Baala.Core.Message         as Message

relkitLink :: forall c. (Ord c) => [C.RelkitDef c] -> B.Map (C.Relkit c)
relkitLink kits = linkKit where
    linkKit :: B.Map (C.Relkit c)
    linkKit (C.Relkit he bo) = C.Relkit he $ link bo

    kitsRec :: [C.RelkitDef c]
    kitsRec = linkKit `B.mapSndTo` kits

    links = map link

    link :: B.Map (C.RelkitBody c)
    link (B.Sourced src core) =
        B.Sourced src $
         case core of
           C.RelkitAbFull      u f bs    ->  C.RelkitAbFull      u f $ links bs
           C.RelkitOneToAbMany u f bs    ->  C.RelkitOneToAbMany u f $ links bs
           C.RelkitOneToAbOne  u f bs    ->  C.RelkitOneToAbOne  u f $ links bs
           C.RelkitAbSemi        f b     ->  C.RelkitAbSemi        f $ link  b
           C.RelkitAppend        b1 b2   ->  C.RelkitAppend (link b1) (link b2)
           C.RelkitWith          with b  ->  C.RelkitWith       with $ link  b
           C.RelkitCopy          copy b  ->  C.RelkitCopy       copy $ link  b

           C.RelkitLink n key _ ->
               case lookup key kitsRec of
                 Nothing                 ->  core
                 Just (C.Relkit _ b)     ->  C.RelkitLink n key $ Just b
           _                             ->  core

-- todo: optimization
relkitRun :: forall c. (Ord c, C.CRel c)
    => C.Global c -> [B.Named [[c]]] -> C.RelkitBody c -> B.AbMap [[c]]
relkitRun global rs (B.Sourced toks core) bo1 =
    ab toks $
     case core of
       C.RelkitFull        u f     ->  right u $ f             bo1
       C.RelkitOneToMany   u f     ->  right u $ f `concatMap` bo1
       C.RelkitOneToOne    u f     ->  right u $ f `map`       bo1
       C.RelkitPred          f     ->  Right   $ filter f      bo1

       C.RelkitAbFull      u f bs  ->  monad u $            f (bmaps bs)        bo1
       C.RelkitOneToAbOne  u f bs  ->  monad u $            f (bmaps bs) `mapM` bo1
       C.RelkitOneToAbMany u f bs  ->  right u . concat =<< f (bmaps bs) `mapM` bo1
       C.RelkitAbSemi        f b   ->  B.filterM (semi f b) bo1
       C.RelkitAbPred        f     ->  B.filterM f bo1

       C.RelkitConst           bo  ->  Right bo
       C.RelkitId                  ->  Right bo1

       C.RelkitAppend b1@(B.Sourced toks1 _) b2
                                   -> do bo2 <- relkitRun global rs b1 bo1
                                         ab toks1 $ relkitRun global rs b2 bo2

       C.RelkitSource p ns         -> let r = C.globalSelect global p ns
                                      in Right $ B.relBody r

       C.RelkitLink _ _ (Just b2)  ->  relkitRun global rs b2 bo1
       C.RelkitLink n _ (Nothing)  ->  Message.unkRelmap n

       C.RelkitNest n              ->  case lookup n rs of
                                         Just bo2 -> Right bo2
                                         Nothing  -> Message.unkNestRel n

       C.RelkitCopy n b            ->  do bo2 <- relkitRun global ((n, bo1) : rs) b bo1
                                          right True $ bo2
       C.RelkitWith with b         ->  do bo2 <- withRel with b `mapM` bo1
                                          right True $ concat bo2
    where
      ab    = B.abortable "run"
      bmaps = map $ relkitRun global rs

      semi :: ([[c]] -> B.Ab Bool) -> C.RelkitBody c -> [c] -> B.Ab Bool
      semi f b cs = f =<< relkitRun global rs b [cs]

      right :: (Ord b) => Bool -> [b] -> B.Ab [b]
      right u = Right . uif u

      monad :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
      monad u = (Right . uif u =<<)

      uif :: (Ord b) => Bool -> [b] -> [b]
      uif True   = B.unique
      uif False  = id

      withRel :: [(String, Int)] -> C.RelkitBody c -> [c] -> B.Ab [[c]]
      withRel with b cs =
          let nest = pickup cs `map` with
          in relkitRun global (nest ++ rs) b [cs]

      pickup :: [c] -> (String, Int) -> B.Named [[c]]
      pickup cs (name, ind) = (name, B.relBody $ C.gRel $ cs !! ind)

fixedRelation :: (Ord c) => B.Map (B.AbMap [[c]])
fixedRelation f = fix where
    fix bo1 = do bo2 <- f bo1
                 if bo1 == bo2 then Right bo2 else fix bo2

bmapAlign :: B.Relhead -> B.Relhead -> B.Map (B.AbMap [[c]])
bmapAlign he1 he2 f = g where
    g bo1 = do bo2 <- f bo1
               Right $ B.bodyAlign he1 he2 bo2


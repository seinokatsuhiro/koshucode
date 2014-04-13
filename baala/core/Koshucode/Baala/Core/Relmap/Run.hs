{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relmap.Run
( 
  -- * Run
  relkitLink,
  relkitRun,
  fixedRelation,
  bmapAlign,
) where

import qualified Control.Monad                       as Monad
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Content        as C
import qualified Koshucode.Baala.Core.Relmap.Relkit  as C
import qualified Koshucode.Baala.Core.Relmap.Rop     as C
import qualified Koshucode.Baala.Core.Message        as Message



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
           C.RelkitAbFull      u kitf kitbs  -> C.RelkitAbFull      u kitf $ links kitbs
           C.RelkitOneToAbMany u kitf kitbs  -> C.RelkitOneToAbMany u kitf $ links kitbs
           C.RelkitOneToAbOne  u kitf kitbs  -> C.RelkitOneToAbOne  u kitf $ links kitbs
           C.RelkitAbSemi        kitf kitb   -> C.RelkitAbSemi        kitf $ link  kitb
           C.RelkitAppend        kitb1 kitb2 -> C.RelkitAppend (link kitb1) (link kitb2)
           C.RelkitWith          with kitb   -> C.RelkitWith          with $ link  kitb
           C.RelkitCopy          copy kitb   -> C.RelkitCopy          copy $ link  kitb

           C.RelkitLink n key _ 
               -> case lookup key kitsRec of
                    Nothing -> core
                    Just (C.Relkit _ kitb) ->
                        C.RelkitLink n key $ Just kitb
           _ -> core

-- todo: optimization
relkitRun :: forall c. (Ord c, C.CRel c)
    => C.Global c -> [B.Named [[c]]] -> C.RelkitBody c -> B.AbMap [[c]]
relkitRun global rs (B.Sourced toks core) bo1 =
    ab toks $
     case core of
       C.RelkitFull        u kitf       ->  right u $ kitf             bo1
       C.RelkitOneToMany   u kitf       ->  right u $ kitf `concatMap` bo1
       C.RelkitOneToOne    u kitf       ->  right u $ kitf `map`       bo1
       C.RelkitPred          kitf       ->  Right   $ filter kitf      bo1

       C.RelkitAbFull      u kitf kitbs ->  monad u $            kitf (bmaps kitbs)        bo1
       C.RelkitOneToAbOne  u kitf kitbs ->  monad u $            kitf (bmaps kitbs) `mapM` bo1
       C.RelkitOneToAbMany u kitf kitbs ->  right u . concat =<< kitf (bmaps kitbs) `mapM` bo1
       C.RelkitAbSemi        kitf kitb  ->  Monad.filterM (semi kitf kitb) bo1
       C.RelkitAbPred        kitf       ->  Monad.filterM kitf bo1

       C.RelkitConst                 bo ->  Right bo
       C.RelkitId                       ->  Right bo1

       C.RelkitAppend kitb1@(B.Sourced toks1 _) kitb2
           -> do bo2 <- relkitRun global rs kitb1 bo1
                 ab toks1 $ relkitRun global rs kitb2 bo2

       C.RelkitSource p ns -> let r = C.globalSelect global p ns
                              in Right $ B.relBody r

       C.RelkitLink _ _ (Just kitb2) -> relkitRun global rs kitb2 bo1
       C.RelkitLink n _ (Nothing)    -> Message.unkRelmap n

       C.RelkitNest n         -> case lookup n rs of
                                   Just bo2 -> Right bo2
                                   Nothing  -> Message.unkNestRel n

       C.RelkitCopy n kitb    -> do bo2 <- relkitRun global ((n, bo1) : rs) kitb bo1
                                    right True $ bo2
       C.RelkitWith with kitb -> do bo2 <- withRel with kitb `mapM` bo1
                                    right True $ concat bo2
    where
      ab    = B.abortable "run"
      bmaps = map $ relkitRun global rs

      semi :: ([[c]] -> B.Ab Bool) -> C.RelkitBody c -> [c] -> B.Ab Bool
      semi kitf kitb cs = kitf =<< relkitRun global rs kitb [cs]

      right :: (Ord b) => Bool -> [b] -> B.Ab [b]
      right u = Right . uif u

      monad :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
      monad u = (Right . uif u =<<)

      uif :: (Ord b) => Bool -> [b] -> [b]
      uif True   = B.unique
      uif False  = id

      withRel :: [(String, Int)] -> C.RelkitBody c -> [c] -> B.Ab [[c]]
      withRel with kitb cs =
          let nest = pickup cs `map` with
          in relkitRun global (nest ++ rs) kitb [cs]

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


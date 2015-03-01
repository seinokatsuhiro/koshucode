{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Link and run relmaps.

module Koshucode.Baala.Core.Relkit.Run
  ( relkitLink,
    relkitRun,
    fixedRelation,
    bmapAlign,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Content          as C
import qualified Koshucode.Baala.Core.Relkit.Relkit    as C
import qualified Koshucode.Baala.Core.Relkit.Construct as C
import qualified Koshucode.Baala.Core.Message          as Msg

relkitLink :: forall c. (Ord c) => [C.RelkitDef c] -> B.Map (C.Relkit c)
relkitLink kits = linkKit where
    linkKit :: B.Map (C.Relkit c)
    linkKit (C.Relkit hi ho bo) = C.Relkit hi ho $ link bo

    kitsRec :: [C.RelkitDef c]
    kitsRec = linkKit `B.mapSndTo` kits

    link :: B.Map (C.RelkitBody c)
    link (B.Sourced src core) =
        B.Sourced src $
         case core of
           C.RelkitAbFull      u f bs    -> C.RelkitAbFull      u f $ map link bs
           C.RelkitOneToAbMany u f bs    -> C.RelkitOneToAbMany u f $ map link bs
           C.RelkitOneToAbOne  u f bs    -> C.RelkitOneToAbOne  u f $ map link bs
           C.RelkitAbSemi        f b     -> C.RelkitAbSemi        f $ link  b
           C.RelkitAppend        b1 b2   -> C.RelkitAppend (link b1) (link b2)
           C.RelkitNest        p nest b  -> C.RelkitNest     p nest $ link  b
           C.RelkitCopy        p copy b  -> C.RelkitCopy     p copy $ link  b
           C.RelkitLink n key _ ->
               case lookup key kitsRec of
                 Just (C.Relkit _ _ b)   -> C.RelkitLink n key $ Just b
                 Nothing                 -> core
           _                             -> core

-- todo: optimization
relkitRun :: forall h. forall c. (Ord c, C.CRel c, B.SelectRel h)
    => h c -> [C.Local [[c]]] -> C.RelkitBody c -> B.AbMap [[c]]
relkitRun hook rs (B.Sourced toks core) bo1 =
    Msg.abRun toks $
     case core of
       C.RelkitFull        u f     -> right u $ f             bo1
       C.RelkitOneToMany   u f     -> right u $ f `concatMap` bo1
       C.RelkitOneToOne    u f     -> right u $ f `map`       bo1
       C.RelkitPred          f     -> Right   $ filter f      bo1

       C.RelkitAbFull      u f bs  -> monad u $            f (mrun bs)        bo1
       C.RelkitOneToAbOne  u f bs  -> monad u $            f (mrun bs) `mapM` bo1
       C.RelkitOneToAbMany u f bs  -> right u . concat =<< f (mrun bs) `mapM` bo1
       C.RelkitAbSemi        f b   -> B.filterM (semi f b) bo1
       C.RelkitAbPred        f     -> B.filterM f bo1

       C.RelkitConst           bo  -> Right bo
       C.RelkitId                  -> Right bo1

       C.RelkitAppend b1@(B.Sourced toks1 _) b2
                                   -> do bo2 <- run b1 bo1
                                         Msg.abRun toks1 $ run b2 bo2

       C.RelkitSource pat ns       -> let r = B.selectRel hook pat ns
                                      in Right $ B.relBody r

       C.RelkitLink _ _ (Just b2)  -> run b2 bo1
       C.RelkitLink n _ (Nothing)  -> Msg.unkRelmap n

       C.RelkitNestVar p n         -> case C.a2lookup p n rs of
                                        Just bo2  -> Right bo2
                                        Nothing   -> Msg.unkNestRel p n $ C.localsLines rs
       C.RelkitNest p nest b       -> do bo2 <- nestRel p nest b `mapM` bo1
                                         right True $ concat bo2
       C.RelkitCopy p n b          -> do bo2 <- relkitRun hook ((p, [(n, bo1)]) : rs) b bo1
                                         right True $ bo2
    where
      run    = relkitRun hook rs
      mrun   = map run

      semi :: ([[c]] -> B.Ab Bool) -> C.RelkitBody c -> [c] -> B.Ab Bool
      semi f b cs = f =<< run b [cs]

      right :: (Ord b) => Bool -> [b] -> B.Ab [b]
      right u = Right . uif u

      monad :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
      monad u = (Right . uif u =<<)

      uif :: (Ord b) => Bool -> [b] -> [b]
      uif True   = B.unique
      uif False  = id

      nestRel :: B.Token -> [(String, Int)] -> C.RelkitBody c -> [c] -> B.Ab [[c]]
      nestRel p nest b cs =
          let cs2 = pickup cs `map` nest
          in relkitRun hook ((p, cs2) : rs) b [cs]

      pickup :: [c] -> (String, Int) -> B.Named [[c]]
      pickup cs (name, ind) = (name, B.relBody $ C.gRel $ cs !! ind)

fixedRelation :: (Ord c) => B.Map (B.AbMap [[c]])
fixedRelation f = fix where
    fix bo1 = do bo2 <- f bo1
                 if bo1 == bo2 then Right bo2 else fix bo2

bmapAlign :: B.Head -> B.Head -> B.Map (B.AbMap [[c]])
bmapAlign he1 he2 f = g where
    g bo1 = do bo2 <- f bo1
               Right $ B.bodyAlign he1 he2 bo2


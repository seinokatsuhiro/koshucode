{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Link and run relmaps.

module Koshucode.Baala.Core.Relkit.Run
  ( relkitLink,
    relkitRun,
    fixedRelation,
    bmapAlign,
    LocalTable,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax                 as S
import qualified Koshucode.Baala.Type                   as T
import qualified Koshucode.Baala.Data                   as D
import qualified Koshucode.Baala.Core.Relkit.Relkit     as C
import qualified Koshucode.Baala.Core.Lexmap.Message    as Msg
import qualified Koshucode.Baala.Core.Relkit.Message    as Msg

-- | Resolve relmap reference.
relkitLink :: forall c. (Ord c) => C.RelkitTable c -> O.Map (C.Relkit c)
relkitLink kits = linkKit where
    linkKit :: O.Map (C.Relkit c)
    linkKit (C.Relkit hi ho bo) = C.Relkit hi ho $ link bo

    kitsRec :: C.RelkitTable c
    kitsRec = linkKit O.<$$> kits

    link :: O.Map (C.RelkitBody c)
    link (B.Codic cp core) =
        B.Codic cp $
         case core of
           C.RelkitAbWhole  u f bs    -> C.RelkitAbWhole     u f $ map link bs
           C.RelkitAbMany   u f bs    -> C.RelkitAbMany      u f $ map link bs
           C.RelkitAbLine   u f bs    -> C.RelkitAbLine      u f $ map link bs
           C.RelkitAbSemi     f b     -> C.RelkitAbSemi        f $ link  b
           C.RelkitAppend     b1 b2   -> C.RelkitAppend (link b1) (link b2)
           C.RelkitNest     p nest b  -> C.RelkitNest     p nest $ link  b
           C.RelkitCopy     p copy b  -> C.RelkitCopy     p copy $ link  b
           C.RelkitLink n key _ ->
               case lookup key kitsRec of
                 Just (C.Relkit _ _ b)   -> C.RelkitLink n key $ Just b
                 Nothing                 -> core
           _                             -> core

-- todo: optimization
-- | Run relkit.
relkitRun :: forall h. forall c. (D.CContent c, T.SelectRel h)
    => h c -> [LocalTable c] -> C.RelkitBody c -> B.AbMap [[c]]
relkitRun hook rs (B.Codic cp core) bo1 =
    Msg.abRun cp $
     case core of
       C.RelkitWhole     u f     -> right u $ f         bo1
       C.RelkitMany      u f     -> right u $ f O.<++>  bo1
       C.RelkitLine      u f     -> right u $ f <$>     bo1
       C.RelkitTest        f     -> Right   $ filter f  bo1

       C.RelkitAbWhole   u f bs  -> monad u $            f (mrun bs)        bo1
       C.RelkitAbLine    u f bs  -> monad u $            f (mrun bs) `mapM` bo1
       C.RelkitAbMany    u f bs  -> right u . concat O.# f (mrun bs) `mapM` bo1
       C.RelkitAbSemi      f b   -> B.filterM (semi f b) bo1
       C.RelkitAbTest      f     -> B.filterM f bo1

       C.RelkitConst         bo  -> Right bo
       C.RelkitId                -> Right bo1

       C.RelkitAppend b1@(B.Codic cp' _) b2
                                 -> do bo2 <- run b1 bo1
                                       Msg.abRun cp' $ run b2 bo2

       C.RelkitSource pat ns     -> let r = T.selectRel hook pat ns
                                    in Right $ T.relBody r

       C.RelkitLink _ _ (Just b2)  -> run b2 bo1
       C.RelkitLink n _ (Nothing)  -> Msg.unkRelmap n

       C.RelkitNest p nest b  -> do bo2 <- nestRel p nest b `mapM` bo1
                                    right True $ concat bo2
       C.RelkitCopy p n b     -> do bo2 <- relkitRun hook ((p, [(S.LocalSymbol n, bo1)]) : rs) b bo1
                                    right True $ bo2
       C.RelkitLocal p n      -> case lookup2 p n rs of
                                   Just bo2 -> Right bo2
                                   Nothing  -> Msg.unkLocalRel p (B.name n) $ localsLines rs
    where
      run    = relkitRun hook rs
      mrun   = map run

      semi :: ([[c]] -> B.Ab Bool) -> C.RelkitBody c -> [c] -> B.Ab Bool
      semi f b cs = f O.# run b [cs]

      right :: (Ord b) => Bool -> [b] -> B.Ab [b]
      right u = Right . uif u

      monad :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
      monad u = (Right . uif u O.#)

      uif :: (Ord b) => Bool -> [b] -> [b]
      uif True   = B.unique
      uif False  = id

      nestRel :: S.Token -> [S.IndexTerm] -> C.RelkitBody c -> [c] -> B.Ab [[c]]
      nestRel p nest b cs =
          let cs2 = pickup cs <$> nest
          in relkitRun hook ((p, cs2) : rs) b [cs]

      pickup :: [c] -> S.IndexTerm -> (S.LocalRef, [[c]])
      pickup cs (n, i) = (S.LocalNest n, T.relBody $ D.gRel $ cs !! i)

-- | Calculate fixed relation.
fixedRelation :: (Ord c) => O.Map (B.AbMap [[c]])
fixedRelation f = fix where
    fix bo1 = do bo2 <- f bo1
                 if bo1 == bo2 then Right bo2 else fix bo2

-- | Change term order.
bmapAlign :: T.Head -> T.Head -> O.Map (B.AbMap [[c]])
bmapAlign he1 he2 f = g where
    g bo1 = do bo2 <- f bo1
               Right $ T.bodyForward he1 he2 bo2

{-# WARNING LocalTable "This is only used in defined module." #-}
-- | Lexical point and table of local relations.
type LocalTable c = (S.Token, [(S.LocalRef, [[c]])])

localsLines :: [LocalTable c] -> [String]
localsLines xs = map desc $ keys xs where
    desc (a, bs) = S.tokenContent a ++ " / " ++ unwords (B.name <$> bs)

-- | Kye list of double associations.
keys :: [(a, [(b, c)])] -> [(a, [b])]
keys = (map fst O.<$$>)

-- | Double lookup.
lookup2 :: (Eq a, Eq b) => a -> b -> [(a, [(b, c)])] -> Maybe c
lookup2 a b = lookup b O.#. lookup a


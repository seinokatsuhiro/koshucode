{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relfy
( 
  Relfy (..),
  RelfyBody (..),
  RelmapCalcRelfy,
  RelmapConflRelfy,
  relfyConst,
  relfyId,
  relfy,
) where

import qualified Control.Monad        as Monad
import qualified Data.Monoid          as Monoid
import qualified Koshucode.Baala.Base as B

data Relfy c = Relfy
    { relfyHead :: B.Relhead
    , relfyBody :: RelfyBody c
    }

instance Monoid.Monoid (Relfy c) where
    mempty = relfyConst B.reldee
    mappend (Relfy _ b1) (Relfy h2 b2) =
        Relfy h2 $ RelfyAppend b1 b2

-- Relmap are compiled to Relg
data RelfyBody c
    = RelfyOneToMany    Bool (  [c]  ->      [[c]] )
    | RelfyOneToOne     Bool (  [c]  ->       [c]  )
    | RelfyFull         Bool ( [[c]] ->      [[c]] )
    | RelfyPred              (  [c]  ->      Bool  )

    | RelfyOneToAbMany  Bool (  [c]  -> B.Ab [[c]] )
    | RelfyOneToAbOne   Bool (  [c]  -> B.Ab  [c]  )
    | RelfyAbFull       Bool ( [[c]] -> B.Ab [[c]] )
    | RelfyAbPred            (  [c]  -> B.Ab Bool  )

    | RelfyConst               [[c]]
    | RelfyId
    | RelfyAppend       (RelfyBody c) (RelfyBody c)
    | RelfyUnion        Bool [RelfyBody c]

instance Show (RelfyBody c) where
    show (RelfyOneToMany   _ _)  =  "RelfyOneToMany"
    show (RelfyOneToOne    _ _)  =  "RelfyOneToOne"
    show (RelfyFull        _ _)  =  "RelfyFull"
    show (RelfyPred          _)  =  "RelfyPred"

    show (RelfyOneToAbMany _ _)  =  "RelfyOneToAbMany"
    show (RelfyOneToAbOne  _ _)  =  "RelfyOneToAbOne"
    show (RelfyAbFull      _ _)  =  "RelfyAbFull"
    show (RelfyAbPred        _)  =  "RelfyAbPred"

    show (RelfyConst         _)  =  "RelfyConst"
    show (RelfyId             )  =  "RelfyId"
    show (RelfyAppend      x y)  =  "RelfyAppend " ++ show [x,y]
    show (RelfyUnion      _ xs)  =  "RelfyUnion " ++ show xs

instance Monoid.Monoid (RelfyBody c) where
    mempty      = relfyBodyId
    mappend x y = RelfyAppend x y

type RelmapConflRelfy c
    =  [(Relfy c)]      -- ^ Relfiers of subrelmaps
    -> RelmapCalcRelfy c

type RelmapCalcRelfy c
    =  B.Relhead        -- ^ Heading of input relation
    -> B.Ab (Relfy c)   -- ^ Relfier for output relation

relfyConst :: B.Rel c -> Relfy c
relfyConst (B.Rel h b) = Relfy h (RelfyConst b)

relfyId :: RelmapCalcRelfy c
relfyId h = Right (Relfy h relfyBodyId)

relfyBodyId :: RelfyBody c
relfyBodyId = RelfyOneToOne False id

relfy :: (Ord c) => RelfyBody c -> B.AbMap [[c]]
relfy = (<$>) where
    RelfyOneToAbMany u f <$> b1  =  do b2 <- f `mapM` b1
                                       uniqueRt u $ concat b2
    RelfyOneToAbOne  u f <$> b1  =  uniqueAb u $ f `mapM` b1
    RelfyOneToMany   u f <$> b1  =  uniqueRt u $ f `concatMap` b1
    RelfyOneToOne    u f <$> b1  =  uniqueRt u $ f `map` b1
    RelfyAbFull      u f <$> b1  =  uniqueAb u $ f b1
    RelfyFull        u f <$> b1  =  uniqueRt u $ f b1

    RelfyAbPred        f <$> b1  =  Monad.filterM f b1
    RelfyPred          f <$> b1  =  Right $ filter f b1
    RelfyConst        bc <$> _   =  Right bc
    RelfyId              <$> b1  =  Right b1

    RelfyUnion      u rs <$> b1  =  do b2 <- (<$> b1) `mapM` rs
                                       uniqueRt u $ concat b2
    RelfyAppend    g1 g2 <$> b1  =  do b2 <- g1 <$> b1
                                       g2 <$> b2

uniqueIf :: (Ord c) => Bool -> [c] -> [c]
uniqueIf True  = B.unique
uniqueIf False = id

uniqueRt :: (Ord c) => Bool -> [c] -> B.Ab [c]
uniqueRt u xs = Right $ uniqueIf u xs

uniqueAb :: (Ord c) => Bool -> B.Ab [c] -> B.Ab [c]
uniqueAb u xs = do xs' <- xs
                   Right $ uniqueIf u xs'


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relfy
( 
  Relfy (..),
  RelfyBody (..),
  RelmapRelfy,
  relfyConst,
  relfyId,
  relfy,
) where

import qualified Control.Monad as Monad
import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base as B

data Relfy c = Relfy
    { relfyHead :: B.Relhead
    , relfyBody :: RelfyBody c
    }

instance M.Monoid (Relfy c) where
    mempty = relfyConst B.reldee
    mappend (Relfy _ b1) (Relfy h2 b2) =
        Relfy h2 $ RelfyAppend b1 b2

-- Relmap are compiled to Relg
data RelfyBody c
    = RelfyOneToAbMany  (  [c]  -> B.Ab [[c]] )
    | RelfyOneToMany    (  [c]  ->      [[c]] )
    | RelfyOneToAbOne   (  [c]  -> B.Ab  [c]  )
    | RelfyOneToOne     (  [c]  ->       [c]  )
    | RelfyAbFull       ( [[c]] -> B.Ab [[c]] )
    | RelfyFull         ( [[c]] ->      [[c]] )
    | RelfyConst                        [[c]]
    | RelfyAbPred       (  [c]  -> B.Ab Bool  )
    | RelfyPred         (  [c]  ->      Bool  )
    | RelfyId
    | RelfyAppend       (RelfyBody c) (RelfyBody c)
    | RelfyUnion        [RelfyBody c]

instance Show (RelfyBody c) where
    show (RelfyOneToAbMany _)  =  "RelfyOneToAbMany"
    show (RelfyOneToAbOne  _)  =  "RelfyOneToAbOne"
    show (RelfyOneToMany   _)  =  "RelfyOneToMany"
    show (RelfyOneToOne    _)  =  "RelfyOneToOne"
    show (RelfyAbFull      _)  =  "RelfyAbFull"
    show (RelfyFull        _)  =  "RelfyFull"
    show (RelfyAbPred      _)  =  "RelfyAbPred"
    show (RelfyPred        _)  =  "RelfyPred"
    show (RelfyConst       _)  =  "RelfyConst"
    show (RelfyId           )  =  "RelfyId"
    show (RelfyAppend    x y)  =  "RelfyAppend " ++ show [x,y]
    show (RelfyUnion      xs)  =  "RelfyUnion " ++ show xs

instance M.Monoid (RelfyBody c) where
    mempty      = RelfyOneToOne id
    mappend x y = RelfyAppend x y

type RelmapRelfy c
    =  [(Relfy c)]      -- ^ Relfiers of subrelmaps
    -> B.Relhead        -- ^ Heading of input relation
    -> B.Ab (Relfy c)   -- ^ Relfier for output relation

relfyConst :: B.Rel c -> Relfy c
relfyConst (B.Rel h b) = Relfy h (RelfyConst b)

relfyId :: RelmapRelfy c
relfyId _ h = Right (Relfy h (RelfyOneToOne id))

relfy :: (Ord c) => RelfyBody c -> B.AbMap [[c]]
relfy = (<$>) where
    RelfyOneToAbMany f <$> b1  =  do b2 <- f `mapM` b1
                                     Right $ concat b2
    RelfyOneToAbOne  f <$> b1  =  f `mapM` b1
    RelfyOneToMany   f <$> b1  =  let b2 = f `map` b1
                                  in Right $ concat b2
    RelfyOneToOne    f <$> b1  =  Right $ B.unique $ f `map` b1
    RelfyAbPred      f <$> b1  =  Monad.filterM f b1
    RelfyPred        f <$> b1  =  Right $ filter f b1
    RelfyAbFull      f <$> b1  =  f b1
    RelfyFull        f <$> b1  =  Right $ f b1
    RelfyConst       b <$> _   =  Right b
    RelfyId            <$> b1  =  Right b1
    RelfyUnion      ts <$> b1  =  do b2 <- (<$> b1) `mapM` ts
                                     Right $ B.unique $ concat b2
    RelfyAppend  g1 g2 <$> b1  =  do b2 <- g1 <$> b1
                                     g2 <$> b2



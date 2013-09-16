{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relgen
( 
  Relgen (..),
  RelgenBody (..),
  RelgenCon,
  relgenConst,
  relgenId,
  runRelgenBody,
) where

import qualified Control.Monad as Monad
import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base as B

data Relgen c = Relgen
    { relgenHead :: B.Relhead
    , relgenBody :: RelgenBody c
    }

instance M.Monoid (Relgen c) where
    mempty = relgenConst B.reldee
    mappend (Relgen _ b1) (Relgen h2 b2) =
        Relgen h2 $ RelgenAppend b1 b2

-- Relmap are compiled to Relg
data RelgenBody c
    = RelgenOneToAbMany  ( [c] -> B.Ab [[c]] )
    | RelgenOneToMany    ( [c] -> [[c]] )
    | RelgenOneToAbOne   ( [c] -> B.Ab [c] )
    | RelgenOneToOne     ( [c] -> [c] )
    | RelgenAbBody       ( [[c]] -> B.Ab [[c]] )
    | RelgenFull         ( [[c]] -> [[c]] )
    | RelgenAbPred       ( [c] -> B.Ab Bool )
    | RelgenConst        [[c]]
    | RelgenAppend       (RelgenBody c) (RelgenBody c)
    | RelgenUnion        [RelgenBody c]

instance Show (RelgenBody c) where
    show (RelgenOneToAbMany _) = "RelgenOneToAbMany"
    show (RelgenOneToAbOne  _) = "RelgenOneToAbOne"
    show (RelgenOneToMany   _) = "RelgenOneToMany"
    show (RelgenOneToOne    _) = "RelgenOneToOne"
    show (RelgenAbPred      _) = "RelgenAbPred"
    show (RelgenAbBody      _) = "RelgenAbBody"
    show (RelgenFull        _) = "RelgenFull"
    show (RelgenConst       _) = "RelgenConst"
    show (RelgenAppend    x y) = "RelgenAppend (" ++ show x ++ ") (" ++ show y ++")"
    show (RelgenUnion      xs) = "RelgenUnion " ++ show xs

instance M.Monoid (RelgenBody c) where
    mempty      = RelgenOneToOne id
    mappend x y = RelgenAppend x y

type RelgenCon c
    =  [(Relgen c)]     -- ^ Relgs for subrelmaps
    -> B.Relhead        -- ^ Input head
    -> B.Ab (Relgen c)  -- ^ Tuple mapping and output head

relgenConst :: B.Rel c -> Relgen c
relgenConst (B.Rel h b) = Relgen h (RelgenConst b)

relgenId :: RelgenCon c
relgenId _ h = Right (Relgen h (RelgenOneToOne id))

runRelgenBody :: (Ord c) => RelgenBody c -> [[c]] -> B.Ab [[c]]
runRelgenBody = (<$>) where
    RelgenOneToAbMany f <$> b1 = do b2 <- f `mapM` b1
                                    Right $ concat b2
    RelgenOneToAbOne  f <$> b1 = f `mapM` b1
    RelgenOneToMany   f <$> b1 = let b2 = f `map` b1
                                 in Right $ concat b2
    RelgenOneToOne    f <$> b1 = Right $ B.unique $ f `map` b1
    RelgenAbPred      f <$> b1 = Monad.filterM f b1
    RelgenAbBody      f <$> b1 = f b1
    RelgenFull        f <$> b1 = Right $ f b1
    RelgenConst  css    <$> _  = Right css
    RelgenUnion ts      <$> b1 = do b2 <- (<$> b1) `mapM` ts
                                    Right $ B.unique $ concat b2
    RelgenAppend t1 t2  <$> b1 = do b2 <- t1 <$> b1
                                    t2 <$> b2



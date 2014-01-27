{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relfy
( 
  -- * Datatype
  Relfy (..),
  RelfyBody,
  RelfyCore (..),
  RelmapCalcRelfy,
  RelmapConflRelfy,

  -- * Constructor
  relfy,
  relfyId,
  relfyConst,
  relfyConstEmpty,
  relfyConstSingleton,
  relfyConstBody,
  relfySetSource,

  -- * Run
  relfyRun,
) where

import qualified Control.Monad        as Monad
import qualified Data.Monoid          as Monoid
import qualified Koshucode.Baala.Base as B



-- ----------------------  Datatype

data Relfy c = Relfy
    { relfyHead :: B.Relhead
    , relfyBody :: RelfyBody c
    }

instance Monoid.Monoid (Relfy c) where
    mempty = relfyConst B.reldee
    mappend (Relfy _ b1) (Relfy h2 b2) =
        Relfy h2 $ B.Sourced [] $ RelfyAppend b1 b2

type RelfyBody c = B.Sourced (RelfyCore c)

-- Relmap are compiled to Relg
data RelfyCore c
    = RelfyOneToMany    Bool (  [c]  ->      [[c]] )
    | RelfyOneToOne     Bool (  [c]  ->       [c]  )
    | RelfyFull         Bool ( [[c]] ->      [[c]] )
    | RelfyPred              (  [c]  ->      Bool  )

    | RelfyOneToAbMany  Bool (  [c]  -> B.Ab [[c]] )
    | RelfyOneToAbOne   Bool (  [c]  -> B.Ab  [c]  )
    | RelfyAbFull       Bool ( [[c]] -> B.Ab [[c]] )
    | RelfyAbPred            (  [c]  -> B.Ab Bool  )

    | RelfyConst                             [[c]]
    | RelfyId
    | RelfyAppend       (RelfyBody c) (RelfyBody c)
    | RelfyUnion        Bool [RelfyBody c]

instance Show (RelfyCore c) where
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

type RelmapConflRelfy c
    =  [(Relfy c)]      -- ^ Relfiers of subrelmaps
    -> RelmapCalcRelfy c

type RelmapCalcRelfy c
    =  B.Relhead        -- ^ Heading of input relation
    -> B.Ab (Relfy c)   -- ^ Relfier for output relation



-- ----------------------  Constructor

relfy :: B.Relhead -> RelfyCore c -> Relfy c
relfy h f = Relfy h $ B.Sourced [] f

relfyId :: B.Relhead -> Relfy c
relfyId h = (relfy h RelfyId)

relfyConst :: B.Rel c -> Relfy c
relfyConst (B.Rel h b) = relfy h $ RelfyConst b

relfyConstEmpty :: [B.Termname] -> Relfy c
relfyConstEmpty ns = relfyConstBody ns []

relfyConstSingleton :: [B.Termname] -> [c] -> Relfy c
relfyConstSingleton ns tuple = relfyConstBody ns [tuple]

relfyConstBody :: [B.Termname] -> [[c]] -> Relfy c
relfyConstBody ns body = r where
    r = relfy h $ RelfyConst body
    h = B.headFrom ns

relfySetSource :: (B.TokenListing a) => a -> B.Map (Relfy c)
relfySetSource src (Relfy h (B.Sourced _ f)) =
    Relfy h $ B.Sourced (B.tokenListing src) f



-- ----------------------  Run

relfyRun :: (Ord c) => RelfyBody c -> B.AbMap [[c]]
relfyRun (B.Sourced src r) b1 =
    B.ab src $
     case r of
       RelfyOneToAbMany u f  ->  do b2 <- f `mapM` b1
                                    uniqueR u $ concat b2
       RelfyOneToAbOne  u f  ->  uniqueA u $ f `mapM` b1
       RelfyOneToMany   u f  ->  uniqueR u $ f `concatMap` b1
       RelfyOneToOne    u f  ->  uniqueR u $ f `map` b1
       RelfyAbFull      u f  ->  uniqueA u $ f b1
       RelfyFull        u f  ->  uniqueR u $ f b1

       RelfyAbPred        f  ->  Monad.filterM f b1
       RelfyPred          f  ->  Right $ filter f b1
       RelfyConst         b  ->  Right b
       RelfyId               ->  Right b1

       RelfyUnion      u rs  ->  do b2 <- mapM (`relfyRun` b1) rs
                                    uniqueR u $ concat b2

       RelfyAppend r1@(B.Sourced src1 _) r2
           -> do b2       <- r1 `relfyRun` b1
                 B.ab src1 $ r2 `relfyRun` b2

uniqueR :: (Ord c) => Bool -> [c] -> B.Ab [c]
uniqueR u = Right . uniqueIf u

uniqueA :: (Ord c) => Bool -> B.Ab [c] -> B.Ab [c]
uniqueA u = (Right . uniqueIf u =<<)

uniqueIf :: (Ord c) => Bool -> [c] -> [c]
uniqueIf True  = B.unique
uniqueIf False = id


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relkit
( 
  -- * Datatype
  Relkit (..),
  RelkitBody,
  RelkitCore (..),
  RelmapCalcRelkit,
  RelmapConflRelkit,
  RelmapBinaryRelkit,

  -- * Constructor
  relkit,
  relkitId,
  relkitConst,
  relkitConstEmpty,
  relkitConstSingleton,
  relkitConstBody,
  relkitSetSource,

  -- * Run
  relkitRun,
) where

import qualified Control.Monad        as Monad
import qualified Data.Monoid          as Monoid
import qualified Koshucode.Baala.Base as B



-- ----------------------  Datatype

data Relkit c = Relkit
    { relkitHead :: B.Relhead
    , relkitBody :: RelkitBody c
    }

instance Monoid.Monoid (Relkit c) where
    mempty = relkitConst B.reldee
    mappend (Relkit _ b1) (Relkit h2 b2) =
        Relkit h2 $ B.Sourced [] $ RelkitAppend b1 b2

type RelkitBody c = B.Sourced (RelkitCore c)

-- Relmap are compiled to Relg
data RelkitCore c
    = RelkitOneToMany    Bool (  [c]  ->      [[c]] )
    | RelkitOneToOne     Bool (  [c]  ->       [c]  )
    | RelkitFull         Bool ( [[c]] ->      [[c]] )
    | RelkitPred              (  [c]  ->      Bool  )

    | RelkitOneToAbMany  Bool (  [c]  -> B.Ab [[c]] )
    | RelkitOneToAbOne   Bool (  [c]  -> B.Ab  [c]  )
    | RelkitAbFull       Bool ( [[c]] -> B.Ab [[c]] )
    | RelkitAbPred            (  [c]  -> B.Ab Bool  )

    | RelkitConst                             [[c]]
    | RelkitId
    | RelkitAppend       (RelkitBody c) (RelkitBody c)
    | RelkitUnion        Bool [RelkitBody c]

instance Show (RelkitCore c) where
    show (RelkitOneToMany   _ _)  =  "RelkitOneToMany"
    show (RelkitOneToOne    _ _)  =  "RelkitOneToOne"
    show (RelkitFull        _ _)  =  "RelkitFull"
    show (RelkitPred          _)  =  "RelkitPred"

    show (RelkitOneToAbMany _ _)  =  "RelkitOneToAbMany"
    show (RelkitOneToAbOne  _ _)  =  "RelkitOneToAbOne"
    show (RelkitAbFull      _ _)  =  "RelkitAbFull"
    show (RelkitAbPred        _)  =  "RelkitAbPred"

    show (RelkitConst         _)  =  "RelkitConst"
    show (RelkitId             )  =  "RelkitId"
    show (RelkitAppend      x y)  =  "RelkitAppend " ++ show [x,y]
    show (RelkitUnion      _ xs)  =  "RelkitUnion " ++ show xs

type RelmapCalcRelkit c
    =  B.Relhead        -- ^ Heading of input relation
    -> B.Ab (Relkit c)   -- ^ Relfier for output relation

type RelmapConflRelkit c
    =  [(Relkit c)]      -- ^ Relfiers of subrelmaps
    -> RelmapCalcRelkit c

type RelmapBinaryRelkit c = Relkit c -> RelmapCalcRelkit c



-- ----------------------  Constructor

relkit :: B.Relhead -> RelkitCore c -> Relkit c
relkit h f = Relkit h $ B.Sourced [] f

relkitId :: B.Relhead -> Relkit c
relkitId h = (relkit h RelkitId)

relkitConst :: B.Rel c -> Relkit c
relkitConst (B.Rel h b) = relkit h $ RelkitConst b

relkitConstEmpty :: [B.Termname] -> Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.Termname] -> [c] -> Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.Termname] -> [[c]] -> Relkit c
relkitConstBody ns body = r where
    r = relkit h $ RelkitConst body
    h = B.headFrom ns

relkitSetSource :: (B.TokenListing a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit h (B.Sourced _ f)) =
    Relkit h $ B.Sourced (B.tokenListing src) f



-- ----------------------  Run

relkitRun :: (Ord c) => RelkitBody c -> B.AbMap [[c]]
relkitRun (B.Sourced src r) b1 =
    B.abortable "run" src $
     case r of
       RelkitOneToAbMany u f  ->  do b2 <- f `mapM` b1
                                     uniqueR u $ concat b2
       RelkitOneToAbOne  u f  ->  uniqueA u $ f `mapM` b1
       RelkitOneToMany   u f  ->  uniqueR u $ f `concatMap` b1
       RelkitOneToOne    u f  ->  uniqueR u $ f `map` b1
       RelkitAbFull      u f  ->  uniqueA u $ f b1
       RelkitFull        u f  ->  uniqueR u $ f b1

       RelkitAbPred        f  ->  Monad.filterM f b1
       RelkitPred          f  ->  Right $ filter f b1
       RelkitConst         b  ->  Right b
       RelkitId               ->  Right b1

       RelkitUnion      u rs  ->  do b2 <- mapM (`relkitRun` b1) rs
                                     uniqueR u $ concat b2

       RelkitAppend r1@(B.Sourced src1 _) r2
           -> do b2 <- r1 `relkitRun` b1
                 B.abortable "run" src1 $ r2 `relkitRun` b2

uniqueR :: (Ord c) => Bool -> [c] -> B.Ab [c]
uniqueR u = Right . uniqueIf u

uniqueA :: (Ord c) => Bool -> B.Ab [c] -> B.Ab [c]
uniqueA u = (Right . uniqueIf u =<<)

uniqueIf :: (Ord c) => Bool -> [c] -> [c]
uniqueIf True  = B.unique
uniqueIf False = id


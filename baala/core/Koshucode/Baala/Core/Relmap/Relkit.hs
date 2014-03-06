{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relkit
( 
  -- * Datatype
  Relkit (..),
  RelkitBody,
  RelkitKey,
  RelkitDef,
  RelkitCore (..),

  -- * Constructor
  relkit,
  relkitJust,
  relkitNothing,
  relkitId,
  relkitConst,
  relkitConstEmpty,
  relkitConstSingleton,
  relkitConstBody,
  relkitSetSource,

  -- * Run
  relkitLink,
  relkitRun,
) where

import qualified Control.Monad        as Monad
import qualified Data.Monoid          as Monoid
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap.Lexical as C



-- ----------------------  Datatype

data Relkit c = Relkit
    { relkitHead :: Maybe B.Relhead
    , relkitBody :: RelkitBody c
    }

instance Monoid.Monoid (Relkit c) where
    mempty = relkitConst B.reldee
    mappend (Relkit _ b1) (Relkit h2 b2) =
        Relkit h2 $ B.Sourced [] $ RelkitAppend b1 b2

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Relhead, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- Specialized relmap
data RelkitCore c
    = RelkitFull         Bool ( [[c]] -> [[c]] )
    | RelkitOneToMany    Bool (  [c]  -> [[c]] )
    | RelkitOneToOne     Bool (  [c]  ->  [c]  )
    | RelkitPred              (  [c]  -> Bool  )

    | RelkitAbFull       Bool [RelkitBody c] ( [B.Ab [[c]]] -> [[c]] -> B.Ab [[c]] )
    | RelkitOneToAbMany  Bool [RelkitBody c] ( [B.Ab [[c]]] -> [c]   -> B.Ab [[c]] )
    | RelkitOneToAbOne   Bool [RelkitBody c] ( [B.Ab [[c]]] -> [c]   -> B.Ab [c] )
    | RelkitAbSemi            (RelkitBody c)                ( [[c]]  -> B.Ab Bool )
    | RelkitAbPred                                           ( [c]   -> B.Ab Bool )

    | RelkitConst             [[c]]
    | RelkitAppend            (RelkitBody c) (RelkitBody c)
    | RelkitUnion        Bool [RelkitBody c]
    | RelkitId

    | RelkitLink         String RelkitKey (Maybe (RelkitBody c))

instance Show (RelkitCore c) where
    show (RelkitOneToMany   _ _)   =  "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   =  "RelkitOneToOne"
    show (RelkitFull        _ _)   =  "RelkitFull"
    show (RelkitPred          _)   =  "RelkitPred"

    show (RelkitOneToAbMany _ _ _) =  "RelkitOneToAbMany"
    show (RelkitOneToAbOne _ _ _)  =  "RelkitOneToAbOne"
    show (RelkitAbFull    _ _ _)   =  "RelkitAbFull"
    show (RelkitAbPred        _)   =  "RelkitAbPred"
    show (RelkitAbSemi      _ _)   =  "RelkitAbSemi"

    show (RelkitConst         _)   =  "RelkitConst"
    show (RelkitId             )   =  "RelkitId"
    show (RelkitAppend      x y)   =  "RelkitAppend " ++ show [x,y]
    show (RelkitUnion      _ xs)   =  "RelkitUnion " ++ show xs

    show (RelkitLink      n _ _)   =  "RelkitLink " ++ n



-- ----------------------  Constructor

relkit :: Maybe B.Relhead -> RelkitCore c -> Relkit c
relkit h f = Relkit h $ B.Sourced [] f

relkitJust :: B.Relhead -> RelkitCore c -> Relkit c
relkitJust h = relkit (Just h)

relkitNothing :: Relkit c
relkitNothing = relkit Nothing RelkitId

relkitId :: Maybe B.Relhead -> Relkit c
relkitId h = (relkit h RelkitId)

relkitConst :: B.Rel c -> Relkit c
relkitConst (B.Rel h b) = relkit (Just h) $ RelkitConst b

relkitConstEmpty :: [B.Termname] -> Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.Termname] -> [c] -> Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.Termname] -> [[c]] -> Relkit c
relkitConstBody ns body = r where
    r = relkit h $ RelkitConst body
    h = Just $ B.headFrom ns

relkitSetSource :: (B.TokenListing a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit h (B.Sourced _ f)) =
    Relkit h $ B.Sourced (B.tokenListing src) f



-- ----------------------  Run

relkitLink :: forall c. (Ord c) => [(RelkitKey, Relkit c)] -> B.Map (Relkit c)
relkitLink kits = linkKit where
    linkKit :: B.Map (Relkit c)
    linkKit (Relkit h b) = Relkit h $ link b

    kitsRec :: [(RelkitKey, Relkit c)]
    kitsRec = linkKit `B.mapSndTo` kits

    link :: B.Map (RelkitBody c)
    link (B.Sourced src core) =
        let s = B.Sourced src
        in s $ case core of
                 RelkitAbFull      u bs f  -> RelkitAbFull      u (map link bs) f
                 RelkitOneToAbMany u bs f  -> RelkitOneToAbMany u (map link bs) f
                 RelkitOneToAbOne  u bs f  -> RelkitOneToAbOne  u (map link bs) f
                 RelkitAbSemi        b1 f  -> RelkitAbSemi        (link b1) f
                 RelkitAppend        b1 b2 -> RelkitAppend        (link b1) (link b2)
                 RelkitUnion       u bs    -> RelkitUnion       u (map link bs)
                 RelkitLink       n key _  -> case lookup key kitsRec of
                                                Nothing -> core
                                                Just (Relkit _ b2) ->
                                                    RelkitLink n key (Just b2)
                 _ -> core

-- todo: optimization
relkitRun :: (Ord c) => RelkitBody c -> B.AbMap [[c]]
relkitRun (B.Sourced src kit) b1 =
    B.abortable "run" src $
     case kit of
       RelkitOneToAbMany u k f  ->  do let b2 = map (`relkitRun` b1) k
                                       b3 <- f b2 `mapM` b1
                                       uniqueRight u $ concat b3
       RelkitOneToAbOne  u k f  ->  do let b2 = map (`relkitRun` b1) k
                                       uniqueAb u $ f b2 `mapM` b1
       RelkitOneToMany   u f  ->  uniqueRight u $ f `concatMap` b1
       RelkitOneToOne    u f  ->  uniqueRight u $ f `map` b1
       RelkitAbFull    u k f  ->  do let b2 = map (`relkitRun` b1) k
                                     uniqueAb u $ f b2 b1
       RelkitFull        u f  ->  uniqueRight u $ f b1

       RelkitAbPred        f  ->  Monad.filterM  f b1
       RelkitAbSemi      k f  ->  Monad.filterM (semi k f) b1
       RelkitPred          f  ->  Right $ filter f b1
       RelkitConst         b  ->  Right b
       RelkitId               ->  Right b1

       RelkitUnion    u kits  ->  do b2 <- mapM (`relkitRun` b1) kits
                                     uniqueRight u $ concat b2

       RelkitAppend kit1@(B.Sourced src1 _) kit2
           -> do b2 <- kit1 `relkitRun` b1
                 B.abortable "run" src1 $ kit2 `relkitRun` b2

       RelkitLink _ _ (Just kit2) -> relkitRun kit2 b1
       RelkitLink n _ (Nothing)   -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap n

    where
      semi k f cs = f =<< relkitRun k [cs]

uniqueRight :: (Ord b) => Bool -> [b] -> B.Ab [b]
uniqueRight u = Right . uniqueIf u

uniqueAb :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
uniqueAb u = (Right . uniqueIf u =<<)

uniqueIf :: (Ord b) => Bool -> [b] -> [b]
uniqueIf True  = B.unique
uniqueIf False = id


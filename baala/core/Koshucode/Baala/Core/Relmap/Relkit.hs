{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Relkit
( 
  -- * Datatype
  Relkit (..),
  RelkitBody,
  RelkitKey,
  RelkitDef,
  Relfun,
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
  fixedRelation,
  bodyMapArrange,
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
type Relfun c     = [[c]] -> B.Ab [[c]]

-- Specialized relmap
data RelkitCore c
    = RelkitFull         Bool ( [[c]] -> [[c]] )
    | RelkitOneToMany    Bool (  [c]  -> [[c]] )
    | RelkitOneToOne     Bool (  [c]  ->  [c]  )
    | RelkitPred              (  [c]  -> Bool  )

    | RelkitAbFull       Bool ( [ Relfun c ] -> Relfun c ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [ B.Ab [[c]] ] -> [c]   -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [ B.Ab [[c]] ] -> [c]   -> B.Ab [c] )   [RelkitBody c]
    | RelkitAbSemi            ( [[c]]  -> B.Ab Bool )                 (RelkitBody c)
    | RelkitAbPred            (  [c]   -> B.Ab Bool )

    | RelkitConst             [[c]]
    | RelkitAppend            (RelkitBody c) (RelkitBody c)
    | RelkitId

    | RelkitLink         String RelkitKey (Maybe (RelkitBody c))

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   =  "RelkitFull"
    show (RelkitOneToMany   _ _)   =  "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   =  "RelkitOneToOne"
    show (RelkitPred          _)   =  "RelkitPred"

    show (RelkitAbFull    _ _ _)   =  "RelkitAbFull"
    show (RelkitOneToAbMany _ _ _) =  "RelkitOneToAbMany"
    show (RelkitOneToAbOne _ _ _)  =  "RelkitOneToAbOne"
    show (RelkitAbSemi      _ _)   =  "RelkitAbSemi"
    show (RelkitAbPred        _)   =  "RelkitAbPred"

    show (RelkitConst         _)   =  "RelkitConst"
    show (RelkitAppend      x y)   =  "RelkitAppend " ++ show [x,y]
    show (RelkitId             )   =  "RelkitId"

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
                 RelkitAbFull      u f bs  -> RelkitAbFull      u f (map link bs)
                 RelkitOneToAbMany u f bs  -> RelkitOneToAbMany u f (map link bs)
                 RelkitOneToAbOne  u f bs  -> RelkitOneToAbOne  u f (map link bs)
                 RelkitAbSemi        f  b1 -> RelkitAbSemi        f (link b1)
                 RelkitAppend        b1 b2 -> RelkitAppend        (link b1) (link b2)
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
       RelkitFull        u f    ->  uniqueRight u $ f b1
       RelkitOneToMany   u f    ->  uniqueRight u $ f `concatMap` b1
       RelkitOneToOne    u f    ->  uniqueRight u $ f `map` b1
       RelkitPred          f    ->  Right $ filter f b1

       RelkitAbFull      u f ks ->  uniqueAb u $ f (map relkitRun ks) b1
       RelkitOneToAbMany u f ks ->  uniqueRight u . concat =<< f (sub ks) `mapM` b1
       RelkitOneToAbOne  u f ks ->  uniqueAb u $ f (sub ks) `mapM` b1
       RelkitAbSemi        f k  ->  Monad.filterM (semi k f) b1
       RelkitAbPred        f    ->  Monad.filterM f b1

       RelkitConst           b  ->  Right b
       RelkitId                 ->  Right b1

       RelkitAppend kit1@(B.Sourced src1 _) kit2
           -> do b2 <- kit1 `relkitRun` b1
                 B.abortable "run" src1 $ kit2 `relkitRun` b2

       RelkitLink _ _ (Just kit2) -> relkitRun kit2 b1
       RelkitLink n _ (Nothing)   -> Left $ B.AbortAnalysis [] $ B.AAUnkRelmap n

    where
      sub = map (`relkitRun` b1)
      semi k f cs = f =<< relkitRun k [cs]

uniqueRight :: (Ord b) => Bool -> [b] -> B.Ab [b]
uniqueRight u = Right . uniqueIf u

uniqueAb :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
uniqueAb u = (Right . uniqueIf u =<<)

uniqueIf :: (Ord b) => Bool -> [b] -> [b]
uniqueIf True  = B.unique
uniqueIf False = id

fixedRelation :: (Ord c) => B.Map (B.AbMap [[c]])
fixedRelation f = fix where
    fix b1 = do b2 <- f b1
                if b1 == b2 then Right b2 else fix b2

bodyMapArrange :: B.Relhead -> B.Relhead ->  B.Map (B.AbMap [[c]])
bodyMapArrange h1 h2 f = g where
    g b1 = do b2 <- f b1
              Right $ B.bodyArrange h1 h2 b2


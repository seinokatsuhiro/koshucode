{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relmap.Relkit
( 
  -- * Datatype
  Relkit (..),
  RelkitBody,
  RelkitKey,
  RelkitDef,
  Relbmap,
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

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitHead :: Maybe B.Relhead
    , relkitBody :: RelkitBody c
    }

instance Monoid.Monoid (Relkit c) where
    mempty = relkitConst B.reldee
    mappend (Relkit _ bo1) (Relkit he2 bo2) =
        relkit he2 $ RelkitAppend bo1 bo2

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Relhead, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- | Mapping for body of relation.
type Relbmap c = [[c]] -> B.Ab [[c]]

data RelkitCore c
    = RelkitFull         Bool ( [[c]] -> [[c]] )
    | RelkitOneToMany    Bool (  [c]  -> [[c]] )
    | RelkitOneToOne     Bool (  [c]  ->  [c]  )
    | RelkitPred              (  [c]  -> Bool  )

    | RelkitAbFull       Bool ( [Relbmap c] -> [[c]] -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [Relbmap c] ->  [c]  -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [Relbmap c] ->  [c]  -> B.Ab  [c]  ) [RelkitBody c]
    | RelkitAbSemi                           ( [[c]] -> B.Ab Bool )  (RelkitBody c)
    | RelkitAbPred                           (  [c]  -> B.Ab Bool )

    | RelkitConst             [[c]]
    | RelkitAppend            (RelkitBody c) (RelkitBody c)
    | RelkitId

    | RelkitLink        String RelkitKey (Maybe (RelkitBody c))

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
relkit he = Relkit he . B.Sourced []

relkitJust :: B.Relhead -> RelkitCore c -> Relkit c
relkitJust he = relkit $ Just he

relkitNothing :: Relkit c
relkitNothing = relkit Nothing RelkitId

relkitId :: Maybe B.Relhead -> Relkit c
relkitId he = relkit he RelkitId

relkitConst :: B.Rel c -> Relkit c
relkitConst (B.Rel he bo) = relkitJust he $ RelkitConst bo

relkitConstEmpty :: [B.Termname] -> Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.Termname] -> [c] -> Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.Termname] -> [[c]] -> Relkit c
relkitConstBody ns bo = kit where
    he  = B.headFrom ns
    kit = relkitJust he $ RelkitConst bo

relkitSetSource :: (B.TokenListing a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit he (B.Sourced _ core)) =
    Relkit he $ B.Sourced (B.tokenListing src) core



-- ----------------------  Run

relkitLink :: forall c. (Ord c) => [RelkitDef c] -> B.Map (Relkit c)
relkitLink kits = linkKit where
    linkKit :: B.Map (Relkit c)
    linkKit (Relkit he bo) = Relkit he $ link bo

    kitsRec :: [RelkitDef c]
    kitsRec = linkKit `B.mapSndTo` kits

    links = map link

    link :: B.Map (RelkitBody c)
    link (B.Sourced src core) =
        B.Sourced src $
         case core of
           RelkitAbFull      u kitf kitbs  -> RelkitAbFull      u kitf $ links kitbs
           RelkitOneToAbMany u kitf kitbs  -> RelkitOneToAbMany u kitf $ links kitbs
           RelkitOneToAbOne  u kitf kitbs  -> RelkitOneToAbOne  u kitf $ links kitbs
           RelkitAbSemi        kitf kitb   -> RelkitAbSemi        kitf $ link  kitb
           RelkitAppend        kitb1 kitb2 -> RelkitAppend (link kitb1) (link kitb2)

           RelkitLink n key _ 
               -> case lookup key kitsRec of
                    Nothing -> core
                    Just (Relkit _ kitb) ->
                        RelkitLink n key $ Just kitb
           _ -> core

-- todo: optimization
relkitRun :: forall c. (Ord c) => RelkitBody c -> B.AbMap [[c]]
relkitRun (B.Sourced src core) bo1 =
    B.abortable "run" src $
     case core of
       RelkitFull        u kitf       ->  right u $ kitf             bo1
       RelkitOneToMany   u kitf       ->  right u $ kitf `concatMap` bo1
       RelkitOneToOne    u kitf       ->  right u $ kitf `map`       bo1
       RelkitPred          kitf       ->  Right   $ filter kitf      bo1

       RelkitAbFull      u kitf kitbs ->  ab    u $            kitf (bmaps kitbs)        bo1
       RelkitOneToAbOne  u kitf kitbs ->  ab    u $            kitf (bmaps kitbs) `mapM` bo1
       RelkitOneToAbMany u kitf kitbs ->  right u . concat =<< kitf (bmaps kitbs) `mapM` bo1
       RelkitAbSemi        kitf kitb  ->  Monad.filterM (semi kitf kitb) bo1
       RelkitAbPred        kitf       ->  Monad.filterM kitf bo1

       RelkitConst                 bo ->  Right bo
       RelkitId                       ->  Right bo1

       RelkitAppend kitb1@(B.Sourced src1 _) kitb2
           -> do bo2 <- kitb1 `relkitRun` bo1
                 B.abortable "run" src1 $ kitb2 `relkitRun` bo2

       RelkitLink _ _ (Just kitb2) -> relkitRun kitb2 bo1
       RelkitLink n _ (Nothing)    -> Left $ B.abortBy $ B.AAUnkRelmap n

    where
      bmaps = map relkitRun

      semi :: ([[c]] -> B.Ab Bool) -> RelkitBody c -> [c] -> B.Ab Bool
      semi kitf kitb cs = kitf =<< relkitRun kitb [cs]

      right :: (Ord b) => Bool -> [b] -> B.Ab [b]
      right u = Right . uif u

      ab :: (Ord b) => Bool -> B.Ab [b] -> B.Ab [b]
      ab u = (Right . uif u =<<)

      uif :: (Ord b) => Bool -> [b] -> [b]
      uif True   = B.unique
      uif False  = id

fixedRelation :: (Ord c) => B.Map (B.AbMap [[c]])
fixedRelation f = fix where
    fix bo1 = do bo2 <- f bo1
                 if bo1 == bo2 then Right bo2 else fix bo2

bodyMapArrange :: B.Relhead -> B.Relhead ->  B.Map (B.AbMap [[c]])
bodyMapArrange he1 he2 f = g where
    g bo1 = do bo2 <- f bo1
               Right $ B.bodyArrange he1 he2 bo2


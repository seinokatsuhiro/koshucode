{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relmap.Relkit
( 
  -- * Datatype
  Relkit (..),
  RelkitCore (..),

  RelkitBody,
  RelkitKey,
  RelkitDef,
  Relbmap,
  RelSelect,

  -- * Constructor
  relkit,
  relkitJust,
  relkitNothing,
  relkitId,
  relkitConst,
  relkitConstEmpty,
  relkitConstSingleton,
  relkitConstBody,
  relkitSource,
  relkitNest,
  relkitCopy,
  relkitWith,
  relkitSetSource,
) where

import qualified Data.Monoid                         as Monoid
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Relmap.Lexmap  as C



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

    | RelkitSource      String [B.TermName]

    | RelkitLink        String RelkitKey (Maybe (RelkitBody c))
    | RelkitNest        String
    | RelkitCopy        String (RelkitBody c)
    | RelkitWith        [(String, Int)] (RelkitBody c)

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

    show (RelkitSource     p ns)   =  "RelkitSource " ++ p ++ " " ++ show ns
    show (RelkitLink      n _ _)   =  "RelkitLink " ++ n
    show (RelkitNest          n)   =  "RelkitNest " ++ n
    show (RelkitCopy        _ _)   =  "RelkitCopy "
    show (RelkitWith        _ _)   =  "RelkitWith "

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Relhead, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- | Relation selector
type RelSelect c = B.JudgePattern -> [String] -> B.Rel c

-- | Mapping for body of relation.
type Relbmap c = [[c]] -> B.Ab [[c]]


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

relkitConstEmpty :: [B.TermName] -> Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.TermName] -> [c] -> Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.TermName] -> [[c]] -> Relkit c
relkitConstBody ns bo = kit where
    he  = B.headFrom ns
    kit = relkitJust he $ RelkitConst bo

relkitSource :: String -> [B.TermName] -> Relkit c
relkitSource p ns = relkitJust he kit where
    he  = B.headFrom ns
    kit = RelkitSource p ns

relkitNest :: String -> B.Relhead -> Relkit c
relkitNest n he = kit where
    kit = relkitJust he $ RelkitNest n

relkitCopy :: String -> B.Map (Relkit c)
relkitCopy n (Relkit he kitb) = kit2 where
    kit2 = relkit he $ RelkitCopy n kitb

relkitWith :: [(String, Int)] -> B.Map (Relkit c)
relkitWith with (Relkit he kitb) = kit2 where
    kit2 = relkit he $ RelkitWith with kitb

relkitSetSource :: (B.TokenListing a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit he (B.Sourced _ core)) =
    Relkit he $ B.Sourced (B.tokenListing src) core


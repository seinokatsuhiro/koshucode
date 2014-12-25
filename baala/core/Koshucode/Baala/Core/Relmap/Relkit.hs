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
  
    RelkitFlow,
    RelkitHook',
    RelkitBinary,
    RelkitConfl,

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
    relkitCopy,
    relkitNest,
    relkitNestVar,
    relkitSetSource,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Lexmap  as C



-- ----------------------  Datatype

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitHead :: Maybe B.Head
    , relkitBody :: RelkitBody c
    }

instance B.Monoid (Relkit c) where
    mempty = relkitConst B.reldee
    mappend (Relkit _ bo1) (Relkit he2 bo2) =
        relkit he2 $ RelkitAppend bo1 bo2

data RelkitCore c
    = RelkitFull         Bool (                [[c]] -> [[c]] )
    | RelkitOneToMany    Bool (                 [c]  -> [[c]] )
    | RelkitOneToOne     Bool (                 [c]  ->  [c]  )
    | RelkitPred              (                 [c]  -> Bool  )

    | RelkitAbFull       Bool ( [Relbmap c] -> [[c]] -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [Relbmap c] ->  [c]  -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [Relbmap c] ->  [c]  -> B.Ab  [c]  ) [RelkitBody c]
    | RelkitAbSemi            (                [[c]] -> B.Ab Bool  ) (RelkitBody c)
    | RelkitAbPred            (                 [c]  -> B.Ab Bool  )

    | RelkitAppend       (RelkitBody c) (RelkitBody c)
    | RelkitConst        [[c]]
    | RelkitId

    | RelkitSource       B.JudgePat [B.TermName]

    | RelkitLink         C.RopName RelkitKey (Maybe (RelkitBody c))
    | RelkitNestVar      C.RopName
    | RelkitCopy         C.RopName (RelkitBody c)
    | RelkitNest         [(String, Int)] (RelkitBody c)

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   = "RelkitFull"
    show (RelkitOneToMany   _ _)   = "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   = "RelkitOneToOne"
    show (RelkitPred          _)   = "RelkitPred"

    show (RelkitAbFull    _ _ _)   = "RelkitAbFull"
    show (RelkitOneToAbMany _ _ _) = "RelkitOneToAbMany"
    show (RelkitOneToAbOne _ _ _)  = "RelkitOneToAbOne"
    show (RelkitAbSemi      _ _)   = "RelkitAbSemi"
    show (RelkitAbPred        _)   = "RelkitAbPred"

    show (RelkitConst         _)   = "RelkitConst"
    show (RelkitAppend      x y)   = "RelkitAppend " ++ show [x,y]
    show (RelkitId             )   = "RelkitId"

    show (RelkitSource     p ns)   = "RelkitSource " ++ p ++ " " ++ show ns
    show (RelkitLink      n _ _)   = "RelkitLink " ++ n
    show (RelkitNestVar       n)   = "RelkitNestVar " ++ n
    show (RelkitCopy        _ _)   = "RelkitCopy "
    show (RelkitNest        _ _)   = "RelkitNest "

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Head, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- | Relation selector
type RelSelect c = B.JudgePat -> [String] -> B.Rel c

-- | Mapping for body of relation.
type Relbmap c = [[c]] -> B.Ab [[c]]

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c     = Maybe B.Head -> B.Ab (Relkit c)

-- | Make 'C.Relkit' from hook data and input heading.
type RelkitHook' h c  = h c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c   = Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c    = [Relkit c] -> RelkitFlow c


-- ----------------------  Constructor

relkit :: Maybe B.Head -> RelkitCore c -> Relkit c
relkit he = Relkit he . B.Sourced []

relkitJust :: B.Head -> RelkitCore c -> Relkit c
relkitJust he = relkit $ Just he

relkitNothing :: Relkit c
relkitNothing = relkit Nothing RelkitId

relkitId :: Maybe B.Head -> Relkit c
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

relkitSource :: B.JudgePat -> [B.TermName] -> Relkit c
relkitSource p ns = relkitJust he kit where
    he  = B.headFrom ns
    kit = RelkitSource p ns

relkitCopy :: String -> B.Map (Relkit c)
relkitCopy n (Relkit he kitb) = kit2 where
    kit2 = relkit he $ RelkitCopy n kitb

relkitNest :: [(String, Int)] -> B.Map (Relkit c)
relkitNest nest (Relkit he kitb) = kit2 where
    kit2 = relkit he $ RelkitNest nest kitb

relkitNestVar :: String -> B.Head -> Relkit c
relkitNestVar n he = kit where
    kit = relkitJust he $ RelkitNestVar n

relkitSetSource :: (B.CodePtr a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit he (B.Sourced _ core)) =
    Relkit he $ B.Sourced (B.codePts src) core


{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relkit.Relkit
  ( 
    -- * Datatype for Relkit
    Relkit (..), RelkitCore (..),
  
    -- * Derived types
    RelkitBody, RelkitKey, RelkitDef,
    BodyMap, RelSelect,

    -- * Calculation type
    RelkitFlow, RelkitHook', RelkitBinary, RelkitConfl,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Data         as B
import qualified Koshucode.Baala.Core.Lexmap  as C



-- ----------------------  Datatype

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitInput  :: Maybe B.Head
    , relkitOutput :: Maybe B.Head
    , relkitBody   :: RelkitBody c
    }

data RelkitCore c
    = RelkitFull         Bool (                B.Map     [[c]] )
    | RelkitOneToMany    Bool (                B.ManyMap [c]   )
    | RelkitOneToOne     Bool (                B.Map     [c]   )
    | RelkitPred              (                B.Pred    [c]   )

    | RelkitAbFull       Bool ( [BodyMap c] -> B.AbMap     [[c]]   ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [BodyMap c] -> B.AbManyMap [c]     ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [BodyMap c] -> B.AbMap     [c]     ) [RelkitBody c]
    | RelkitAbSemi            (                B.AbPred    [[c]]   ) (RelkitBody c)
    | RelkitAbPred            (                B.AbPred    [c]     )

    | RelkitAppend       (RelkitBody c) (RelkitBody c)
    | RelkitConst        [[c]]
    | RelkitId

    | RelkitSource       B.JudgePat [B.TermName]

    | RelkitLink         C.RopName RelkitKey (Maybe (RelkitBody c))
    | RelkitCopy         B.Token C.RopName (RelkitBody c)
    | RelkitNestVar      B.Token C.RopName
    | RelkitNest         B.Token [(String, Int)] (RelkitBody c)

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
    show (RelkitCopy      _ _ _)   = "RelkitCopy "
    show (RelkitNestVar     _ n)   = "RelkitNestVar " ++ n
    show (RelkitNest      _ _ _)   = "RelkitNest "

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Head, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- | Relation selector
type RelSelect c = B.JudgePat -> [String] -> B.Rel c

-- | Mapping for body of relation.
type BodyMap c = B.AbMap [[c]]

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c     = Maybe B.Head -> B.Ab (Relkit c)

-- | Make 'C.Relkit' from hook data and input heading.
type RelkitHook' h c  = h c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c   = Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c    = [Relkit c] -> RelkitFlow c

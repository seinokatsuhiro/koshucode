{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relkit.Relkit
  ( 
    -- * Datatype for Relkit
    Relkit (..), RelkitBody, RelkitCore (..),
  
    -- * Derived types
    RelkitTable, RelkitKey,
    BodyMap,

    -- * Calculation type
    RelkitFlow, RelkitHook', RelkitBinary, RelkitConfl,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core.Lexmap  as C



-- ----------------------  Datatype

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitInput  :: Maybe D.Head     -- ^ Input heading
    , relkitOutput :: Maybe D.Head     -- ^ Output heading
    , relkitBody   :: RelkitBody c     -- ^ Calculation function
    }

-- | Body of relkit.
type RelkitBody c = B.Sourced (RelkitCore c)

-- | Calculation of relation-to-relation mapping.
data RelkitCore c
    = RelkitFull         Bool (                O.Map     [[c]] )
    | RelkitOneToMany    Bool (                O.ManyMap [c]   )
    | RelkitOneToOne     Bool (                O.Map     [c]   )
    | RelkitPred              (                O.Test    [c]   )

    | RelkitAbFull       Bool ( [BodyMap c] -> B.AbMap     [[c]]   ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [BodyMap c] -> B.AbManyMap [c]     ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [BodyMap c] -> B.AbMap     [c]     ) [RelkitBody c]
    | RelkitAbSemi            (                B.AbTest    [[c]]   ) (RelkitBody c)
    | RelkitAbTest            (                B.AbTest    [c]     )

    | RelkitAppend       (RelkitBody c) (RelkitBody c)
    | RelkitConst        [[c]]
    | RelkitId

    | RelkitSource       D.JudgeClass [S.TermName]

    | RelkitLink         C.RopName RelkitKey (Maybe (RelkitBody c))
    | RelkitCopy         S.Token C.RopName (RelkitBody c)
    | RelkitLocal        S.Token C.RopName
    | RelkitNest         S.Token [S.TermIndex] (RelkitBody c)

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   = "RelkitFull"
    show (RelkitOneToMany   _ _)   = "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   = "RelkitOneToOne"
    show (RelkitPred          _)   = "RelkitPred"

    show (RelkitAbFull    _ _ _)   = "RelkitAbFull"
    show (RelkitOneToAbMany _ _ _) = "RelkitOneToAbMany"
    show (RelkitOneToAbOne _ _ _)  = "RelkitOneToAbOne"
    show (RelkitAbSemi      _ _)   = "RelkitAbSemi"
    show (RelkitAbTest        _)   = "RelkitAbTest"

    show (RelkitConst         _)   = "RelkitConst"
    show (RelkitAppend      x y)   = "RelkitAppend " ++ show [x,y]
    show (RelkitId             )   = "RelkitId"

    show (RelkitSource     p ns)   = "RelkitSource " ++ p ++ " " ++ show ns
    show (RelkitLink      n _ _)   = "RelkitLink " ++ n
    show (RelkitCopy      _ _ _)   = "RelkitCopy "
    show (RelkitLocal       _ n)   = "RelkitLocal " ++ n
    show (RelkitNest      _ _ _)   = "RelkitNest "

-- | Relkit search table.
type RelkitTable c = [(RelkitKey, Relkit c)]

-- | Search key of relkit.
type RelkitKey = (Maybe D.Head, [C.Lexmap])

-- | Mapping for body of relation.
type BodyMap c = B.AbMap [[c]]

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c     = Maybe D.Head -> B.Ab (Relkit c)

-- | Make 'C.Relkit' from hook data and input heading.
type RelkitHook' h c  = h c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c   = Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c    = [Relkit c] -> RelkitFlow c

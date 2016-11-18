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
      -- ^ /Safe flow:/ Arbitrary relation mapping, i.e.,
      --   mapping from multiple tuples to multiple tuples.
    | RelkitOneToMany    Bool (                O.ManyMap [c]   )
      -- ^ /Safe flow:/ Mapping from single tuple to multiple tuples, include no tuples.
    | RelkitOneToOne     Bool (                O.Map     [c]   )
      -- ^ /Safe flow:/ Mapping from single tuple to single tuple.
    | RelkitTest              (                O.Test    [c]   )
      -- ^ /Safe flow:/ Filter tuples
    | RelkitAbTest            (                B.AbTest    [c]     )
      -- ^ /Abortable flow:/ Filter by calculation.
    | RelkitAbSemi            (                B.AbTest    [[c]]   ) (RelkitBody c)
      -- ^ /Abortable flow:/ Filter by data.
    | RelkitAbFull       Bool ( [BodyMap c] -> B.AbMap     [[c]]   ) [RelkitBody c]
      -- ^ /Abortable confluence:/ Multiple to multiple.
    | RelkitOneToAbMany  Bool ( [BodyMap c] -> B.AbManyMap [c]     ) [RelkitBody c]
      -- ^ /Abortable confluence:/ Single to multiple.
    | RelkitOneToAbOne   Bool ( [BodyMap c] -> B.AbMap     [c]     ) [RelkitBody c]
      -- ^ /Abortable confluence:/ Single to single.

    | RelkitAppend       (RelkitBody c) (RelkitBody c)
      -- ^ /Connection:/ Append two mapping.
    | RelkitId
      -- ^ /Connection:/ Identity mapping, i.e.,
      --   output input relation without modification.
    | RelkitConst        [[c]]
      -- ^ /Source:/ Ignore input, and output constant relation.
    | RelkitSource       D.JudgeClass [S.TermName]
      -- ^ /Source:/ Ignore input, and output relation from data source.

    | RelkitLink         C.RopName RelkitKey (Maybe (RelkitBody c))
      -- ^ /Reference:/ Link point to other relmap.
    | RelkitNest         S.Token [S.TermIndex] (RelkitBody c)
      -- ^ /Reference:/ Give indecies to nested relations.
    | RelkitCopy         S.Token C.RopName (RelkitBody c)
      -- ^ /Reference:/ Give a name to input relation.
    | RelkitLocal        S.Token C.RopName
      -- ^ /Reference:/ Local relation reference, i.e., @^/r@ or @^r@.

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   = "RelkitFull"
    show (RelkitOneToMany   _ _)   = "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   = "RelkitOneToOne"
    show (RelkitTest          _)   = "RelkitTest"

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

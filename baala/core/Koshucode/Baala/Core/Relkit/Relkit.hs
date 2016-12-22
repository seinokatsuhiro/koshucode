{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relkit.Relkit
  ( 
    -- * Datatype for Relkit
    Relkit (..), 
    pattern RelkitOutput,
    RelkitBody, RelkitCore (..),

    -- * Derived types
    RelkitTable, RelkitKey,
    Flow, FlowAb, Confl, BodyMap,

    -- * Calculation type
    RelkitFlow, RelkitHook', RelkitBinary, RelkitConfl,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Type         as T
import qualified Koshucode.Baala.Core.Lexmap  as C



-- ----------------------  Datatype

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitInput  :: Maybe T.Head     -- ^ Input heading
    , relkitOutput :: Maybe T.Head     -- ^ Output heading
    , relkitBody   :: RelkitBody c     -- ^ Calculation function
    }

-- | Relkit of output heading and its body.
pattern RelkitOutput he bo <- Relkit _ (Just he) bo

-- | Body of relkit.
type RelkitBody c = B.Codic (RelkitCore c)

-- | Calculation of relation-to-relation mapping.
data RelkitCore c
    = RelkitFull     Bool ( Flow [[c]] [[c]] )
                     -- ^ __Safe flow:__ Arbitrary relation mapping, i.e.,
                     --   mapping from multiple tuples to multiple tuples.
    | RelkitMany     Bool ( Flow  [c]  [[c]] )
                     -- ^ __Safe flow:__ Mapping from single tuple to multiple tuples,
                     --   include no tuples.
    | RelkitLine     Bool ( Flow  [c]   [c]  )
                     -- ^ __Safe flow:__ Mapping from single tuple to single tuple.

    | RelkitTest          ( O.Test    [c] )
                     -- ^ __Safe flow:__ Filter tuples
    | RelkitAbTest        ( B.AbTest  [c] )
                     -- ^ __Abortable flow:__ Filter by calculation.
    | RelkitAbSemi        ( B.AbTest  [[c]] ) (RelkitBody c)
                     -- ^ __Abortable flow:__ Filter by data.

    | RelkitAbFull   Bool ( Confl c [[c]] [[c]] ) [RelkitBody c]
                     -- ^ __Abortable confluence:__ Multiple to multiple.
    | RelkitAbMany   Bool ( Confl c  [c]  [[c]] ) [RelkitBody c]
                     -- ^ __Abortable confluence:__ Single to multiple.
    | RelkitAbLine  Bool ( Confl c  [c]   [c] ) [RelkitBody c]
                     -- ^ __Abortable confluence:__ Single to single.

    | RelkitAppend   (RelkitBody c) (RelkitBody c)
                     -- ^ __Connection:__ Append two mapping.
    | RelkitId
                     -- ^ __Connection:__ Identity mapping, i.e.,
                     --   output input relation without modification.
    | RelkitConst    [[c]]
                     -- ^ __Source:__ Ignore input, and output constant relation.
    | RelkitSource   T.JudgeClass [S.TermName]
                     -- ^ __Source:__ Ignore input, and output relation from data source.

    | RelkitLink     C.RopName RelkitKey (Maybe (RelkitBody c))
                     -- ^ __Reference:__ Link point to other relmap.
    | RelkitNest     S.Token [S.IndexTerm] (RelkitBody c)
                     -- ^ __Reference:__ Give indecies to nested relations.
    | RelkitCopy     S.Token C.RopName (RelkitBody c)
                     -- ^ __Reference:__ Give a name to input relation.
    | RelkitLocal    S.Token S.LocalRef
                     -- ^ __Reference:__ Local relation reference, i.e., @^/r@ or @^r@.

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   = "RelkitFull"
    show (RelkitMany        _ _)   = "RelkitMany"
    show (RelkitLine        _ _)   = "RelkitLine"
    show (RelkitTest          _)   = "RelkitTest"

    show (RelkitAbFull    _ _ _)   = "RelkitAbFull"
    show (RelkitAbMany    _ _ _)   = "RelkitAbMany"
    show (RelkitAbLine    _ _ _)   = "RelkitAbLine"
    show (RelkitAbSemi      _ _)   = "RelkitAbSemi"
    show (RelkitAbTest        _)   = "RelkitAbTest"

    show (RelkitConst         _)   = "RelkitConst"
    show (RelkitAppend      x y)   = "RelkitAppend " ++ show [x,y]
    show (RelkitId             )   = "RelkitId"

    show (RelkitSource     p ns)   = "RelkitSource " ++ p ++ " " ++ show ns
    show (RelkitLink      n _ _)   = "RelkitLink " ++ n
    show (RelkitCopy      _ _ _)   = "RelkitCopy "
    show (RelkitLocal       _ n)   = "RelkitLocal " ++ B.name n
    show (RelkitNest      _ _ _)   = "RelkitNest "

-- | Relkit search table.
type RelkitTable c = [(RelkitKey, Relkit c)]

-- | Search key of relkit.
type RelkitKey = (Maybe T.Head, [C.Lexmap])

-- | Flow mapping.
type Flow from to = from -> to

-- | Abortable flow mapping.
type FlowAb from to = from -> B.Ab to

-- | Confluent mapping.
type Confl c from to = [BodyMap c] -> from -> B.Ab to

-- | Mapping for body of relation.
type BodyMap c = B.AbMap [[c]]

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c     = Maybe T.Head -> B.Ab (Relkit c)

-- | Make 'C.Relkit' from hook data and input heading.
type RelkitHook' h c  = h c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c   = Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c    = [Relkit c] -> RelkitFlow c


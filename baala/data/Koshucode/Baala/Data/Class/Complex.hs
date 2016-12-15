{-# OPTIONS_GHC -Wall #-}

-- | Complex content type.

module Koshucode.Baala.Data.Class.Complex
  ( -- * Complex contents

    -- ** Collection
    -- *** List
    CList (..), pTextList,
    -- *** Set
    CSet (..), pTermSet, pTextSet, gSetSort, isMember,

    -- ** Relational
    -- *** Tie
    CTie (..),
    -- *** Relation
    CRel (..), dee, dum,
    -- *** Interpretation
    CInterp (..),

    -- ** Type
    CType (..),
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as D
import qualified Koshucode.Baala.Data.Class.Edge         as D
import qualified Koshucode.Baala.Data.Class.Simple       as D


-- ============================================  Complex contents

-- ---------------------------------  List

-- | List of contents.
class (D.CTypeOf c) => CList c where
    isList      ::       c -> Bool
    gList       ::       c -> [c]
    pList       ::     [c] -> c

    getList     ::  D.GetContent [c] c
    getList     =   D.getContent isList gList

    putList     ::     [c] -> B.Ab c
    putList     =    Right . pList

-- | Create list of text contents.
pTextList :: (D.CText c, CList c) => [String] -> c
pTextList = pList . map D.pText

-- ---------------------------------  Set

-- | Set of contents.
class (D.CTypeOf c) => CSet c where
    isSet       ::          c -> Bool
    gSet        ::          c -> [c]
    pSet        ::        [c] -> c

    getSet      ::     D.GetContent [c] c
    getSet      =      D.getContent isSet gSet

    putSet      ::        [c] -> B.Ab c
    putSet      =       Right . pSet

-- | Create set of term contents.
pTermSet :: (D.CTerm c, CSet c) => [S.TermName] -> c
pTermSet = pSet . map D.pTerm

-- | Create set of text contents.
pTextSet :: (D.CText c, CSet c) => [String] -> c
pTextSet = pSet . map D.pText

-- | Sorted version of 'gSet'.
gSetSort :: (Ord c, CSet c) => c -> [c]
gSetSort = B.sort . gSet

-- | Test membership between element and collection contents.
isMember :: (Eq c, CSet c, CList c) => c -> c -> Bool
isMember x xs | isSet xs  = x `elem` gSet xs
isMember x xs | isList xs = x `elem` gList xs
isMember _ _ = False

-- ---------------------------------  Tie

-- | Tie of terms.
class (D.CTypeOf c) => CTie c where
    isTie       ::           c -> Bool
    gTie        ::           c -> [S.Term c]
    pTie        ::  [S.Term c] -> c

    getTie      ::      D.GetContent [S.Term c] c
    getTie      =       D.getContent isTie gTie

    putTie      ::  [S.Term c] -> B.Ab c
    putTie      =  Right . pTie

-- ---------------------------------  Rel

-- | Relation of terms.
class (D.CTypeOf c) => CRel c where
    isRel       ::           c -> Bool
    gRel        ::           c -> D.Rel c
    pRel        ::     D.Rel c -> c

    getRel      ::      D.GetContent (D.Rel c) c
    getRel      =       D.getContent isRel gRel

    putRel      ::     D.Rel c -> B.Ab c
    putRel      =      Right . pRel

-- | The nullary full relation.
dee :: (CRel c) => c
dee = pRel D.reldee

-- | The nullary empty relation.
dum :: (CRel c) => c
dum = pRel D.reldum

-- ---------------------------------  Interp

-- | Data intepretation.
class (D.CTypeOf c) => CInterp c where
    isInterp    ::           c -> Bool
    gInterp     ::           c -> D.Interp
    pInterp     ::    D.Interp -> c

    getInterp   ::      D.GetContent D.Interp c
    getInterp   =       D.getContent isInterp gInterp

    putInterp   ::    D.Interp -> B.Ab c
    putInterp   =     Right . pInterp

-- ---------------------------------  Type

-- | Type of content.
class (D.CTypeOf c) => CType c where
    isType      ::           c -> Bool
    gType       ::           c -> D.Type
    pType       ::      D.Type -> c

    getType     ::      D.GetContent D.Type c
    getType     =       D.getContent isType gType

    putType     ::      D.Type -> B.Ab c
    putType     =       Right . pType


{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Type
  ( -- * Type
    Type (..),
    NamedType,
    typeExplain,
    typeTermMix,

    -- * Utility
    -- ** Construct
    typeFlatRel,
    typeConsRel,
    typeConsNest,
    typeAppendRel,

    -- ** Select
    typeRelTermNames,
    typeRelDegree,
    typeTerms,
    isTypeRel,
    typeRelIndex,
    --typeRelIndexList,

    -- ** Modify
    typeRelMapTerms,
    typeRelMapTerm,
    typeRelMapName,

    -- $Types
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data.Type.Judge   as D

-- | Type for types.
data Type
    = TypeAny                     -- ^ Everything
    | TypeType                    -- ^ Type of types
    | TypeTerm                    -- ^ Term name
    | TypeInterp                  -- ^ Data interpreation

    | TypeEmpty                   -- ^ Empty
    | TypeEnd                     -- ^ End
    | TypeBool                    -- ^ Boolean
    | TypeText                    -- ^ Text
    | TypeCode                    -- ^ Code
    | TypeDec                     -- ^ Decimal
    | TypeClock   (Maybe String)  -- ^ Clock
    | TypeTime    (Maybe String)  -- ^ Time
    | TypeBin                     -- ^ Binary data

    | TypeList    Type            -- ^ List
    | TypeSet     Type            -- ^ Set
    | TypeTag     String Type     -- ^ Tagged type
    | TypeTie     [NamedType]     -- ^ Tie
    | TypeRel     [NamedType]     -- ^ Relation

    | TypeTuple   [Type]          -- ^ Tuple (Product type)
    | TypeSum     [Type]          -- ^ Sum type
      deriving (Show, Eq, Ord)

type NamedType = B.Named Type

instance B.MixEncode Type where
    mixEncode = typeToMix

typeToMix :: Type -> B.MixText
typeToMix = wf where
    wf = w False    -- no parens
    wt = w True     -- with parens

    w _ TypeAny                = B.mixString "any"
    w _ TypeEmpty              = B.mixString "empty"
    w _ TypeEnd                = B.mixString "end"
    w _ TypeBool               = B.mixString "boolean"
    w _ TypeText               = B.mixString "text"
    w _ TypeCode               = B.mixString "code"
    w _ TypeDec                = B.mixString "decimal"
    w _ (TypeClock (Nothing))  = B.mixString "clock"
    w _ (TypeClock (Just p))   = B.mixString "clock" `B.mixSep` B.mixString p
    w _ (TypeTime (Nothing))   = B.mixString "time"
    w _ (TypeTime (Just p))    = B.mixString "time"  `B.mixSep` B.mixString p
    w _ TypeBin                = B.mixString "binary"
    w _ TypeTerm               = B.mixString "term"
    w _ TypeType               = B.mixString "type"
    w _ TypeInterp             = B.mixString "interp"

    w _ (TypeList    t)        = B.mixString "list" `B.mixSep` wt t
    w _ (TypeSet     t)        = B.mixString "set"  `B.mixSep` wt t
    w _ (TypeTag tag t)        = B.mixString "tag"  `B.mixSep` B.mixString tag
                                                    `mappend`  B.mixString ":"
                                                    `B.mixSep` wt t

    w q (TypeTie    ts)        = wrap q (B.mixString "tie" `B.mixSep` termTypes ts)
    w q (TypeRel    ts)        = wrap q (B.mixString "rel" `B.mixSep` termTypes ts)
    w _ (TypeTuple  ts)        = B.mixString "tuple" `B.mixSep` (B.mixJoin1 $ map wt ts)
    w q (TypeSum ts)           = wrap q (B.mixJoinBar $ map wf ts)

    wrap :: Bool -> B.MixText -> B.MixText
    wrap False xs              = xs
    wrap True  xs              = B.mixBracketS "(" ")" xs

    termTypes = B.mixJoin1 . map termType

    termType :: NamedType -> B.MixText
    termType (n, t) = D.termNameToMix n `B.mixSep` wt t

-- | Print type as tree.
typeExplain :: Type -> B.Doc
typeExplain ty =
    case ty of
      TypeList     t  -> B.doc "list"   B.<+> typeExplain t
      TypeSet      t  -> B.doc "set"    B.<+> typeExplain t
      TypeTag  tag t  -> B.doc "tag"    B.<+> B.doc (tag ++ ":") B.<+> typeExplain t
      TypeTie     ts  -> B.doc "tie"    B.<+> vmap term ts
      TypeRel     ts  -> B.doc "rel"    B.<+> vmap term ts
      TypeTuple   ts  -> B.doc "tuple"  B.<+> vmap (item ":") ts
      TypeSum     ts  -> B.doc "sum"    B.<+> vmap (item "|") ts
      _               -> B.doc $ B.mixToFlatString $ B.mixEncode ty
    where
      term (n,t)  =  B.doc ('/' : n) B.<+> typeExplain t
      item i t    =  B.doc i B.<+> typeExplain t
      vmap f      =  B.docv . map f

-- | Encode term types.
typeTermMix :: Type -> B.MixText
typeTermMix (TypeRel ts) = B.mixJoin1 $ map name ts where
    name (n, _) = B.mix $ S.showTermName n
typeTermMix _ = B.mixEmpty


-- --------------------------------------------  Relation utilities

-- ----------------------  Construct

-- | Create relation type from term names.
typeFlatRel :: [S.TermName] -> Type
typeFlatRel ns = TypeRel $ map term ns where
    term n = (n, TypeAny)

-- | Add term name to relation type.
typeConsRel :: S.TermName -> B.Map Type
typeConsRel n = typeConsNest n TypeAny

-- | Add term name and type to relation type.
typeConsNest :: S.TermName -> Type -> B.Map Type
typeConsNest n t (TypeRel ts) = TypeRel $ (n, t) : ts
typeConsNest _ _ t = t

-- | Add term names to relation type.
typeAppendRel :: [S.TermName] -> B.Map Type
typeAppendRel ns (TypeRel ts) = TypeRel $ map (, TypeAny) ns ++ ts where
typeAppendRel _ t = t

-- ----------------------  Select

-- | Get term names from relation type.
typeRelTermNames :: Type -> [S.TermName]
typeRelTermNames (TypeRel ts) = map fst ts
typeRelTermNames _ = []

-- | Get degree of relation type.
typeRelDegree :: Type -> Int
typeRelDegree (TypeRel ts) = length ts
typeRelDegree _ = 0

-- | Get named type from relation type or tie type.
typeTerms :: Type -> [NamedType]
typeTerms (TypeRel ts) = ts
typeTerms (TypeTie ts) = ts
typeTerms _            = []

-- | Test type is relational.
isTypeRel :: Type -> Bool
isTypeRel (TypeRel _)  = True
isTypeRel _            = False

-- ----------------------  Index

-- | Calculate term index.
--
--   >>> typeRelIndex (typeFlatRel ["a", "b", "c"]) ["b"]
--   [1]
--
--   >>> typeRelIndex (typeFlatRel ["a", "b", "c"]) ["e"]
--   [-1]
--
--   >>> typeRelIndex (typeConsNest "r" (typeFlatRel ["a", "b"]) (typeFlatRel [])) ["r", "b"]
--   [0, 1]
--
typeRelIndex :: Type -> S.TermPath -> [Int]
typeRelIndex (TypeRel ts) p = loop ts p 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop ((n1, TypeRel ts') : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)
    loop ((n1, _) : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
typeRelIndex _ _ = []

-- typeRelIndexList :: Type -> [S.TermPath] -> [[Int]]
-- typeRelIndexList = map . typeRelIndex

-- ----------------------  Modify

-- | Modify terms of relation type.
typeRelMapTerms :: B.Map [NamedType] -> B.Map Type
typeRelMapTerms f (TypeRel ts) = TypeRel $ f ts
typeRelMapTerms _ t = t

-- | Modify term of relation type.
typeRelMapTerm :: B.Map NamedType -> B.Map Type
typeRelMapTerm f t = typeRelMapTerms (map f) t

-- | Modify term name of relation type.
typeRelMapName :: B.Map S.TermName -> B.Map Type
typeRelMapName = typeRelMapTerm . B.mapFst



-- ------------------------------------------------------------------
-- $Types
--
--  [Empty]     Empty means that there are no values.
--              i.e., universal negation on the term holds.
--              Textual form is the non-quoted parens: @()@.
--
--  [Boolean]   Boolean used for something is hold or unhold.
--              Textual forms: @\<0\>@ (false), @\<1\>@ (true).
--
--  [Text]      Sequence of characters.
--              Textual forms is chars with apostrophe or
--              doubly-quoted line: @\'abc@, @\"abc def\"@.
--
--  [Decimal]   Decimal number.
--              Textual forms is sequence of digits:
--              @100@, @99.50@, @hex AF@.
--
--  [Set]       Set is an unordered collection of contents.
--              Duplication among contents is not significant.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in braces:
--              @{ \'a | \'b | \'c }@.
--
--  [List]      List is an ordered list of contents.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in square brackets:
--              @[ \'abc | \'def ]@.
--
--  [Tie]       Tie is an bundle of terms,
--              i.e., a list of named contents.
--              Textual form is a sequence of terms
--              with hyphen-braces: @{- \/a 10 \/b 20 -}@.
--
--  [Relation]  Relation is a set of same-type tuples,
--              Textual form is a sequence of tuples
--              enclosed in equal-braces.
--              The first tuple is a heading of relation,
--              and succeeding tuples are delimited by vertical bar:
--              @{= \/a \/b [ \'A1 | 20 ][ \'A3 | 40 ] =}@.
--

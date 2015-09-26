{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module Koshucode.Baala.Data.Type.Type
  ( Type (..),
    NamedType,
    typeExplain,
    typeFlatRel,
    typeConsRel,
    typeConsNest,
    typeAppendRel,
    typeRelTermNames,
    typeRelDegree,
    typeRelIndex,
    typeRelIndexList,
    typeTermDoc,
    typeTerms,
    isTypeRel,
    typeRelMapTerms, typeRelMapTerm, typeRelMapName,
    -- $Types
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Token    as B

-- | Type for types.
data Type
    = TypeAny                     -- ^ Everything
    | TypeType                    -- ^ Type of types
    | TypeTerm                    -- ^ Term name
    | TypeInterp                  -- ^ Data interpreation

    | TypeEmpty                   -- ^ Empty
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
    | TypeAssn    [NamedType]     -- ^ Association
    | TypeRel     [NamedType]     -- ^ Relation

    | TypeTuple   [Type]          -- ^ Tuple (Product type)
    | TypeSum     [Type]          -- ^ Sum type
      deriving (Show, Eq, Ord)

type NamedType = B.Named Type

instance B.Write Type where
    writeDocWith _ = writeType

writeType :: Type -> B.Doc
writeType = wf where
    wt = w True
    wf = w False

    w _ TypeAny                = B.doc "any"
    w _ TypeEmpty              = B.doc "empty"
    w _ TypeBool               = B.doc "boolean"
    w _ TypeText               = B.doc "text"
    w _ TypeCode               = B.doc "code"
    w _ TypeDec                = B.doc "decimal"
    w _ (TypeClock (Nothing))  = B.doc "clock"
    w _ (TypeClock (Just p))   = B.doc "clock" B.<+> B.doc p
    w _ (TypeTime (Nothing))   = B.doc "time"
    w _ (TypeTime (Just p))    = B.doc "time" B.<+> B.doc p
    w _ TypeBin                = B.doc "binary"
    w _ TypeTerm               = B.doc "term"
    w _ TypeType               = B.doc "type"
    w _ TypeInterp             = B.doc "interp"

    w _ (TypeList    t)        = B.doc "list"  B.<+> wt t
    w _ (TypeSet     t)        = B.doc "set"   B.<+> wt t
    w _ (TypeTag tag t)        = B.doc "tag"   B.<+> B.doc (tag ++ ":") B.<+> wt t

    w q (TypeAssn   ts)        = wrap q $ B.doc "assn"  B.<+> B.writeTerms wt ts
    w q (TypeRel    ts)        = wrap q $ B.doc "rel"   B.<+> B.writeTerms wt ts

    w _ (TypeTuple  ts)        = B.doc "tuple" B.<+> B.doch (map wt ts)
    w q (TypeSum ts)           = wrap q $ B.doch $ B.writeSepsWith wf "|" ts

    wrap False x               = x
    wrap True  x               = B.docWraps "(" ")" x

-- | Print type as tree.
typeExplain :: Type -> B.Doc
typeExplain ty =
    case ty of
      TypeList     t  ->  B.doc "list"   B.<+>  typeExplain t
      TypeSet      t  ->  B.doc "set"    B.<+>  typeExplain t
      TypeTag  tag t  ->  B.doc "tag"    B.<+>  B.doc (tag ++ ":") B.<+> typeExplain t
      TypeAssn    ts  ->  B.doc "assn"   B.<+>  vmap term ts
      TypeRel     ts  ->  B.doc "rel"    B.<+>  vmap term ts
      TypeTuple   ts  ->  B.doc "tuple"  B.<+>  vmap (item ":") ts
      TypeSum     ts  ->  B.doc "sum"    B.<+>  vmap (item "|") ts
      _               ->  writeType ty
    where
      term (n,t)  =  B.doc ('/' : n) B.<+> typeExplain t
      item i t    =  B.doc i B.<+> typeExplain t
      vmap f      =  B.docv . map f


-- ----------------------  Relation utilities

typeFlatRel :: [B.TermName] -> Type
typeFlatRel ns = TypeRel $ map term ns where
    term n = (n, TypeAny)

typeConsRel :: B.TermName -> B.Map Type
typeConsRel n (TypeRel ts) = TypeRel $ (n, TypeAny) : ts
typeConsRel _ t = t

typeConsNest :: B.TermName -> Type -> B.Map Type
typeConsNest n t (TypeRel ts) = TypeRel $ (n, t) : ts
typeConsNest _ _ t = t

typeAppendRel :: [B.TermName] -> B.Map Type
typeAppendRel ns (TypeRel ts) = TypeRel $ map (, TypeAny) ns ++ ts where
typeAppendRel _ t = t

typeRelTermNames :: Type -> [B.TermName]
typeRelTermNames (TypeRel ts) = map fst ts
typeRelTermNames _ = []

typeRelDegree :: Type -> Int
typeRelDegree (TypeRel ts) = length ts
typeRelDegree _ = 0

-- | Term index
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
typeRelIndex :: Type -> B.TermPath -> [Int]
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

typeRelIndexList :: Type -> [B.TermPath] -> [[Int]]
typeRelIndexList = map . typeRelIndex

typeTermDoc :: Type -> B.Doc
typeTermDoc (TypeRel ts) = B.doch $ map name ts where
    name (n, _) = B.doc $ B.showTermName n
typeTermDoc _ = B.docEmpty

typeTerms :: Type -> [NamedType]
typeTerms (TypeRel  ts) = ts
typeTerms (TypeAssn ts) = ts
typeTerms _             = []

isTypeRel :: Type -> Bool
isTypeRel (TypeRel _)  =  True
isTypeRel _            =  False

typeRelMapTerms :: B.Map [NamedType] -> B.Map Type
typeRelMapTerms f (TypeRel ts) = TypeRel $ f ts
typeRelMapTerms _ t = t

typeRelMapTerm :: B.Map NamedType -> B.Map Type
typeRelMapTerm f t = typeRelMapTerms (map f) t

typeRelMapName :: B.Map B.TermName -> B.Map Type
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
--              @{ \'a : \'b : \'c }@.
--
--  [List]      List is an ordered list of contents.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in square brackets:
--              @[ \'abc : \'def ]@.
--
--  [Assn]      Assn is an association of terms,
--              i.e., a list of named contents.
--              Textual form is a sequence of terms
--              with bar-angles: @\<\< \/a 10 \/b 20 \>\>@.
--
--  [Relation]  Relation is a set of same-type tuples,
--              Textual form is a sequence of tuples
--              enclosed in bar-braces.
--              The first tuple is a heading of relation,
--              and succeeding tuples are delimited by vertical bar:
--              @{| \/a : \/b | \'A1 : 20 | \'A3 : 40 |}@.
--

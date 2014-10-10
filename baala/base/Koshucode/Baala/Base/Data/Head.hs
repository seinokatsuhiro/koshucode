{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Head
( -- * Type
  Head (..),
  headExplain,
  headExplainLines,
  headExplainDoc,

  -- * Constructor
  headEmpty,
  headFrom,
  headWords,
  headNames,
  headDegree,
  -- $ConstructorExample

  -- * Predicate
  headEquiv,
  isSubhead, isSuperhead,

  -- * Add terms
  headConsNest,
  headCons,
  headAppend,
  headNests,
  -- $AddTermExample

  -- * Utility
  headMapTerms,
  headChange,
  headRename,
  headIndex1, headIndex, 
  headAlign, bodyAlign,
  headNested,
  headUp,
  -- $UtilityExample
) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Term    as B
import qualified Koshucode.Baala.Base.Data.Type    as B


-- ---------------------- Type

-- | Heading of relation as a list of terms
data Head =
    Head { headTerms :: [B.Term]
            , headType  :: B.Type
            } deriving (Show, Eq, Ord)

instance B.Monoid Head where
    mempty = headEmpty
    mappend Head { headTerms = t1 } Head { headTerms = t2 } =
        headEmpty { headTerms = B.unionUp t1 t2 }

instance B.Write Head where
    write _ Head { headTerms = ts } =
        B.typeTermDoc $ termsToType $ ts

headExplainLines :: Head -> [String]
headExplainLines = lines . headExplain

headExplain :: Head -> String
headExplain = show . headExplainDoc

headExplainDoc :: Head -> B.Doc
headExplainDoc Head { headTerms = ts } = B.typeDoc $ termsToType ts

termsToType :: [B.Term] -> B.Type
termsToType = B.TypeRel . map term where
    term (B.TermFlat n)    = (n, B.TypeAny)
    term (B.TermNest n ts) = (n, termsToType ts)

typeToTerms :: B.Type -> [B.Term]
typeToTerms (B.TypeRel ts) = map term ts where
    term (n, B.TypeRel ts2) = B.TermNest n $ map term ts2
    term (n, _) = B.TermFlat n
typeToTerms _ = []



-- ----------------------  Constructor

-- $ConstructorExample
--
--  /Examples/
--
--  Make heading of @\/a@ @\/b@.
--
--    >>> headFrom ["a", "b"]
--    Head [TermFlat "a", TermFlat "b"]
--
--  Show heading.
--
--    >>> B.doc $ headFrom ["a", "b"]
--    /a : /b
--
--  Term names of heading.
--
--    >>> let h = headFrom ["a", "b"] in headNames h
--    ["a", "b"]
--
--  Degree of relation.
--
--    >>> let h = headFrom ["a", "b"] in headDegree h
--    2
--

-- | Make head from term names.
headFrom :: [B.TermName] -> Head
headFrom ns = Head { headTerms = map B.TermFlat ns
                   , headType  = B.typeFlatRel ns }

-- | Empty heading, i.e., no terms in heading.
headEmpty :: Head
headEmpty = headFrom []

headWords :: String -> Head
headWords = headFrom . words

-- | List of term names.
headNames :: Head -> [B.TermName]
headNames = B.typeRelTermNames . termsToType . headTerms

-- | Degree of relation, i.e., number of terms.
headDegree :: Head -> Int
headDegree = B.typeRelDegree . termsToType . headTerms



-- ----------------------  Predicate

headEquiv :: Head -> Head -> Bool
headEquiv Head { headTerms = a } Head { headTerms = b } = B.sort a == B.sort b

isSubhead :: Head -> Head -> Bool
isSubhead he1 he2 = null $ headNames he1 `B.snipLeft` headNames he2 

isSuperhead :: Head -> Head -> Bool
isSuperhead he1 he2 = isSubhead he2 he1



-- ----------------------  Add terms

-- $AddTermExample
--
--  Add term @\/c@ to heading.
--
--    >>> let h = headFrom ["a", "b"] in headCons "c" h
--    Head [TermFlat "c", TermFlat "a", TermFlat "b"]
--

headConsNest :: B.TermName -> Head -> B.Map Head
headConsNest n1 Head { headTerms = ns1, headType = t1 }
                Head { headTerms = ns, headType = t } =
    Head { headTerms = B.TermNest n1 ns1 : ns
         , headType  = B.typeConsNest n1 t1 t }

-- | Add term name to head.
headCons :: B.TermName -> B.Map Head
headCons n1 h@Head { headTerms = ns, headType = t } =
    h { headTerms = B.TermFlat n1 : ns
      , headType  = B.typeConsRel n1 t }

-- | Add term names to head.
headAppend :: [B.TermName] -> B.Map Head
headAppend ns1 h@Head { headTerms = ns, headType = t } =
    h { headTerms = map B.TermFlat ns1 ++ ns
      , headType  = B.typeAppendRel ns1 t }

headNests :: [B.TermName] -> B.Map Head
headNests ns1 Head { headTerms = ns, headType = t } =
    Head { headTerms = map nest1 ns1
         , headType  = B.TypeRel $ map nest2 ns1 }
    where nest1 n = B.TermNest n ns
          nest2 n = (n, t)



-- ----------------------  Utility

-- $UtilityExample
--
--  /Examples/
--
--  Reverse term names.
--
--    >>> let h = headFrom ["a", "b"] in headChange reverse h
--    Head [TermFlat "b", TermFlat "a"]
--
--  Calculate indices of heading.
--
--    >>> let h = headFrom ["a", "b"] in headIndex1 h ["a"]
--    [0]
--    >>> let h = headFrom ["a", "b"] in headIndex h [["a"], ["b"], ["c"]]
--    [[0], [1], [-1]]
--
--  Heading of @\/a@ @\/b@ is a subhead of @\/a@ @\/b@ @\/c@, and @\/a@ @\/e@ is not.
--
--    >>> headFrom ["a", "b"] `isSubhead` headFrom ["a", "b", "c"]
--    True
--    >>> headFrom ["a", "e"] `isSubhead` headFrom ["a", "b", "c"]
--    False
--

-- | Reconstruct head.
headChange :: B.Map [B.Term] -> B.Map Head
headChange f h@Head { headTerms = ts } = h { headTerms = f ts }

headMapTerms :: B.Map [B.NamedType] -> B.Map Head
headMapTerms f h@Head { headTerms = ts } =
    let ty = B.typeRelMapTerms f $ termsToType ts
    in h { headTerms = typeToTerms ty
         , headType  = ty }

headRename :: B.Map B.TermName -> B.Map Head
headRename f h@Head { headTerms = ts } =
    let ty = B.typeRelMapName f $ termsToType ts
    in h { headTerms = typeToTerms ty
         , headType  = ty }

-- | Index of a term.
headIndex1 :: Head -> B.TermPath -> [Int]
headIndex1 = B.typeRelIndex . termsToType . headTerms

-- | Indices of terms.
headIndex :: Head -> [B.TermPath] -> [[Int]]
headIndex = B.typeRelIndexList . termsToType .headTerms

headAlign :: Head -> Head -> B.Map [c]
headAlign to from = B.snipOrder (headNames to) (headNames from)

bodyAlign :: Head -> Head -> B.Map [[c]]
bodyAlign h1 h2 = (headAlign h1 h2 `map`)

headNested :: Head -> [(String, Head)]
headNested Head { headTerms = ts1 } = ts2 where
    ts2 = map h $ filter (B.isTypeRel . snd) $ B.typeTerms $ termsToType ts1
    h (n, t) = (n, headEmpty { headTerms = typeToTerms t, headType = t })

headUp :: B.Map Head
headUp Head { headTerms = [B.TermNest _ ts] } = headEmpty { headTerms = ts }
headUp he = he


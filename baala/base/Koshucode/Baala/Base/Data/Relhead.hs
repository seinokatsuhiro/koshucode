{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Type
  Relhead (..),
  headFrom,
  headCons, headCons2, headCons3,
  headChange,
  headNames,
  headDegree,

  -- * Other functions
  headExistTerms,
  headNonExistTerms,
  headIndex, headIndex1,

  -- * Monoid
  M.mempty,
  M.mappend,
) where

import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Syntax       as B
import qualified Koshucode.Baala.Base.Data.Relterm as B



-- ---------------------- Type

{-| Heading of relation as a list of terms -}
data Relhead = Relhead {
      headTerms :: [B.Relterm]
    } deriving (Show, Eq, Ord)

instance M.Monoid Relhead where
    mempty = Relhead []
    mappend (Relhead t1) (Relhead t2) =
        Relhead $ B.unionUp t1 t2

{-| >>> doc $ headFrom ["/a", "/b"]
    /a : /b  -}
instance B.Pretty Relhead where
    doc (Relhead ts) = B.docColon ts

{-| Make head from termnames.

    >>> headFrom ["/a", "/b"]
    Relhead [Term "/a", Term "/b"]  -}
headFrom :: [B.Termname] -> Relhead
headFrom = Relhead . map B.Term

{-| Add term to head.

    >>> let h = headFrom ["/a", "/b"] in headCons "/c" h
    Relhead [Term "/c", Term "/a", Term "/b"]  -}
headCons :: B.Termname -> B.Map Relhead
headCons n1 (Relhead ns) =
    Relhead $ B.Term n1 : ns

headCons2 :: B.Termname -> B.Termname -> B.Map Relhead
headCons2 n1 n2 (Relhead ns) =
    Relhead $ B.Term n1 : B.Term n2 : ns

headCons3 :: B.Termname -> B.Termname -> B.Termname -> B.Map Relhead
headCons3 n1 n2 n3 (Relhead ns) =
    Relhead $ B.Term n1 : B.Term n2 : B.Term n3 : ns

{-| Reconstruct head.

    >>> let h = headFrom ["/a", "/b"] in headChange reverse h
    Relhead [Term "/b", Term "/a"]  -}
headChange :: (B.Map [B.Termname]) -> B.Map Relhead
headChange f = headFrom . f . headNames

{-| List of term names.

    >>> let h = headFrom ["/a", "/b"] in headNames h
    ["/a", "/b"]  -}
headNames :: Relhead -> [B.Termname]
headNames = B.names . headTerms

{-| Number of terms.

    >>> let h = headFrom ["/a", "/b"] in headDegree h
    2  -}
headDegree :: Relhead -> Int
headDegree = length . headTerms



-- ----------------------  Other functions

{-| Filter keeping terms that exist in head.

    >>> let h = headFrom ["/a", "/b"] in headExistTerms h ["/a", "/c"]
    ["/a"]  -}
headExistTerms :: Relhead -> B.Map [B.Termname]
headExistTerms (Relhead ts) = filter $ nameExist ts

{-| Filter dropping terms that exist in head.

    >>> let h = headFrom ["/a", "/b"] in headNonExistTerms h ["/a", "/c"]
    ["/c"]  -}
headNonExistTerms :: Relhead -> B.Map [B.Termname]
headNonExistTerms (Relhead ts) = filter $ not . nameExist ts

nameExist :: [B.Relterm] -> B.Termname -> Bool
nameExist ts n = B.termExist ts [n]

{-| Index of terms.

    >>> let h = headFrom ["/a", "/b"] in headIndex h [["/a"], ["/b"], ["/c"]]
    [[0], [1], [-1]]  -}
headIndex :: Relhead -> [B.Termpath] -> [[Int]]
headIndex = B.termsIndex . headTerms

{-| Index of a term.

    >>> let h = headFrom ["/a", "/b"] in headIndex1 h ["/a"]
    [0]  -}
headIndex1 :: Relhead -> B.Termpath -> [Int]
headIndex1 = B.termIndex . headTerms


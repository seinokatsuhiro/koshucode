{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Type
  Relhead (..),
  headFrom,
  headConsTerm,
  headCons, headCons2, headCons3,
  headChange,
  headNames,
  headDegree,

  -- * Other functions
  headKeepTerms,
  headDropTerms,
  headIndex, headIndex1,
  isSubhead, isSuperhead, isEqvHead,

  -- * Monoid
  M.mempty,
  M.mappend,
) where

import qualified Data.Monoid as M
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Token        as B
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

headConsTerm :: B.Relterm -> B.Map Relhead
headConsTerm t1 (Relhead ns) = Relhead $ t1 : ns

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

{-| Keep terms that exist in head.

    >>> let h = headFrom ["/b"] in headKeepTerms h ["/a", "/b", "/c"]
    ["/b"]  -}
headKeepTerms :: Relhead -> B.Map [B.Termname]
headKeepTerms (Relhead ts) = filter $ nameExist ts

{-| Drop terms that exist in head.

    >>> let h = headFrom ["/b"] in headDropTerms h ["/a", "/b", "/c"]
    ["/a","/c"]  -}
headDropTerms :: Relhead -> B.Map [B.Termname]
headDropTerms (Relhead ts) = filter $ not . nameExist ts

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

{-| >>> headFrom ["/a", "/b"] `isSubhead` headFrom ["/a", "/b", "/c"]
    True

    >>> headFrom ["/a", "/d"] `isSubhead` headFrom ["/a", "/b", "/c"]
    False
  -}
isSubhead :: Relhead -> Relhead -> Bool
isSubhead h1 h2 = null rest where
    ns1  = headNames h1
    rest = headDropTerms h2 ns1

isSuperhead :: Relhead -> Relhead -> Bool
isSuperhead h1 h2 = isSubhead h2 h1

isEqvHead :: Relhead -> Relhead -> Bool
isEqvHead h1 h2 = isSubhead h1 h2 && isSubhead h2 h1



{-# OPTIONS_GHC -Wall #-}

-- | Terms in heading of relation.

module Koshucode.Baala.Base.Data.Relterm
( Relterm (..),
  Termpath,
  termsIndex,
  termIndex,
  termExist,

  showTermName,
  showNestedTermName,
)
where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Token   as B

-- | Path of term,
--   e.g., term @\/r\/x@ is correspond to path @["\/r", "\/x"]@.
type Termpath = [B.TermName]

-- | Term in heading of relation
data Relterm
    = Term B.TermName             -- ^ For non-relation
    | Nest B.TermName [Relterm]   -- ^ For relation
      deriving (Show, Eq, Ord)

instance B.Name Relterm where
    name (Term s)   = s
    name (Nest s _) = s

instance B.Pretty Relterm where
    doc (Term n)    = B.doc (showTermName n)
    doc (Nest n xs) = B.docWraps "(" ")"
                      (B.doch $ B.doc (showTermName n) : map B.doc xs)

showTermName :: B.Map String
showTermName n = ('/' : n)

showNestedTermName :: [String] -> String
showNestedTermName = concat . map showTermName

-- | Term path to term position
--
--   >>> termIndex [Term "/a", Term "/b", Term "/c"] ["/b"]
--   [1]
--
--   >>> termIndex [Term "/a", Term "/b", Term "/c"] ["/e"]
--   [-1]
--
--   >>> termIndex [Nest "/r" [Term "/a", Term "/b"]] ["/r", "/b"]
--   [0, 1]
--
termIndex :: [Relterm] -> Termpath -> [Int]
termIndex ts p = loop ts p 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop (Term n1 : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
    loop (Nest n1 ts' : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)

termsIndex :: [Relterm] -> [Termpath] -> [[Int]]
termsIndex = map . termIndex

termExist :: [Relterm] -> Termpath -> Bool
termExist ts p = all (>= 0) $ termIndex ts p


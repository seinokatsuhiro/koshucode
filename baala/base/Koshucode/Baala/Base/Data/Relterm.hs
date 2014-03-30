{-# OPTIONS_GHC -Wall #-}

-- | Terms in heading of relation.

module Koshucode.Baala.Base.Data.Relterm
( Relterm (..),
  showTermName,
  showNestedTermName,
  relnestTerms,

  termsIndex,
  termIndex,
  termExist,
  isNested,
)
where

import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B
import qualified Koshucode.Baala.Base.Token   as B

-- | Term in heading of relation
data Relterm
    = Relterm B.TermName             -- ^ For non-relation
    | Relnest B.TermName [Relterm]   -- ^ For relation
      deriving (Show, Eq, Ord)

instance B.Name Relterm where
    name (Relterm s)   = s
    name (Relnest s _) = s

instance B.Pretty Relterm where
    doc (Relterm n)    = B.doc (showTermName n)
    doc (Relnest n xs) = B.docWraps "(" ")"
                         (B.doch $ B.doc (showTermName n) : map B.doc xs)

showTermName :: B.Map String
showTermName n = ('/' : n)

showNestedTermName :: [String] -> String
showNestedTermName = concat . map showTermName

relnestTerms :: Relterm -> [Relterm]
relnestTerms (Relnest _ ts) = ts
relnestTerms (Relterm _)    = B.bug "flat term"

-- | Term path to term position
--
--   >>> termIndex [Term "/a", Term "/b", Term "/c"] ["/b"]
--   [1]
--
--   >>> termIndex [Term "/a", Term "/b", Term "/c"] ["/e"]
--   [-1]
--
--   >>> termIndex [Relnest "/r" [Term "/a", Term "/b"]] ["/r", "/b"]
--   [0, 1]
--
termIndex :: [Relterm] -> B.TermPath -> [Int]
termIndex ts p = loop ts p 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop (Relterm n1 : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
    loop (Relnest n1 ts' : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)

termsIndex :: [Relterm] -> [B.TermPath] -> [[Int]]
termsIndex = map . termIndex

termExist :: [Relterm] -> B.TermPath -> Bool
termExist ts p = all (>= 0) $ termIndex ts p

isNested :: Relterm -> Bool
isNested (Relnest _ _) = True
isNested _             = False


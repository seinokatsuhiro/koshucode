{-# OPTIONS_GHC -Wall #-}

-- | Term names in relation.

module Koshucode.Baala.Base.Data.Term
(
  -- * Type
  Term (..),
  isTermNest,
  termName,
  termNest,

  -- * Utility
  termExplainDoc,
  showTermName,
  showNestedTermName,
  termChange,
  termsIndex,
  termIndex,
)
where

import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B
import qualified Koshucode.Baala.Base.Token   as B


-- ---------------------------------  Type

data Term
    = TermFlat B.TermName          -- ^ Term name for non-relation
    | TermNest B.TermName [Term]   -- ^ Term name for relation
      deriving (Show, Eq, Ord)

instance B.Name Term where
    name (TermFlat s)   = s
    name (TermNest s _) = s

instance B.ShortDoc Term where
    shortDoc sh term =
        case term of
          TermFlat n    -> sd1 (showTermName n)
          TermNest n xs -> B.docWraps "(" ")"
                           $ B.shortDocH sh $ sd1 (showTermName n) : map sd2 xs
        where sd1 = B.shortDoc sh
              sd2 = B.shortDoc sh

-- | Test that term is nested.
isTermNest :: Term -> Bool
isTermNest (TermNest _ _) = True
isTermNest _              = False

-- | Get name part from term.
termName :: Term -> B.TermName
termName (TermFlat n)   = n
termName (TermNest n _) = n

-- | Get nested part from term.
termNest :: Term -> [Term]
termNest (TermNest _ ts) = ts
termNest (TermFlat _)    = B.bug "flat term"


-- ---------------------------------  Doc

termExplainDoc :: Term -> B.Doc
termExplainDoc (TermFlat n)    = B.doc (showTermName n)
termExplainDoc (TermNest n ts) = B.doc (showTermName n)
                                B.<+> (B.docv $ map termExplainDoc ts)

showTermName :: B.Map String
showTermName n = ('/' : n)

showNestedTermName :: [String] -> String
showNestedTermName = concat . map showTermName

termChange :: (B.Map B.TermName) -> B.Map Term
termChange f (TermFlat n)    = TermFlat (f n)
termChange f (TermNest n ts) = TermNest (f n) ts


-- ---------------------------------  Index

-- | Term path to term position
--
--   >>> termIndex [TermFlat "a", TermFlat "b", TermFlat "c"] ["b"]
--   [1]
--
--   >>> termIndex [TermFlat "a", TermFlat "b", TermFlat "c"] ["e"]
--   [-1]
--
--   >>> termIndex [TermNest "r" [TermFlat "a", TermFlat "b"]] ["r", "b"]
--   [0, 1]
--
termIndex :: [Term] -> B.TermPath -> [Int]
termIndex ts p = loop ts p 0 where
    loop _ [] _ = []
    loop [] _ _ = [-1]
    loop (TermFlat n1 : ts2) nns@(n2 : _) i
        | n1 == n2  = [i]
        | otherwise = loop ts2 nns (i + 1)
    loop (TermNest n1 ts' : ts2) nns@(n2 : ns) i
        | n1 == n2  = i : loop ts' ns 0
        | otherwise = loop ts2 nns (i + 1)

termsIndex :: [Term] -> [B.TermPath] -> [[Int]]
termsIndex = map . termIndex


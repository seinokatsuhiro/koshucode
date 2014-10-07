{-# OPTIONS_GHC -Wall #-}

-- | Term names in relation.

module Koshucode.Baala.Base.Data.Term
(
  Term (..),
  isTermNest,
  termName,
  termNest,
  termChange,
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

instance B.Write Term where
    write sh term =
        case term of
          TermFlat n    -> sd1 (B.showTermName n)
          TermNest n xs -> B.docWraps "(" ")"
                           $ B.writeH sh $ sd1 (B.showTermName n) : map sd2 xs
        where sd1 = B.write sh
              sd2 = B.write sh

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

termChange :: B.Map B.TermName -> B.Map Term
termChange f (TermFlat n)    = TermFlat (f n)
termChange f (TermNest n ts) = TermNest (f n) ts


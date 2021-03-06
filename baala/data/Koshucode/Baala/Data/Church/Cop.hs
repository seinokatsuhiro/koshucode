{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content operator.

module Koshucode.Baala.Data.Church.Cop
  ( -- * Operator
    Cop (..), CopCox, CopTree, copName, 
  
    -- * Operator name
    copNormal, copInternal, copPrefix, copInfix, copPostfix,
  
    -- * Operator set
    CopSet (..), CopFind,
    copsetFill,
    GetCops (..),
  ) where

import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Data.Church.Cox  as D


-- ----------------------  Operator

-- | Term-content operator.
data Cop c
    = CopCalc S.BlankName (D.CopCalc c)     -- ^ Convert @c@ (content)
    | CopCox  S.BlankName (CopCox c)        -- ^ Convert 'D.Cox' c
    | CopTree S.BlankName (CopTree S.Chars) -- ^ Convert 'B.TTree'

-- | Expression-level syntax.
type CopCox c = [D.Cox c] -> D.AbCox c

-- | Tree-level syntax.
type CopTree t = [S.TTree t] -> B.Ab (S.TTree t)

instance Show (Cop c) where
    show (CopCalc n _) = "(CopCalc " ++ show n ++ " _)"
    show (CopCox  n _) = "(CopCox "  ++ show n ++ " _)"
    show (CopTree n _) = "(CopTree " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name = B.name . copName

-- | Name of content operator.
copName :: Cop c -> S.BlankName
copName (CopCalc  n _) = n
copName (CopCox   n _) = n
copName (CopTree  n _) = n


-- ----------------------  Operator name

-- | Name for non-binary operator.
copNormal :: String -> S.BlankName
copNormal = S.BlankNormal

-- | Name for program-generated operator.
copInternal :: String -> S.BlankName
copInternal = S.BlankInternal

-- | Name for prefix operator.
copPrefix :: String -> S.BlankName
copPrefix = S.BlankPrefix

-- | Name for postfix operator.
copPostfix :: String -> S.BlankName
copPostfix = S.BlankPostfix

-- | Name for infix operator.
copInfix :: String -> S.BlankName
copInfix = S.BlankInfix


-- ----------------------  Operator set

-- | Set of content operators.
data CopSet c = CopSet
    { copsetCopList    :: [Cop c]
    , copsetInfixList  :: [(S.Chars, B.InfixHeight)]

    , copsetCalcList   :: [B.Named (Cop c)]  -- ^ CopCalc
    , copsetCoxList    :: [B.Named (Cop c)]  -- ^ CopCox
    , copsetTreeList   :: [B.Named (Cop c)]  -- ^ CopTree

    , copsetFindCalc   :: CopFind (D.Cox c)   -- ^ This is created by 'copsetFill'.
    , copsetFindCox    :: CopFind (CopCox c)  -- ^ This is created by 'copsetFill'.
    , copsetFindTree   :: CopFind (CopTree S.Chars)  -- ^ This is created by 'copsetFill'.

    , copsetDerived    :: [D.NamedCox c]
    }

-- | Empty operator set.
instance B.Default (CopSet c) where
    def = copset

instance Monoid (CopSet c) where
    mempty = copset
    mappend s1 s2 = copsetFill $ copset
                    { copsetCopList   = appendOn copsetCopList   s1 s2
                    , copsetInfixList = appendOn copsetInfixList s1 s2
                    , copsetCalcList  = appendOn copsetCalcList  s1 s2
                    , copsetCoxList   = appendOn copsetCoxList   s1 s2
                    , copsetTreeList  = appendOn copsetTreeList  s1 s2
                    , copsetDerived   = appendOn copsetDerived   s1 s2
                    }

appendOn :: (a -> [b]) -> a -> a -> [b]
appendOn f x y = f x ++ f y

-- | Find content operator from its name.
type CopFind f = S.BlankName -> Maybe f

-- | Empty operator set.
copset :: CopSet c
copset = CopSet [] [] [] [] [] O.nothing O.nothing O.nothing []

-- | Complete operator set.
copsetFill :: O.Map (CopSet c)
copsetFill opset = opset2 where

    opset2 = opset { copsetFindCalc  =  contFind
                   , copsetFindCox   =  coxFind
                   , copsetFindTree  =  treeFind }

    contFind    =  B.assocFinder contList
    coxFind     =  B.assocFinder coxList
    treeFind    =  B.assocFinder treeList

    contList    =  B.catMaybes $ map cont cops
    coxList     =  B.catMaybes $ map cox  cops
    treeList    =  B.catMaybes $ map tree cops

    cont (CopCalc n f)   =  Just (n, D.CoxCalc [] n f)
    cont _               =  Nothing

    cox (CopCox n f)     =  Just (n, f)
    cox _                =  Nothing

    tree (CopTree n f)   =  Just (n, f)
    tree _               =  Nothing

    cops  = copsetCopList opset

-- | Get set of content operators.
class GetCops a where
    getCops :: a c -> CopSet c

instance GetCops CopSet where
    getCops = id


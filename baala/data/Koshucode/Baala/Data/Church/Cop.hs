{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content operator.

module Koshucode.Baala.Data.Church.Cop
  ( -- * Operator
    Cop (..), CopCox, CopTree, copName, 
  
    -- * Operator name
    copNormal, copInternal, copPrefix, copInfix, copPostfix,
  
    -- * Operator set
    CopSet (..), CopFind, copset, copsetFill,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Data.Token       as D
import qualified Koshucode.Baala.Data.Church.Cox  as D


-- ----------------------  Operator

-- | Term-content operator.
data Cop c
    = CopCalc D.BlankName (D.CopCalc c) -- ^ Convert @c@ (content)
    | CopCox  D.BlankName (CopCox c)    -- ^ Convert 'D.Cox' c
    | CopTree D.BlankName (CopTree)     -- ^ Convert 'B.TTree'

-- | Expression-level syntax.
type CopCox c = [D.Cox c] -> B.Ab (D.Cox c)

-- | Tree-level syntax.
type CopTree  = [D.TTree] -> B.Ab D.TTree

instance Show (Cop c) where
    show (CopCalc n _) = "(CopCalc " ++ show n ++ " _)"
    show (CopCox  n _) = "(CopCox "  ++ show n ++ " _)"
    show (CopTree n _) = "(CopTree " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name = B.name . copName

copName :: Cop c -> D.BlankName
copName (CopCalc  n _) = n
copName (CopCox   n _) = n
copName (CopTree  n _) = n


-- ----------------------  Operator name

-- | Name for non-binary operator.
copNormal :: String -> D.BlankName
copNormal = D.BlankNormal

-- | Name for program-generated operator.
copInternal :: String -> D.BlankName
copInternal = D.BlankInternal

-- | Name for prefix operator.
copPrefix :: String -> D.BlankName
copPrefix = D.BlankPrefix

-- | Name for postfix operator.
copPostfix :: String -> D.BlankName
copPostfix = D.BlankPostfix

-- | Name for infix operator.
copInfix :: String -> D.BlankName
copInfix = D.BlankInfix


-- ----------------------  Operator set

data CopSet c = CopSet
    { copsetCopList    :: [Cop c]
    , copsetInfixList  :: [B.Named B.InfixHeight]

    , copsetCalcList   :: [B.Named (Cop c)]  -- CopCalc
    , copsetCoxList    :: [B.Named (Cop c)]  -- CopCox
    , copsetTreeList   :: [B.Named (Cop c)]  -- CopTree

    , copsetFindCalc   :: CopFind (D.Cox c)
    , copsetFindCox    :: CopFind (CopCox c)
    , copsetFindTree   :: CopFind CopTree

    , copsetDerived    :: [D.NamedCox c]
    }

-- | Find content operator from its name.
type CopFind f = D.BlankName -> Maybe f

-- | Empty operator set.
copset :: CopSet c
copset = CopSet [] [] [] [] [] find find find [] where
    find _ = Nothing

copsetFill :: B.Map (CopSet c)
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


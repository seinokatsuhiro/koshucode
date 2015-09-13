{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content operator.

module Koshucode.Baala.Core.Church.Cop
  ( -- * Operator
    Cop (..), CopCox, CopTree, copName, 
  
    -- * Operator name
    copNormal, copInternal, copPrefix, copInfix, copPostfix,
  
    -- * Operator set
    CopSet (..), CopFind, copset, copsetFill,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Core.Church.Cox  as C


-- ----------------------  Operator

-- | Term-content operator.
data Cop c
    = CopCalc B.BlankName (C.CopCalc c) -- ^ Convert @c@ (content)
    | CopCox  B.BlankName (CopCox c)    -- ^ Convert 'C.Cox' c
    | CopTree B.BlankName (CopTree)     -- ^ Convert 'B.TTree'

-- | Expression-level syntax.
type CopCox c = [C.Cox c] -> B.Ab (C.Cox c)

-- | Tree-level syntax.
type CopTree  = [B.TTree] -> B.Ab B.TTree

instance Show (Cop c) where
    show (CopCalc n _) = "(CopCalc " ++ show n ++ " _)"
    show (CopCox  n _) = "(CopCox "  ++ show n ++ " _)"
    show (CopTree n _) = "(CopTree " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name = B.name . copName

copName :: Cop c -> B.BlankName
copName (CopCalc  n _) = n
copName (CopCox   n _) = n
copName (CopTree  n _) = n


-- ----------------------  Operator name

-- | Name for non-binary operator.
copNormal :: String -> B.BlankName
copNormal = B.BlankNormal

-- | Name for program-generated operator.
copInternal :: String -> B.BlankName
copInternal = B.BlankInternal

-- | Name for prefix operator.
copPrefix :: String -> B.BlankName
copPrefix = B.BlankPrefix

-- | Name for postfix operator.
copPostfix :: String -> B.BlankName
copPostfix = B.BlankPostfix

-- | Name for infix operator.
copInfix :: String -> B.BlankName
copInfix = B.BlankInfix


-- ----------------------  Operator set

data CopSet c = CopSet
    { copsetCopList    :: [Cop c]
    , copsetInfixList  :: [B.Named B.InfixHeight]

    , copsetCalcList   :: [B.Named (Cop c)]  -- CopCalc
    , copsetCoxList    :: [B.Named (Cop c)]  -- CopCox
    , copsetTreeList   :: [B.Named (Cop c)]  -- CopTree

    , copsetFindCalc   :: CopFind (C.Cox c)
    , copsetFindCox    :: CopFind (CopCox c)
    , copsetFindTree   :: CopFind CopTree

    , copsetDerived    :: [C.NamedCox c]
    }

-- | Find content operator from its name.
type CopFind f = B.BlankName -> Maybe f

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

    cont (CopCalc n f)   =  Just (n, C.CoxCalc [] n f)
    cont _               =  Nothing

    cox (CopCox n f)     =  Just (n, f)
    cox _                =  Nothing

    tree (CopTree n f)   =  Just (n, f)
    tree _               =  Nothing

    cops  = copsetCopList opset


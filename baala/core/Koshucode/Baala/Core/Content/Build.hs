{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Build
( coxBuild, coxDebruijn,
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Cox       as C
import qualified Koshucode.Baala.Core.Content.Literal   as C
import qualified Koshucode.Baala.Core.Message           as Message


-- | Construct content expression from token tree
coxBuild :: forall c. (C.CContent c)
  => ([C.Cop c], [B.Named B.InfixHeight]) -> B.TokenTree -> B.Ab (C.Cox c)
coxBuild (syn, htab) =
    convCox syn           -- convert cox to cox
      B.<=< Right
      . debruijn          -- attach De Bruijn indicies
      . unlist            -- expand multiple variables/arguments
      B.<=< construct     -- construct content expression from token tree
      B.<=< prefix htab   -- convert infix operator to prefix
      B.<=< convTree syn  -- convert token tree to token tree

debruijn :: B.Map (C.Cox c)
debruijn = de [] where
    de :: [String] -> B.Map (C.Cox c)
    de vars cox =
        case cox of
          C.CoxVar cp v _     ->  maybe cox (C.CoxVar cp v) $ indexFrom 1 v vars
          C.CoxApplyL _ _ _   ->  C.mapToCox (de vars) cox
          C.CoxDeriv _ _ v _  ->  C.mapToCox (de $ v : vars) cox
          _                   ->  cox

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

unlist :: B.Map (C.Cox c)
unlist = derivL . (C.mapToCox unlist) where
    derivL :: B.Map (C.Cox c)
    derivL e =
        case e of
          C.CoxDerivL _ _ []     b -> b
          C.CoxDerivL cp tag (v : vs) b
              -> let sub = derivL $ C.CoxDerivL cp tag vs b
                 in C.CoxDeriv cp tag v sub
          _ -> e

convCox :: forall c. [C.Cop c] -> B.AbMap (C.Cox c)
convCox syn = expand where
    assn :: [B.Named (C.Cop c)]
    assn = map B.named syn

    expand :: B.AbMap (C.Cox c)
    expand cox =
        case cox of
            C.CoxDeriv  cp tag n  x -> Right . C.CoxDeriv  cp tag n  =<< expand x
            C.CoxDerivL cp tag ns x -> Right . C.CoxDerivL cp tag ns =<< expand x
            C.CoxApplyL cp f@(C.CoxVar _ n 0) xs ->
                case lookup n assn of
                  Just (C.CopCox _ g) -> expand =<< g xs
                  _                   -> expandApply cp f xs
            C.CoxApplyL cp f xs       -> expandApply cp f xs
            _                         -> Right cox

    expandApply cp f xs =
        do f'  <- expand f
           xs' <- mapM expand xs
           Right $ C.CoxApplyL cp f' xs'
    
-- construct content expression from token tree
construct :: forall c. (C.CContent c) => B.TokenTree -> B.Ab (C.Cox c)
construct = expr where
    expr tree = 
        B.abortableTree "cox-build" tree $
         let cp = concatMap B.codePoint $ B.front $ B.untree tree
         in cons cp tree

    -- function application
    cons :: [B.CodePoint] -> B.TokenTree -> B.Ab (C.Cox c)
    cons cp (B.TreeB 1 _ (fun : args)) =
        do fun'  <- expr fun
           args' <- expr `mapM` args
           Right $ C.CoxApplyL cp fun' args'

    -- function abstruction
    cons cp (B.TreeB 6 _ [vars, b1]) =
        do b2 <- expr b1
           let (tag, vars') = untag vars
           let vs = map B.tokenContent $ B.untree vars'
           Right $ C.CoxDerivL cp tag vs b2
    cons _ (B.TreeB 6 _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- literal or variable
    cons cp tree@(B.TreeL tok) =
        case tok of
          B.TTerm _ ns   ->  Right $ C.CoxTerm cp ns []
          B.TText _ _ v  ->  case C.litContent tree of
                               Right c -> Right $ C.CoxLit cp c
                               Left _  -> Right $ C.CoxVar cp v 0
          _              ->  B.bug "core/leaf"

    -- literal composite
    cons cp tree@(B.TreeB n _ _) | n > 1 = fmap (C.CoxLit cp) $ C.litContent tree
    cons _ (B.TreeB n _ _) = Message.unkCox $ show n

    untag :: B.TokenTree -> (Maybe String, B.TokenTree)
    untag (B.TreeB l p (B.TreeL (B.TText _ 1 tag) : vars))
               = (Just tag, B.TreeB l p $ vars)
    untag vars = (Nothing, vars)

-- convert from infix to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap (B.TokenTree)
prefix htab tree =
    B.abortableTree "cox-prefix" tree $
     case B.infixToPrefix conv ht tree of
       Right tree3 -> Right $ B.undouble (== 1) tree3
       Left  xs    -> Message.ambInfixes $ map detail xs
    where
      conv :: B.Map B.Token
      conv (B.TText cp i s) = B.TText cp i $ ("&" ++) s
      conv x = x

      ht :: B.Token -> B.InfixHeight
      ht = B.infixHeight wordText htab

      wordText :: B.Token -> Maybe String
      wordText (B.TText _ 0 w) = Just w
      wordText _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

-- expand syntax operator
convTree :: forall c. [C.Cop c] -> B.AbMap B.TokenTree
convTree syn = expand where
    assoc :: [B.Named (C.Cop c)]
    assoc = map B.named syn

    expand tree@(B.TreeB 1 p subtrees) =
        case subtrees of
          op@(B.TreeL (B.TText _ 0 name)) : args
              -> B.abortableTree "cox-syntax" tree $
                 case lookup name assoc of
                   Just (C.CopTree _ f) -> expand =<< f args
                   _                    -> do args2 <- mapM expand args
                                              Right $ B.TreeB 1 p (op : args2)
          _ -> do sub2 <- mapM expand subtrees
                  Right $ B.TreeB 1 p sub2

    expand (B.TreeB 6 p trees) =
        case B.divideTreesBy "|" trees of
          [vars, b1] -> do b2 <- expand $ B.treeWrap b1
                           Right $ B.TreeB 6 p [B.treeWrap vars, b2]
          _ -> Message.unkCox "abstruction"
    expand tree = Right tree

coxDebruijn :: B.Map (C.Cox c)
coxDebruijn = comer . deepen . unlist where

comer :: B.Map (C.Cox c)
comer = de [] where
    de :: [String] -> B.Map (C.Cox c)
    de vars cox =
        case cox of
          C.CoxVar cp v i
              | i >  0        ->  C.CoxVar cp v i
              | i == 0        ->  maybe cox (C.CoxVar cp v) $ indexFrom 1 v vars
          C.CoxApplyL _ _ _   ->  C.mapToCox (de vars) cox
          C.CoxDeriv _ _ v _  ->  C.mapToCox (de $ v : vars) cox
          _                   ->  cox

deepen :: B.Map (C.Cox c)
deepen = level (0 :: Int) where
    level n (C.CoxDeriv cp tag v e)
        = C.CoxDeriv cp tag v $ level (n + 1) e
    level n cox = de n [] cox

    de :: Int -> [String] -> B.Map (C.Cox c)
    de n vars cox =
        case cox of
          C.CoxVar cp v i
              | i == 0        ->  C.CoxVar cp v 0
              | i >  0        ->  if v `elem` vars
                                  then C.CoxVar cp v i        -- bound variable
                                  else C.CoxVar cp v (i + n)  -- free variable
          C.CoxApplyL _ _ _   ->  C.mapToCox (de n vars) cox
          C.CoxDeriv _ _ v _  ->  C.mapToCox (de n $ v : vars) cox
          _                   ->  cox


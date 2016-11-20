{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Data.Church.Build
  ( treeCox, coxForm,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax                 as S
import qualified Koshucode.Baala.Data.Class             as D
import qualified Koshucode.Baala.Data.Decode            as D
import qualified Koshucode.Baala.Data.Church.Cop        as D
import qualified Koshucode.Baala.Data.Church.Cox        as D
import qualified Koshucode.Baala.Data.Church.Message    as Msg

import Koshucode.Baala.Syntax.TTree.Pattern

-- | Construct content expression from token tree.
treeCox :: forall c. (D.CContent c)
  => D.CalcContent c -> D.CopSet c -> S.TTree -> B.Ab (D.Cox c)
treeCox calc copset =
    convCox findCox            -- convert cox to cox
      B.<.> Right
      . debruijn               -- attach De Bruijn indicies
      . coxUnfold              -- expand multiple-blank form
      B.<.> construct calc     -- construct content expression from token tree
      B.<.> prefix htab        -- convert infix operator to prefix
      B.<.> convTree findTree  -- convert token tree to token tree
    where
      findCox  = D.copsetFindCox   copset
      findTree = D.copsetFindTree  copset
      htab     = D.copsetInfixList copset

debruijn :: O.Map (D.Cox c)
debruijn = index [] where
    index :: [String] -> O.Map (D.Cox c)
    index vars cox = case cox of
       D.CoxBlank cp n     -> let v = B.name n
                              in maybe cox (D.CoxLocal cp v) $ indexFrom 1 v vars
       D.CoxFill _ _ _     -> D.coxCall cox (index vars)
       D.CoxForm1 _ _ v _  -> D.coxCall cox (index $ v : vars)
       _                   -> cox

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

coxUnfold :: O.Map (D.Cox c)
coxUnfold = unfold . D.coxMap coxUnfold where
    unfold :: O.Map (D.Cox c)
    unfold cox = case cox of
        D.CoxForm _ _ [] b -> unfold b
        D.CoxForm cp tag (v : vs) b
            -> let sub = unfold $ D.CoxForm cp tag vs b
               in D.CoxForm1 cp tag v sub
        _   -> cox

convCox :: forall c. D.CopFind (D.CopCox c) -> B.AbMap (D.Cox c)
convCox find = expand where
    expand :: B.AbMap (D.Cox c)
    expand (D.CoxForm1 cp tag n  x) = Right . D.CoxForm1 cp tag n  =<< expand x
    expand (D.CoxForm  cp tag ns x) = Right . D.CoxForm  cp tag ns =<< expand x
    expand (D.CoxFill  cp f@(D.CoxBlank _ n) xs)
                                    = case find n of
                                        Just g -> expand =<< g xs
                                        _      -> fill cp f xs
    expand (D.CoxFill  cp f xs)     = fill cp f xs
    expand cox                      =  Right cox

    fill cp f xs = do f'  <- expand f
                      xs' <- mapM expand xs
                      Right $ D.CoxFill cp f' xs'
    
-- construct content expression from token tree
construct :: forall c. (D.CContent c) => D.CalcContent c -> S.TTree -> B.Ab (D.Cox c)
construct calc = expr where
    expr tree = Msg.abCoxBuild tree $
         let cp = concatMap B.codePtList $ B.takeFirst $ B.untree tree
         in cons cp tree

    cons :: [B.CodePt] -> S.TTree -> B.Ab (D.Cox c)
    cons cp tree@(B S.BracketGroup subtrees)
         = case subtrees of
             f@(L (S.TText _ q w)) : xs
                 | q == S.TextRaw && isName w -> fill cp f xs
                 | otherwise -> lit cp tree

             -- term with options (experimental)
             (L (S.TTermPath _ ns)) : _
                 -> Right $ D.CoxTerm cp ns []

             -- fill args in the blanks (application)
             f : xs -> fill cp f xs
             []     -> lit cp tree

    -- form with blanks (function)
    cons cp (B S.BracketForm [vars, b1]) =
        do b2 <- expr b1
           let (tag, vars') = untag vars
           let vs = map S.tokenContent $ B.untree vars'
           Right $ D.CoxForm cp tag vs b2
    cons _ (B S.BracketForm trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- compound literal
    cons cp tree@(B _ _) = lit cp tree

    -- literal or variable
    cons cp tree@(L tok) = case tok of
        S.TTermN _ n               -> Right $ D.CoxTerm  cp [S.toTermName n] []
        S.TTermPath _ ns           -> Right $ D.CoxTerm  cp ns []
        S.TName _ op               -> Right $ D.CoxBlank cp op
        S.TTextRaw _ n | isName n  -> Right $ D.CoxBlank cp $ S.BlankNormal n
        S.TText _ _ _              -> lit cp tree
        S.TTermQ _ _               -> lit cp tree
        _                          -> B.bug "core/leaf"

    cons _ _ = B.bug "core"

    fill cp f xs =
        do f'  <- expr f
           xs' <- expr `mapM` xs
           Right $ D.CoxFill cp f' xs'

    lit cp tree  = fmap (D.CoxLit cp) $ D.treeContent (calc' tree) tree
    calc' tree tree' | tree' == tree  = Msg.unkCox "Neither literal nor calculable"
                     | otherwise      = calc tree'

    untag :: S.TTree -> (D.CoxTag, S.TTree)
    untag (B.TreeB l p (S.TextLeafQ _ tag : vars))
                = (Just tag, B.TreeB l p $ vars)
    untag vars  = (Nothing, vars)

isName :: O.Test String
isName (c:_)  = isNameFirst c
isName _      = False

isNameFirst :: O.Test Char
isNameFirst c = case O.majorGeneralCategory c of
                  O.UnicodeLetter  -> True
                  O.UnicodeMark    -> True
                  O.UnicodeSymbol  -> True
                  _                -> False

-- convert from infix operator to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap S.TTree
prefix htab tree =
    Msg.abCoxPrefix tree $
     case B.infixToPrefix conv ht (B.TreeB S.BracketGroup Nothing) mapper tree of
       Right tree3 -> Right $ undoubleGroup tree3
       Left  xs    -> Msg.ambInfixes $ map detail xs
    where
      conv = (c D.copPrefix, c D.copInfix, c D.copPostfix)
      c :: (String -> S.BlankName) -> O.Map S.Token
      c f (S.TTextRaw cp s) = S.TName cp $ f s
      c _ x = x

      ht :: S.Token -> B.InfixHeight
      ht = B.infixHeight wordText htab

      wordText :: S.Token -> Maybe String
      wordText (S.TTextRaw _ w) = Just w
      wordText _               = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = S.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

mapper :: B.InfixMapper S.BracketType S.Token
mapper pre = loop where
    loop (B.TreeB S.BracketGroup p xs) =
        do preXs <- pre xs
           Right $ B.TreeB S.BracketGroup p preXs
    loop (B.TreeB S.BracketForm p1 [vs, B.TreeB S.BracketGroup p2 body]) =
        do preBody <- pre body
           let body' = undoubleGroup $ B.TreeB S.BracketGroup p2 preBody
           Right $ B.TreeB S.BracketForm p1 [vs, body']
    loop tree = Right tree

undoubleGroup :: O.Map (B.CodeTree S.BracketType a)
undoubleGroup = B.undouble (== S.BracketGroup)

-- expand tree-level syntax
convTree :: D.CopFind D.CopTree -> B.AbMap S.TTree
convTree find = expand where
    expand tree@(B.TreeB S.BracketGroup p subtrees) =
        case subtrees of
          op@(S.TextLeafRaw _ name) : args
              -> Msg.abCoxSyntax tree $
                 case find $ S.BlankNormal name of
                   Just f -> expand =<< f args
                   _      -> do args2 <- mapM expand args
                                Right $ B.TreeB S.BracketGroup p (op : args2)
          _ -> do sub2 <- mapM expand subtrees
                  Right $ B.TreeB S.BracketGroup p sub2

    expand (B.TreeB S.BracketForm p trees) =
        case S.divideTreesByBar trees of
          [vars, b1] -> do b2 <- expand $ S.ttreeGroup b1
                           Right $ B.TreeB S.BracketForm p [S.ttreeGroup vars, b2]
          _ -> Msg.unkCox "abstruction"

    expand tree = Right tree

-- | Insert fresh form into indexed expression.
coxForm :: [B.CodePt] -> D.CoxTag -> [String] -> O.Map (D.Cox c)
coxForm cp0 tag vs = debruijn . outside [] . coxUnfold . D.CoxForm cp0 tag vs where
    n = length vs
    outside vars cox = case cox of
       D.CoxLocal cp v i    
           | v `elem` vars  -> D.CoxLocal cp v i        -- inside blank
           | otherwise      -> D.CoxLocal cp v (i + n)  -- outside blank
       D.CoxFill _ _ _      -> D.coxCall cox (outside vars)
       D.CoxForm1 _ _ v _   -> D.coxCall cox (outside $ v : vars)
       _                    -> cox


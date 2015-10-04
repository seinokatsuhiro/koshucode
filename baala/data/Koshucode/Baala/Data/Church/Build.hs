{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Data.Church.Build
  ( coxBuild, coxForm,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Data.Token             as D
import qualified Koshucode.Baala.Data.Content           as D
import qualified Koshucode.Baala.Data.Church.Cop        as D
import qualified Koshucode.Baala.Data.Church.Cox        as D
import qualified Koshucode.Baala.Data.Church.Message    as Msg

-- | Construct content expression from token tree
coxBuild :: forall c. (D.CContent c)
  => D.ContentCalc c -> D.CopSet c -> D.TTreeToAb (D.Cox c)
coxBuild calc copset =
    convCox findCox            -- convert cox to cox
      B.<=< Right
      . debruijn               -- attach De Bruijn indicies
      . coxUnfold              -- expand multiple-blank form
      B.<=< construct calc     -- construct content expression from token tree
      B.<=< prefix htab        -- convert infix operator to prefix
      B.<=< convTree findTree  -- convert token tree to token tree
    where
      findCox  = D.copsetFindCox   copset
      findTree = D.copsetFindTree  copset
      htab     = D.copsetInfixList copset

debruijn :: B.Map (D.Cox c)
debruijn = index [] where
    index :: [String] -> B.Map (D.Cox c)
    index vars cox = case cox of
       D.CoxBlank cp n     ->  let v = B.name n
                               in maybe cox (D.CoxLocal cp v) $ indexFrom 1 v vars
       D.CoxFill _ _ _     ->  D.coxCall cox (index vars)
       D.CoxForm1 _ _ v _  ->  D.coxCall cox (index $ v : vars)
       _                   ->  cox

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

coxUnfold :: B.Map (D.Cox c)
coxUnfold = unfold . D.coxMap coxUnfold where
    unfold :: B.Map (D.Cox c)
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
construct :: forall c. (D.CContent c) => D.ContentCalc c -> D.TTreeToAb (D.Cox c)
construct calc = expr where
    expr tree = Msg.abCoxBuild tree $
         let cp = concatMap B.codePtList $ B.front $ B.untree tree
         in cons cp tree

    cons :: [B.CodePt] -> D.TTreeToAb (D.Cox c)
    cons cp tree@(B.TreeB D.BracketGroup _ subtrees)
         = case subtrees of
             f@(D.TextLeaf q _ w) : xs
                 | q == D.TextRaw && isName w -> fill cp f xs
                 | otherwise -> lit cp tree

             -- fill args in the blanks (application)
             f : xs -> fill cp f xs
             []     -> lit cp tree

    -- form with blanks (function)
    cons cp (B.TreeB D.BracketForm _ [vars, b1]) =
        do b2 <- expr b1
           let (tag, vars') = untag vars
           let vs = map D.tokenContent $ B.untree vars'
           Right $ D.CoxForm cp tag vs b2
    cons _ (B.TreeB D.BracketForm _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- compound literal
    cons cp tree@(B.TreeB _ _ _) = lit cp tree

    -- literal or variable
    cons cp tree@(B.TreeL tok) = case tok of
        D.TTermN _ n               -> Right $ D.CoxTerm  cp [n] []
        D.TTermPath _ ns           -> Right $ D.CoxTerm  cp ns []
        D.TName _ op               -> Right $ D.CoxBlank cp op
        D.TTextRaw _ n | isName n  -> Right $ D.CoxBlank cp $ D.BlankNormal n
        D.TText _ _ _              -> lit cp tree
        D.TTermQ _ _               -> lit cp tree
        _                          -> B.bug "core/leaf"

    fill cp f xs =
        do f'  <- expr f
           xs' <- expr `mapM` xs
           Right $ D.CoxFill cp f' xs'

    lit cp tree  = fmap (D.CoxLit cp) $ D.contentCons (calc' tree) tree
    calc' tree tree' | tree' == tree  = Msg.unkCox "Neither literal nor calculable"
                     | otherwise      = calc tree'

    untag :: D.TTreeTo (D.CoxTag, D.TTree)
    untag (B.TreeB l p (D.TextLeafQ _ tag : vars))
                = (Just tag, B.TreeB l p $ vars)
    untag vars  = (Nothing, vars)

isName :: B.Pred String
isName (c:_)  = isNameFirst c
isName _      = False

isNameFirst :: B.Pred Char
isNameFirst c = case B.generalCategoryGroup c of
                  B.UnicodeLetter     ->  True
                  B.UnicodeMark       ->  True
                  B.UnicodeSymbol     ->  True
                  _                   ->  False

-- convert from infix operator to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap D.TTree
prefix htab tree =
    Msg.abCoxPrefix tree $
     case B.infixToPrefix conv ht (B.TreeB D.BracketGroup Nothing) mapper tree of
       Right tree3 -> Right $ undoubleGroup tree3
       Left  xs    -> Msg.ambInfixes $ map detail xs
    where
      conv = (c D.copPrefix, c D.copInfix, c D.copPostfix)
      c :: (String -> D.BlankName) -> B.Map D.Token
      c f (D.TTextRaw cp s) = D.TName cp $ f s
      c _ x = x

      ht :: D.Token -> B.InfixHeight
      ht = B.infixHeight wordText htab

      wordText :: D.Token -> Maybe String
      wordText (D.TTextRaw _ w) = Just w
      wordText _               = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = D.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

mapper :: B.InfixMapper D.BracketType D.Token
mapper f = loop where
    loop (B.TreeB D.BracketGroup p xs) =
        do xs' <- f xs
           Right $ B.TreeB D.BracketGroup p xs'
    loop (B.TreeB D.BracketForm p1 [vs, B.TreeB D.BracketGroup p2 body]) =
        do body' <- f body
           Right $ B.TreeB D.BracketForm p1 [vs, undoubleGroup $ B.TreeB D.BracketGroup p2 body']
    loop tree = Right tree

undoubleGroup :: B.Map (B.CodeTree D.BracketType a)
undoubleGroup = B.undouble (== D.BracketGroup)

-- expand tree-level syntax
convTree :: D.CopFind D.CopTree -> B.AbMap D.TTree
convTree find = expand where
    expand tree@(B.TreeB D.BracketGroup p subtrees) =
        case subtrees of
          op@(D.TextLeafRaw _ name) : args
              -> Msg.abCoxSyntax tree $
                 case find $ D.BlankNormal name of
                   Just f -> expand =<< f args
                   _      -> do args2 <- mapM expand args
                                Right $ B.TreeB D.BracketGroup p (op : args2)
          _ -> do sub2 <- mapM expand subtrees
                  Right $ B.TreeB D.BracketGroup p sub2

    expand (B.TreeB D.BracketForm p trees) =
        case D.divideTreesByBar trees of
          [vars, b1] -> do b2 <- expand $ D.ttreeGroup b1
                           Right $ B.TreeB D.BracketForm p [D.ttreeGroup vars, b2]
          _ -> Msg.unkCox "abstruction"

    expand tree = Right tree

-- | Insert fresh form into indexed expression.
coxForm :: [B.CodePt] -> D.CoxTag -> [String] -> B.Map (D.Cox c)
coxForm cp0 tag vs = debruijn . outside [] . coxUnfold . D.CoxForm cp0 tag vs where
    n = length vs
    outside vars cox = case cox of
       D.CoxLocal cp v i    
           | v `elem` vars  ->  D.CoxLocal cp v i        -- inside blank
           | otherwise      ->  D.CoxLocal cp v (i + n)  -- outside blank
       D.CoxFill _ _ _      ->  D.coxCall cox (outside vars)
       D.CoxForm1 _ _ v _   ->  D.coxCall cox (outside $ v : vars)
       _                    ->  cox


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term-content calcutation.

module Koshucode.Baala.Core.Content.Build
( CoxSyntax, coxBuild, coxForm,
) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Cox       as C
import qualified Koshucode.Baala.Core.Content.Literal   as C
import qualified Koshucode.Baala.Core.Message           as Message

type CoxSyntax c = ( [C.Cop c], [B.Named B.InfixHeight] )

-- | Construct content expression from token tree
coxBuild :: forall c. (C.CContent c)
  => C.CalcContent c -> CoxSyntax c -> B.TokenTreeToAb (C.Cox c)
coxBuild calc (syn, htab) =
    convCox syn             -- convert cox to cox
      B.<=< Right
      . debruijn            -- attach De Bruijn indicies
      . coxUnfold           -- expand multiple-blank form
      B.<=< construct calc  -- construct content expression from token tree
      B.<=< prefix htab     -- convert infix operator to prefix
      B.<=< convTree syn    -- convert token tree to token tree

debruijn :: B.Map (C.Cox c)
debruijn = index [] where
    index :: [String] -> B.Map (C.Cox c)
    index vars cox = case cox of
       C.CoxBlank cp n     ->  let v = B.name n
                               in maybe cox (C.CoxLocal cp v) $ indexFrom 1 v vars
       C.CoxFill _ _ _     ->  C.coxCall cox (index vars)
       C.CoxForm1 _ _ v _  ->  C.coxCall cox (index $ v : vars)
       _                   ->  cox

indexFrom :: (Eq c) => Int -> c -> [c] -> Maybe Int
indexFrom origin key = loop origin where
    loop _ [] = Nothing
    loop i (x:xs) | x == key  = Just i
                  | otherwise = loop (i + 1) xs

coxUnfold :: B.Map (C.Cox c)
coxUnfold = unfold . C.coxMap coxUnfold where
    unfold :: B.Map (C.Cox c)
    unfold cox = case cox of
        C.CoxForm _ _ [] b -> unfold b
        C.CoxForm cp tag (v : vs) b
            -> let sub = unfold $ C.CoxForm cp tag vs b
               in C.CoxForm1 cp tag v sub
        _   -> cox

convCox :: forall c. [C.Cop c] -> B.AbMap (C.Cox c)
convCox syn = expand where
    assn :: [(B.BlankName, C.Cop c)]
    assn = map name syn

    name cop = (C.copName cop, cop)

    expand :: B.AbMap (C.Cox c)
    expand cox =
        case cox of
            C.CoxForm1  cp tag n  x -> Right . C.CoxForm1  cp tag n  =<< expand x
            C.CoxForm   cp tag ns x -> Right . C.CoxForm   cp tag ns =<< expand x
            C.CoxFill cp f@(C.CoxBlank _ n) xs ->
                case lookup n assn of
                  Just (C.CopCox _ g) -> expand =<< g xs
                  _                   -> expandApply cp f xs
            C.CoxFill cp f xs         -> expandApply cp f xs
            _                         -> Right cox

    expandApply cp f xs =
        do f'  <- expand f
           xs' <- mapM expand xs
           Right $ C.CoxFill cp f' xs'
    
-- construct content expression from token tree
construct :: forall c. (C.CContent c) => C.CalcContent c -> B.TokenTreeToAb (C.Cox c)
construct calc = expr where
    expr tree = Message.abCoxBuild tree $
         let cp = concatMap B.codePts $ B.front $ B.untree tree
         in cons cp tree

    -- fill args in the blanks (application)
    cons :: [B.CodePt] -> B.TokenTreeToAb (C.Cox c)
    cons cp (B.TreeB B.BracketGroup _ (fun : args)) =
        do fun'  <- expr fun
           args' <- expr `mapM` args
           Right $ C.CoxFill cp fun' args'

    -- form with blanks (function)
    cons cp (B.TreeB B.BracketForm _ [vars, b1]) =
        do b2 <- expr b1
           let (tag, vars') = untag vars
           let vs = map B.tokenContent $ B.untree vars'
           Right $ C.CoxForm cp tag vs b2
    cons _ (B.TreeB B.BracketForm _ trees) =
        B.bug $ "core/abstruction: " ++ show (length trees)

    -- compound literal
    cons cp tree@(B.TreeB _ _ _) = fmap (C.CoxLit cp) $ C.litContent calc tree

    -- literal or variable
    cons cp tree@(B.TreeL tok) = case tok of
        B.TTerm _ 0 ns ->  Right $ C.CoxTerm  cp ns []
        B.TName _ op   ->  Right $ C.CoxBlank cp op
        B.TText _ 0 n | isName n ->  Right $ C.CoxBlank cp $ B.BlankNormal n
        B.TText _ _ _  ->  Right . C.CoxLit cp =<< C.litContent calc tree
        _              ->  B.bug "core/leaf"

    untag :: B.TokenTreeTo (Maybe String, B.TokenTree)
    untag (B.TreeB l p (B.TreeL (B.TText _ 1 tag) : vars))
               = (Just tag, B.TreeB l p $ vars)
    untag vars = (Nothing, vars)

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
prefix :: [B.Named B.InfixHeight] -> B.AbMap B.TokenTree
prefix htab tree =
    Message.abCoxPrefix tree $
     case B.infixToPrefix conv ht (B.TreeB B.BracketGroup Nothing) mapper tree of
       Right tree3 -> Right $ undoubleGroup tree3
       Left  xs    -> Message.ambInfixes $ map detail xs
    where
      conv = (c C.copPrefix, c C.copInfix, c C.copPostfix)
      c :: (String -> B.BlankName) -> B.Map B.Token
      c f (B.TText cp 0 s) = B.TName cp $ f s
      c _ x = x

      ht :: B.Token -> B.InfixHeight
      ht = B.infixHeight wordText htab

      wordText :: B.Token -> Maybe String
      wordText (B.TText _ 0 w) = Just w
      wordText _               = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

mapper :: B.InfixMapper B.BracketType B.Token
mapper f = loop where
    loop (B.TreeB B.BracketGroup p xs) =
        do xs' <- f xs
           Right $ B.TreeB B.BracketGroup p xs'
    loop (B.TreeB B.BracketForm p1 [vs, B.TreeB B.BracketGroup p2 body]) =
        do body' <- f body
           Right $ B.TreeB B.BracketForm p1 [vs, undoubleGroup $ B.TreeB B.BracketGroup p2 body']
    loop tree = Right tree

undoubleGroup :: B.Map (B.CodeTree B.BracketType a)
undoubleGroup = B.undouble (== B.BracketGroup)

-- expand tree-level syntax
convTree :: forall c. [C.Cop c] -> B.AbMap B.TokenTree
convTree syn = expand where
    assoc :: [B.Named (C.Cop c)]
    assoc = map B.named syn

    expand tree@(B.TreeB B.BracketGroup p subtrees) =
        case subtrees of
          op@(B.TreeL (B.TText _ 0 name)) : args
              -> Message.abCoxSyntax tree $
                 case lookup name assoc of
                   Just (C.CopTree _ f) -> expand =<< f args
                   _                    -> do args2 <- mapM expand args
                                              Right $ B.TreeB B.BracketGroup p (op : args2)
          _ -> do sub2 <- mapM expand subtrees
                  Right $ B.TreeB B.BracketGroup p sub2

    expand (B.TreeB B.BracketForm p trees) =
        case B.divideTreesByBar trees of
          [vars, b1] -> do b2 <- expand $ B.wrapTrees b1
                           Right $ B.TreeB B.BracketForm p [B.wrapTrees vars, b2]
          _ -> Message.unkCox "abstruction"

    expand tree = Right tree

-- | Insert fresh form into indexed expression.
coxForm :: [B.CodePt] -> Maybe String -> [String] -> B.Map (C.Cox c)
coxForm cp0 tag vs = debruijn . outside [] . coxUnfold . C.CoxForm cp0 tag vs where
    n = length vs
    outside vars cox = case cox of
       C.CoxLocal cp v i    
           | v `elem` vars  ->  C.CoxLocal cp v i        -- inside blank
           | otherwise      ->  C.CoxLocal cp v (i + n)  -- outside blank
       C.CoxFill _ _ _      ->  C.coxCall cox (outside vars)
       C.CoxForm1 _ _ v _   ->  C.coxCall cox (outside $ v : vars)
       _                    ->  cox


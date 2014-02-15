{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Term-content calcutation. -}

module Koshucode.Baala.Core.Content.Cox
( -- $Process

  -- * Expression
  Cox, CoxCore (..),
  -- * Operator
  Cop (..), CopFun, CopSyn,
  -- * Construction
  CoxCons, coxCons,
  -- * Run
  coxRun,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal as C


-- ----------------------  Expressions and operators

{-| Content expressions with source code. -}
type Cox c = B.Sourced (CoxCore c)

{-| Content expressions. -}
data CoxCore c
    = CoxLit c                    -- ^ Literal content
    | CoxApp (CopFun c) [Cox c]   -- ^ Operator invocation
    | CoxTerm [B.Termname] [Int]  -- ^ Term reference, its name and position

{-| Term-content operator. -}
data Cop c
    = CopFun String (CopFun c)  -- ^ Function
    | CopSyn String (CopSyn)    -- ^ Syntax

type CopFun c = [c] -> B.Ab c
type CopSyn   = CopFun B.TokenTree

instance Show (Cop c) where
    show (CopFun n _) = "(CopFun " ++ show n ++ " _)"
    show (CopSyn n _) = "(CopSyn " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopFun n _) = n
    name (CopSyn n _) = n



-- ----------------------  Construction

type CoxCons c = B.Relhead -> B.Ab (Cox c)

{-| Construct content expressions from token tree. -}
coxCons :: forall c. (C.CContent c)
  => ([Cop c], [B.Named B.InfixHeight]) -> B.TokenTree -> B.Ab (CoxCons c)
coxCons (cops, htab) = body where
    body :: B.TokenTree -> B.Ab (CoxCons c)
    body tree =
        Right . position     -- replace term name to term position
            =<< expression   -- construct content expression from token tree
            =<< prefix htab  -- convert infix operator to prefix
            =<< syntax tree  -- expand syntax operator

    assoc :: [B.Named (Cop c)]
    assoc = map B.named cops

    -- expand syntax operator
    syntax :: B.AbMap B.TokenTree
    syntax tree@(B.TreeB 1 p (op@(B.TreeL (B.TWord _ 0 name)) : args)) =
        B.abortableTree "syntax" tree $
         case lookup name assoc of
           Just (CopSyn _ f) -> syntax =<< f args
           Just _            -> do args2 <- mapM syntax args
                                   Right $ B.TreeB 1 p (op : args2)
           _                 -> Right tree
    syntax tree = Right tree

    -- construct content expression from token tree
    expression :: B.TokenTree -> B.Ab (Cox c)
    expression tree =
        B.abortableTree "cox" tree $
         let src = (B.front $ B.untree tree)
         in Right . B.Sourced src =<< core tree

    core :: B.TokenTree -> B.Ab (CoxCore c)
    core tree@(B.TreeL tok) =
        case tok of
          B.TWord _ _ _  ->  fmap CoxLit $ C.litContent tree
          B.TTerm _ ns   ->  Right $ CoxTerm ns []
          _              ->  B.bug

    -- parend unquoted word and its arguments
    core (B.TreeB 1 _ (B.TreeL (B.TWord _ 0 name) : args)) =
        case lookup name assoc of
          Just (CopFun _ f) -> CoxApp f `fmap` (expression `mapM` args)
          Just (CopSyn _ _) -> B.bug
          Nothing -> Left $ B.AbortAnalysis [] $ B.AAUnkCop name

    -- literal composite
    core tree@(B.TreeB n _ _) | n > 1 = fmap CoxLit $ C.litContent tree  
    core _ = Left $ B.AbortSyntax [] $ B.ASUnkCox ""

-- convert from infix to prefix
prefix :: [B.Named B.InfixHeight] -> B.AbMap (B.TokenTree)
prefix htab tree =
    B.abortableTree "prefix" tree $
     case B.infixToPrefix ht tree of
       Right tree3 -> Right $ B.undouble (== 1) tree3
       Left  xs    -> Left  $ B.AbortSyntax [] $ B.ASAmbInfixes $ map detail xs
    where
      ht = B.infixHeight wordText htab

      wordText :: B.Token -> Maybe String
      wordText (B.TWord _ 0 w) = Just w
      wordText _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n

      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

-- put term positions for actural heading
position :: Cox c -> CoxCons c
position scox h = spos scox where
    spos = B.sourcedAbMap pos
    pos (CoxLit c)     = Right . CoxLit $ c
    pos (CoxApp f cs)  = Right . CoxApp f =<< mapM spos cs
    pos (CoxTerm ns _) =
        let index = B.headIndex1 h ns
        in if all (>= 0) index
           then Right $ CoxTerm ns index
           else Left  $ B.AbortAnalysis [] (B.AANoTerms ns)



-- ----------------------  Run

{-| Calculate content expression. -}
coxRun
  :: (C.CRel c, C.CList c)
  => B.Relhead     -- ^ Heading of relation
  -> [c]           -- ^ Tuple in body of relation
  -> CoxCons c     -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun h arg pcox = run =<< pcox h where
    run (B.Sourced src cox) = B.abortable "calc" src $
        case cox of
          CoxLit c      -> Right c
          CoxTerm _ [p] -> Right $ arg !! p
          CoxTerm _ ps  -> term ps arg
          CoxApp f cs   -> f =<< mapM run cs

    term []       _ = Left $ B.abortNotFound ""
    term (-1 : _) _ = Left $ B.abortNotFound ""
    term (p : ps) arg2 =
        let c = arg2 !! p
        in if C.isRel c
           then rel ps $ C.getRel c
           else Right c

    rel ps (B.Rel _ args) = Right . C.putList =<< mapM (term ps) args


-- ----------------------
{- $Process

   Phase 1. From string to prefixed token tree.

     [1. @String -> \[Token\]@]
        Parse input string into list of token.
        See 'B.tokens'.

     [2. @\[Token\] -> \[Tree Token\]@]
        Analyze token list structure.
        See 'B.tokenTrees'

     [3. @\[Tree Token\] -> Tree Token@]
        Enclose list of token tree in 'B.TreeB'.
        See 'B.treeWrap'.

     [4. @Tree Token -> Tree Token@]
        Translate binary operators from infix to prefix.
        See 'B.infixToPrefix'.

   Phase 2. From prefixed token tree to literal content.

     [5. @Tree Token -> Cox c@]
        Convert from token tree to content expression.
        See 'coxCons'.

     [6. @Cox c -> (Relhead -> Cox c)@]
        Attach term positions using actural heading of relation.
        See 'coxCons'.

     [7. @Cox c -> \[c\] -> c@]
        Calculate content expression for each tuple of relation.
        See 'coxRun'.

-}


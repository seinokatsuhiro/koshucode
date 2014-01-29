{-# OPTIONS_GHC -Wall #-}

{-| Term-content calcutation. -}

module Koshucode.Baala.Core.Content.Cox
(
  -- * Expression
  Cox,
  CoxCore (..),

  -- * Operator
  Cop (..),
  CopLit, CopFun, CopMacro,

  -- * Construction
  CoxCons,
  coxCons,

  -- * Run
  coxRun,
  -- $Process
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal as C


-- ----------------------  Cox

{-| Content expressions with source code. -}
type Cox c = B.Sourced (CoxCore c)

{-| Content expressions. -}
data CoxCore c
    = CoxLit c                 -- ^ Literal content
    | CoxApp (Cop c) [Cox c]   -- ^ Operator invocation
    | CoxTerm [B.Termname] [Int]   -- ^ Term reference
    deriving (Show)


-- ----------------------  Cop

{-| Term-content operator. -}
data Cop c
    = CopLit   String (CopLit   c)
    | CopMacro String (CopMacro c)
    | CopFun   String (CopFun   c)

instance Show (Cop c) where
    show (CopLit n _)   = "(CopLit "   ++ show n ++ " _)"
    show (CopFun n _)   = "(CopFun "   ++ show n ++ " _)"
    show (CopMacro n _) = "(CopMacro " ++ show n ++ " _)"

instance B.Name (Cop c) where
    name (CopLit   n _)  = n
    name (CopFun n _)    = n
    name (CopMacro  n _) = n

type CopLit   c = [B.TokenTree] -> B.Ab c
type CopFun   c = [c]           -> B.Ab c
type CopMacro c = [Cox c]       -> B.Ab (Cox c)



-- ----------------------  Construction

type CoxCons c = B.Relhead -> B.Ab (Cox c)

{-| Construct content expression. -}
coxCons :: (C.CContent c) => ([Cop c], [B.Named B.InfixHeight]) -> B.TokenTree -> B.Ab (CoxCons c)
coxCons (cops, htab) tree =
    case B.infixToPrefix ht tree of
      Right tree2 -> fmap positioning . construct cops $ B.undouble (== 1) tree2
      Left  xs    -> Left $ B.AbortSyntax [] $ B.ASAmbInfixes $ map detail xs
                     
    where
      ht = B.infixHeight text htab

      text (B.TWord _ 0 w) = Just w
      text _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n
      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n

construct :: (C.CContent c) => [Cop c] -> B.TokenTree -> B.Ab (Cox c)
construct cops = cons where
    cons tree = Right . B.Sourced src =<< cox where
       src = B.front $ B.untree tree
       cox = case tree of
            B.TreeL tok -> 
                case tok of
                  B.TWord _ _ _  ->  fmap CoxLit $ C.litContent tree
                  B.TTerm _ ns   ->  Right $ CoxTerm ns []
                  _              ->  B.bug

            -- parend unquoted word and its arguments
            B.TreeB 1 _ (B.TreeL (B.TWord _ 0 name) : args) ->
                case lookup name $ map B.named cops of
                  Just cop   ->  call cop args
                  Nothing    ->  Left $ B.AbortAnalysis [] $ B.AAUnkCop name

            B.TreeB n _ _
                | n > 1  ->  fmap CoxLit $ C.litContent tree  -- literal composite

            _ -> Left $ B.AbortSyntax [] $ B.ASUnkCox ""

    call (CopLit _ f) = fmap CoxLit . f
    call cop          = fmap (CoxApp cop) . mapM cons

-- Put term positions for actural heading.
positioning :: Cox c -> CoxCons c
positioning scox h = spos scox where
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
  -> (CoxCons c)   -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun h arg pcox = run =<< pcox h where
    run (B.Sourced src cox) = B.abortable "calc" src $
        case cox of
          CoxLit c      -> Right c
          CoxTerm _ [p] -> Right $ arg !! p
          CoxTerm _ ps  -> term ps arg
          CoxApp (CopFun   _ f) cs -> f   =<< mapM run cs
          CoxApp (CopMacro _ f) cs -> run =<< f cs
          CoxApp (CopLit   _ _) _  -> Left $ B.abortNotFound ""

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
        See 'B.treeG'.

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


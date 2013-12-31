{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Cox.Run
( -- $Process
  coxRun,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal       as C
import qualified Koshucode.Baala.Core.Content.Cox.Operator  as C
import qualified Koshucode.Baala.Core.Content.Cox.Construct as C


{-| Calculate content expression. -}
coxRun
  :: (C.CRel c, C.CList c)
  => B.Relhead     -- ^ Heading of relation
  -> [c]           -- ^ Tuple in body of relation
  -> (C.CoxPos c)  -- ^ Content expression
  -> B.Ab c        -- ^ Calculated literal content
coxRun h arg coxPos = run =<< coxPos h where
    run (B.Sourced src cox) = B.ab src $
        case cox of
          C.CoxLit c      -> Right c
          C.CoxTerm _ [p] -> Right $ arg !! p
          C.CoxTerm _ ps  -> term ps arg
          C.CoxApp cop cs ->
              case cop of
                C.CopLazy  _ f -> f cs
                C.CopEager _ f -> f =<< mapM run cs
                C.CopLit   _ _ -> Left $ B.abortNotFound ""

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
        See 'B.binaryTree'.

   Phase 2. From prefixed token tree to literal content.

     [5. @Tree Token -> Cox c@]
        Convert from token tree to content expression.
        See 'coxCons'.

     [6. @Cox c -> (Relhead -> Cox c)@]
        Attach term positions using actural heading of relation.
        See 'coxPos'.

     [7. @Cox c -> \[c\] -> c@]
        Calculate content expression for each tuple of relation.
        See 'coxRun'.

-}


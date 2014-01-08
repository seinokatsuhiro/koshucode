{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Cox.Construct
( coxPos,
  CoxPos,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal      as C
import qualified Koshucode.Baala.Core.Content.Cox.Operator as C


{-| Construct content expression. -}
coxPos :: (C.CContent c) => C.FindCop c -> B.TokenTree -> B.Ab (CoxPos c)
coxPos findCop tree = positioning `fmap` construct findCop tree

type CoxPos c = B.Relhead -> B.Ab (C.Cox c)

{-| Put term positions for actural heading. -}
positioning :: C.Cox c -> CoxPos c
positioning scox h = spos scox where
    spos = B.sourcedAbMap pos
    pos (C.CoxLit c)     = Right . C.CoxLit $ c
    pos (C.CoxApp f cs)  = Right . C.CoxApp f =<< mapM spos cs
    pos (C.CoxTerm ns _) =
        let index = B.headIndex1 h ns
        in if all (>= 0) index
           then Right $ C.CoxTerm ns index
           else Left  $ B.AbortAnalysis [] (B.AANoTerms ns)

{-| Construct content expression. -}
construct :: (C.CContent c) => C.FindCop c -> B.TokenTree -> B.Ab (C.Cox c)
construct findCop = cons where
    cons tree = Right . B.Sourced src =<< cox where
       src = B.front $ B.untree tree
       cox = case tree of
            B.TreeL tok -> 
                case tok of
                  B.TWord _ _ _  ->  fmap C.CoxLit $ C.litContent tree
                  B.TTerm _ ns   ->  Right $ C.CoxTerm ns []
                  _              ->  B.bug

            -- parend unquoted word and its arguments
            B.TreeB 1 _ (B.TreeL (B.TWord _ 0 name) : args) ->
                case findCop name of
                  Just cop   ->  call cop args
                  Nothing    ->  Left $ B.AbortAnalysis src $ B.AAUnkCop name

            B.TreeB n _ _
                | n > 1  ->  fmap C.CoxLit $ C.litContent tree  -- literal composite

            _ -> Left $ B.AbortSyntax src $ B.ASUnkCox ""

    call (C.CopLit _ f) = fmap C.CoxLit . f
    call cop            = fmap (C.CoxApp cop) . mapM cons


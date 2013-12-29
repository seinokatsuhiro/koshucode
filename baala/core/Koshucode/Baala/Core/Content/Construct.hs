{-# OPTIONS_GHC -Wall #-}

{-| Term calcutation. -}

module Koshucode.Baala.Core.Content.Construct
( coxPos,
  --coxCons,
  CoxPos,
  --coxPos,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Class     as C
import qualified Koshucode.Baala.Core.Content.Extension as C
import qualified Koshucode.Baala.Core.Content.Operator  as C


{-| Construct content expression. -}
coxPos :: (C.CContent c) => C.FindCop c -> B.TokenTree -> B.Ab (CoxPos c)
coxPos cops tree = coxConsPos `fmap` coxCons cops tree

{-| Construct content expression. -}
coxCons
    :: (C.CContent c)
    => C.FindCop c      -- ^ Collection of operators
    -> B.TokenTree      -- ^ Input token tree
    -> B.Ab (C.Cox c)   -- ^ Result content expression
coxCons cops = cons where
    cons tree = Right . B.Sourced src =<< cox where
       src = B.front $ B.untree tree
       cox = case tree of
            B.TreeL tok -> 
                case tok of
                  B.TWord _ _ _  ->  fmap C.CoxLit $ C.litContent tree
                  B.TTerm _ ns   ->  Right $ C.CoxTerm ns []
                  _              ->  B.bug

            -- parend unquoted word and its arguments
            B.TreeB 1 _ (B.TreeL (B.TWord _ 0 w) : args) ->
                case cops w of
                  Just cop   ->  call cop args
                  Nothing    ->  Left $ B.AbortAnalysis src $ B.AAUnkCop w

            B.TreeB n _ _
                | n > 1  ->  fmap C.CoxLit $ C.litContent tree  -- literal composite

            _ -> Left $ B.AbortSyntax src $ B.ASUnkCox ""

    call (C.CopLit _ f) = fmap C.CoxLit . f
    call cop            = fmap (C.CoxApp cop) . mapM cons

type CoxPos c = B.Relhead -> B.Ab (C.Cox c)

{-| Put term positions for actural heading. -}
coxConsPos :: C.Cox c -> CoxPos c
coxConsPos scox h = sourceAbMap pos scox where
    pos cox =
        case cox of
          C.CoxLit c     -> Right . C.CoxLit $ c
          C.CoxApp f cs  -> Right . C.CoxApp f =<< mapM (sourceAbMap pos) cs
          C.CoxTerm ns _ ->
              let index = B.headIndex1 h ns
              in if all (>= 0) index
                 then Right $ C.CoxTerm ns index
                 else Left  $ B.AbortAnalysis [] (B.AANoTerms ns)

sourceAbMap :: (a -> B.Ab b) -> B.Sourced a -> B.Ab (B.Sourced b)
sourceAbMap f (B.Sourced src x) =
    case f x of
      Right y -> Right $ B.Sourced src y
      Left a  -> Left  $ B.abortPushToken src a


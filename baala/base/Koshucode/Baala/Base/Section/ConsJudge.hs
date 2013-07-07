{-# OPTIONS_GHC -Wall #-}

{-| Construct judge from tokens -}

module Koshucode.Baala.Base.Section.ConsJudge
(
-- * Library
  consJudge
, consTerms
, consContent
, consContents

-- * Examples
-- $Examples
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Section.Clausify


-- ----------------------  Judge

{-| Construct judge from token trees. -}
consJudge :: (Value v) => ClauseSource
      -> Bool -> Relsign -> [TokenTree]
      -> AbortOr (Judge v)
consJudge src q s xs =
  do xs' <- consTerms src xs
     Right $ Judge q s xs'

{-| Collect term name and content. -}
consTerms :: (Value v) => ClauseSource
      -> [TokenTree] -> AbortOr [Named v]
consTerms src = loop where
    loop [] = Right []
    loop (n : x : xs) =
        do n'  <- consName1 src n
           x'  <- consContent src x
           xs' <- loop xs
           Right $ (n', x') : xs'
    loop (x : _) = Left $ AbortMalformedTerms (clauseLines src) (show x) -- ???

consName1 :: ClauseSource -> TokenTree -> AbortOr String
consName1 _ (TreeL (TTermN _ [n])) = Right n
consName1 src x = Left $ AbortMalformedTerms (clauseLines src) (show x)

{-| Construct term content. -}
consContent :: (Value v) => ClauseSource -> TokenTree -> AbortOr v
consContent src = cons where
    cons (TreeL (TWord _ 0 k))
        | k == "()"            =  Right $ nil
        | k == "#true"         =  Right $ boolValue True
        | k == "#false"        =  Right $ boolValue False
    cons (TreeL (TWord _ _ w)) =  Right $ stringValue w
    cons (TreeB level cs)
         | level == 2          =  do cs' <- consContents src cs
                                     Right $ listValue cs'
         | level == 3          =  do cs' <- consTerms src cs
                                     Right $ termsetValue cs'
         | level == 4          =  do cs' <- consRel src cs
                                     Right $ relValue cs'
    -- unknown content
    cons x = Left $ AbortMalformedTerms (clauseLines src) (show x)

{-| Construct list of term contents. -}
consContents :: (Value v) => ClauseSource -> [TokenTree] -> AbortOr [v]
consContents src = loop where
    loop [] = Right []
    loop (x : xs) =
        do x'  <- consContent src x
           xs' <- loop xs
           Right $ x' : xs'

consRel :: (Value v) => ClauseSource -> [TokenTree] -> AbortOr (Rel v)
consRel src cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM (consName1 src) h1
       b2 <- mapM (consContents src) b1
       Right $ Rel (headFrom h2) b2



-- ----------------------
{- $Examples

   >>> :m +Koshucode.Baala.Vanilla.Value.Val
   >>> let trees = tokenTrees . tokens
   >>> let cons  = consContents undefined . trees :: String -> AbortOr [Val]

   Words.

   >>> cons "a"
   Right [Stringv "a"]

   >>> cons "a 1"
   Right [Stringv "a",Stringv "1"]

   Digits.

   >>> cons "1"
   Right [Stringv "1"]

   Nil as no value.

   >>> cons "()"
   Right [Nov]

   List.

   >>> cons "[ a 1 ]"
   Right [Listv [Stringv "a", Stringv "1"]]

   Termset.

   >>> cons "{ /a 0 /b [ a 1 ] }"
   Right [Termsetv [
            ("/a", Stringv "0"),
            ("/b", Listv [Stringv "a", Stringv "1"])]]

   Relation.

   >>> cons "{| /a /b | 10 20 | 30 40 |}"
   Right [Relv (Rel
     (Relhead [Term "/a", Term "/b"])
     [[Stringv "10", Stringv "20"],
      [Stringv "30", Stringv "40"]])]

-}


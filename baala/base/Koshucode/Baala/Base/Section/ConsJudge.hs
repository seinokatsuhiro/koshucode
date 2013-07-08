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



-- ----------------------  Judge

{-| Construct judge from token trees. -}
consJudge :: (Value v) => [SourceLine]
      -> Bool -> Relsign -> [TokenTree]
      -> AbortOr (Judge v)
consJudge src q s xs =
  do xs' <- consTerms src xs
     Right $ Judge q s xs'

{-| Collect term name and content. -}
consTerms :: (Value v) => [SourceLine]
      -> [TokenTree] -> AbortOr [Named v]
consTerms src xs = mapM internal =<< divideByName src xs where
    internal (n, c) =
        do c' <- consContent src c
           Right (n, c')

divideByName :: [SourceLine] -> [TokenTree] -> AbortOr [Named TokenTree]
divideByName src = nam where
    nam [] = Right []
    nam (x : xs) =
        let (cs, xs2) = content xs
        in do n    <- consName1 src x
              xs2' <- nam xs2
              Right $ (n, singleToken cs) : xs2'

    content :: [TokenTree] -> ([TokenTree], [TokenTree])
    content xs@(TreeL (TTermN _ _) : _) = ([], xs)
    content [] = ([], [])
    content (x : xs) = cons1 x $ content xs

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
consName1 :: [SourceLine] -> TokenTree -> AbortOr String
consName1 _   (TreeL (TTermN _ [n])) = Right n
consName1 src (TreeL (TTermN _ ns))  = Left $ AbortRequireFlatname src (concat ns)
consName1 src x = Left $ AbortMissingTermName src (show x)

{-| Transform 'TokenTree' into
    internal form of term content.
    There are five types of contents.

    [String]    Sequence of characters.
                External forms: @abc@, @\'abc\'@, @\"abc def\"@.

    [Boolean]   Boolean used for something is hold, or unhold.
                External forms: @\#true@, @\#fasle@.

    [Nil]       Nil means that there are no values.
                i.e., universal negation on the term holds.
                External form is the non-quoted parens: @()@.

    [List]      List is a ordered list of contents.
                External form is a sequence of contents
                with brackets: @[ abc def ]@.

    [Termset]   Termset is a set of terms,
                i.e., a list of named contents.
                External form is a sequence of terms
                with braces: @{ \/a 10 \/b 20 }@.

    [Relation]  Relation is a set of same-type tuples,
                External form is a sequence of tuples enclosed in braces,
                and tuples are delimited by vertical bar:
                @{| \/a \/b | 10 20 | 30 40 |}@.
 -}
consContent :: (Value v) => [SourceLine] -> TokenTree -> AbortOr v
consContent src = cons where
    -- leaf
    cons (TreeL (TWord _ q w))
        | q >  0    =  Right . stringValue $ w
        | q == 0    =  case w of
          "()"      -> Right nil
          "#true"   -> Right . boolValue   $ True
          "#false"  -> Right . boolValue   $ False
          _         -> Right . stringValue $ w

    -- branch
    cons (TreeB t xs) = case t of
          1         ->  Right nil
          2         ->  Right . listValue    =<< consContents src xs
          3         ->  Right . termsetValue =<< consTerms src xs
          4         ->  Right . relValue     =<< consRel src xs
          _         ->  bug

    -- unknown content
    cons x          = Left $ AbortUnknownContent src (show x)

{-| Construct list of term contents. -}
consContents :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr [v]
consContents src = loop where
    loop [] = Right []
    loop (x : xs) =
        do x'  <- consContent src x
           xs' <- loop xs
           Right $ x' : xs'

consRel :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr (Rel v)
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


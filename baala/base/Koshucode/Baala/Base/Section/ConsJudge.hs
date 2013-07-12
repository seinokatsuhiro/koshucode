{-# OPTIONS_GHC -Wall #-}

{-| Construct judge from tokens -}

module Koshucode.Baala.Base.Section.ConsJudge
(
-- * Library
-- $Types
  consContent,
  consJudge

-- * Examples
-- $Examples
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax



-- ----------------------  Library

{-| Transform 'TokenTree' into
    internal form of term content.
    There are five types of contents.
 -}
consContent :: (Value v) => [SourceLine] -> TokenTree -> AbortOr v
consContent src = cons where
    -- leaf
    cons (TreeL (TWord _ q w))
        | q >  0    =   Right . stringValue $ w
        | q == 0    =   case w of
          '#' : w2  ->  hash src w2
          "()"      ->  Right nil
          _         ->  Right . stringValue $ w

    -- branch
    cons (TreeB t xs) = case t of
          1         ->  paren src xs
          2         ->  Right . listValue    =<< consList    src xs
          3         ->  Right . listValue    =<< consList    src xs
          4         ->  Right . termsetValue =<< consTermset src xs
          5         ->  Right . relValue     =<< consRel     src xs
          _         ->  bug

    -- unknown content
    cons x          =   Left $ AbortUnknownContent src (show x)

hash :: (Value v) => [SourceLine] -> String -> AbortOr v
hash src w =
    case w of
      "true"  -> Right . boolValue $ True
      "false" -> Right . boolValue $ False
      _       -> case hashWord w of
                   Just w2 -> Right . stringValue $ w2
                   Nothing -> Left $ AbortUnknownSymbol src ('#' : w)

paren :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr v
paren src (TreeL (TWord _ 0 tag) : xs) =
    case tag of
      "text"  -> Right . joinContent =<< consList src xs
      "int"   -> consInt src xs
      _       -> Left $ AbortUnknownSymbol src (show xs)
paren src x = Left $ AbortUnknownSymbol src (show x)

consInt :: (IntValue v) => [SourceLine] -> [TokenTree] -> AbortOr v
consInt src [TreeL (TWord _ 0 digits)] = 
    Right . intValue =<< readInt src digits
consInt src xs = Left $ AbortNotNumber src (show xs)

readInt :: [SourceLine] -> String -> AbortOr Int
readInt src s =
    case reads s of
      [(n, "")] -> Right n
      _         -> Left $ AbortNotNumber src s

{-| Construct list of term contents. -}
consList :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr [v]
consList src = mapM (consContent src)

{-| Collect term name and content. -}
consTermset :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr [Named v]
consTermset src xs = mapM internal =<< divideByName src xs where
    internal (n, c) = do c' <- consContent src c
                         Right (n, c')

divideByName :: [SourceLine] -> [TokenTree] -> AbortOr [Named TokenTree]
divideByName src = nam where
    nam [] = Right []
    nam (x : xs) =
        let (cs, xs2) = content xs
        in do n    <- consFlatname src x
              xs2' <- nam xs2
              Right $ (n, singleToken cs) : xs2'

    content :: [TokenTree] -> ([TokenTree], [TokenTree])
    content xs@(TreeL (TTermN _ _) : _) = ([], xs)
    content [] = ([], [])
    content (x : xs) = cons1 x $ content xs

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
consFlatname :: [SourceLine] -> TokenTree -> AbortOr String
consFlatname _   (TreeL (TTermN _ [n])) = Right n
consFlatname src (TreeL (TTermN _ ns))  = Left $ AbortRequireFlatname src (concat ns)
consFlatname src x = Left $ AbortMissingTermName src (show x)

consRel :: (Value v) => [SourceLine] -> [TokenTree] -> AbortOr (Rel v)
consRel src cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM (consFlatname src) h1
       b2 <- mapM (consList src) b1
       let b3 = unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ AbortOddRelation src
          else Right $ Rel (headFrom h2) b3

{-| Construct judge from token trees. -}
consJudge :: (Value v) => [SourceLine]
      -> Bool -> Relsign -> [TokenTree]
      -> AbortOr (Judge v)
consJudge src q s xs =
  do xs' <- consTermset src xs
     Right $ Judge q s xs'



-- ----------------------
{- $Types

   'consContent' recognizes the following types.

   [String]    Sequence of characters.
               External forms: @abc@, @\'abc\'@, @\"abc def\"@.

   [Boolean]   Boolean used for something is hold or unhold.
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



-- ----------------------
{- $Examples

   >>> :m +Koshucode.Baala.Vanilla.Value.Val
   >>> let trees = tokenTrees . tokens
   >>> let cons  = consList undefined . trees :: String -> AbortOr [Val]

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


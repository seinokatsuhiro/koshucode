{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Literalize
(
  -- * Library

  -- ** Types
  LitTrees,
  LitTree,
  LitString,
  LitOperators,

  -- ** Functions
  litContentBy,
  litTermset,

  -- * Document

  -- ** Types
  -- $Types

  -- ** Simple data
  -- $SimpleData

  -- ** Compound data
  -- $CompoundData
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Core.Content.Class



-- ----------------------  Type

{-| Make @a@ from list of token trees. -}
type LitTrees  a = AbMap2 [TokenTree] a

{-| Make @a@ from a token tree. -}
type LitTree   a = AbMap2 TokenTree a

{-| Make @a@ from a string. -}
type LitString a = AbMap2 String a



-- ----------------------  Content

type LitOperators c = [Named (LitTree c -> LitTrees c)]

-- litContent :: (CContent c) => LitTree c
-- litContent = litContentBy litOperators

{-| Transform 'TokenTree' into
    internal form of term content. -}
litContentBy :: (CContent c) => LitOperators c -> LitTree c
litContentBy ops = lit where
    -- leaf
    lit (TreeL (TWord _ q w))
        | q >  0   =   Right . putString $ w  -- quoted text
        | q == 0   =   case w of
          '#' : w' ->  hash w'
          "()"     ->  Right nil
          _        ->  Right . putString $ w

    -- branch
    lit (TreeB t xs) = case t of
          1        ->  paren xs
          2        ->  Right . putList    =<< mapM       lit xs
          3        ->  Right . putSet     =<< mapM       lit xs
          4        ->  Right . putTermset =<< litTermset lit xs
          5        ->  Right . putRel     =<< litRel     lit xs
          _        ->  bug

    -- unknown content
    lit x          =   Left $ AbortUnknownContent (show x)

    -- hash word
    hash "true"    =   Right . putBool $ True
    hash "false"   =   Right . putBool $ False
    hash w         =   case hashWord w of
                         Just w2 -> Right . putString $ w2
                         Nothing -> Left $ AbortUnknownSymbol ('#' : w)

    -- sequence of token trees
    paren (TreeL (TWord _ 0 tag) : xs) =
        case lookup tag ops of
          Just f  -> f lit xs
          Nothing -> Left $ AbortUnknownSymbol (show xs)
    paren [] = Right nil
    paren x  = Left $ AbortUnknownSymbol (show x)



-- ----------------------  Complex data

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: LitTree String
litFlatname (TreeL (TTerm _ [n])) = Right n
litFlatname (TreeL (TTerm _ ns))  = Left $ AbortRequireFlatname (concat ns)
litFlatname x = Left $ AbortMissingTermName (show x)

{-| Collect term name and content. -}
litTermset :: (CContent c) => LitTree c -> LitTrees [Named c]
litTermset lit xs = mapM lit2 =<< divideByTermname xs where
    lit2 (n, c) = do c' <- lit c
                     Right (n, c')

litRel :: (CContent c) => LitTree c -> LitTrees (Rel c)
litRel lit cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM litFlatname h1
       b2 <- mapM (mapM lit) b1
       let b3 = unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ AbortOddRelation
          else Right $ Rel (headFrom h2) b3

divideByTermname :: LitTrees [Named TokenTree]
divideByTermname = nam where
    nam [] = Right []
    nam (x : xs) =
        let (cs, xs2) = content xs
        in do n    <- litFlatname x
              xs2' <- nam xs2
              Right $ (n, singleTree cs) : xs2'

    content :: [TokenTree] -> ([TokenTree], [TokenTree])
    content xs@(TreeL (TTerm _ _) : _) = ([], xs)
    content [] = ([], [])
    content (x : xs) = cons1 x $ content xs






-- ----------------------  Document
{- $Types

   'litContent' recognizes the following types.

   [Boolean]   Boolean used for something is hold or unhold.
               Textual forms: @\#true@, @\#fasle@.

   [Nil]       Nil means that there are no values.
               i.e., universal negation on the term holds.
               Textual form is the non-quoted parens: @()@.

   [Text]      Sequence of characters.
               Textual forms is @q@-prefixed chars or
               quoted chars: @q abc@, @\'abc\'@, @\"abc def\"@.

   [Decimal]   Decimal number.
               Textual forms is @n@-prefixed digits:
               @n 100@, @n 99.50@, @hex AF@.

   [Set]       Set is an unordered collection of contents.
               Duplication among contents is not significant.
               Textual form is a sequence of contents
               delimited by colon, enclosed in braces:
               @{ 'a' : 'b' : 'c' }@.

   [List]      List is an ordered list of contents.
               Textual form is a sequence of contents
               delimited by colon, enclosed in square brackets:
               @[ q abc : q def ]@.

   [Termset]   Termset is a set of terms,
               i.e., a list of named contents.
               Textual form is a sequence of terms
               with bar-angles: @\<| \/a n 10 \/b n 20 |\>@.

   [Relation]  Relation is a set of same-type tuples,
               Textual form is a sequence of tuples
               enclosed in bar-braces.
               The first tuple is a heading of relation,
               and succeeding tuples are delimited by vertical bar:
               @{| \/a \/b | n 10 : n 20 | n 30 : n 40 |}@.
-}



-- ----------------------
{- $SimpleData

   Prepere some definitions.

   >>> :m +Koshucode.Baala.Vanilla.Value.Val
   >>> let trees = tokenTrees . tokens
   >>> let lit  = litList [] . trees :: String -> AbortOr [Val]

   Boolean.

     >>> lit "#true #false"
     Right [VBool True, VBool False]

   Words.

     >>> lit "a 'b c'"
     Right [VString "a", VString "b c"]

     >>> lit "a 1"
     Right [VString "a", VString "1"]

   Integer.

     >>> lit "(int 12)"
     Right [VInt 12]

   Nil as no value.

     >>> lit "()"
     Right [VNil]

-}

-- ----------------------
{- $CompoundData

   Set.

     >>> lit "{ b a a c a }"
     Right [VSet [VString "b",VString "a",VString "c"]]

   List.

     >>> lit "[ a 10 (int 20) ]"
     Right [VList [VString "a",VString "10",VInt 20]]

   Termset.

     >>> lit "{| /a 0 /b [ a 1 ] |}"
     Right [VTermset
       [ ("/a", VString "0")
       , ("/b", VList [VString "a", VString "1"]) ]]

   Relation.

     >>> lit "[| /a /x | A1 (int 20) | A3 (int 40) | A4 (int 60) |]"
     Right [ VRel (Rel
       (Relhead [Term "/a", Term "/x"])
       [ [VString "A1", VInt 20]
       , [VString "A3", VInt 40]
       , [VString "A4", VInt 60] ])]

-}


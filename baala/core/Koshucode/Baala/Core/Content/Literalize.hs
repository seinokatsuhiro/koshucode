{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Literalize
(
  -- * Library

  -- ** Types
  LitTrees,
  LitTree,
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

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Content.Class
import Koshucode.Baala.Core.Content.Decimal


-- ----------------------  Type

{-| Make @a@ from list of token trees. -}
type LitTrees  a = AbMap2 [TokenTree] a

{-| Make @a@ from a token tree. -}
type LitTree   a = AbMap2 TokenTree a




-- ----------------------  Content

type LitOperators c = [Named (LitTree c -> LitTrees c)]

{-| Transform 'TokenTree' into
    internal form of term content. -}
litContentBy :: (CContent c) => LitOperators c -> LitTree c
litContentBy ops = lit where
    lit (TreeB t xs) = case t of
          1  ->  paren xs
          2  ->  fmap putList    $ litList    lit xs
          3  ->  fmap putSet     $ litList    lit xs
          4  ->  fmap putTermset $ litTermset lit xs
          5  ->  fmap putRel     $ litRel     lit xs
          _  ->  bug

    lit (TreeL tok) = case tok of
        TWord _ 0 cs@(c:_) | c `elem` "0123456789+-."
                     ->  do (n, _, _) <- litDecimal cs
                            Right . putInt $ n
        TWord _ 0 w  ->  case w of
            '#' : s  ->  litHash s
            "()"     ->  Right nil
            _        ->  Left $ AbortUnkWord w
        TWord _ _ w  ->  Right . putText $ w  -- quoted text
        _            ->  Left $ AbortUnknownContent (show tok)

    -- sequence of token trees
    paren (TreeL (TWord _ 0 tag) : xs) =
        case lookup tag ops of
          Just f  -> f lit xs
          Nothing -> Left $ AbortUnkCop tag
    paren [] = Right nil
    paren x  = Left $ AbortUnknownSymbol (show x)

-- hash word
litHash :: (CBool c) => LitString c
litHash "true"    =  Right . putBool $ True
litHash "false"   =  Right . putBool $ False
litHash w         =  Left $ AbortUnkWord ('#' : w)


-- ----------------------  Complex data

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: LitTree String
litFlatname (TreeL (TTerm _ [n])) = Right n
litFlatname (TreeL (TTerm _ ns))  = Left $ AbortReqFlatname (concat ns)
litFlatname x = Left $ AbortMissingTermname (show x)

litList :: (CContent c) => LitTree c -> LitTrees [c]
litList _   [] = Right []
litList lit cs = mapM lt $ divideByTokenTree ":" cs where
    lt []  = Right nil
    lt [x] = lit x
    lt xs  = lit $ TreeB 1 xs

litRel :: (CContent c) => LitTree c -> LitTrees (Rel c)
litRel lit cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM litFlatname h1
       b2 <- mapM (litList lit) b1
       let b3 = unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ AbortOddRelation
          else Right $ Rel (headFrom h2) b3

{-| Collect term name and content. -}
litTermset :: (CContent c) => LitTree c -> LitTrees [Named c]
litTermset lit xs = namedC where
    namedC   = mapM p       =<< litNamedTrees xs
    p (n, c) = Right . (n,) =<< lit c

litNamedTrees :: LitTrees [Named TokenTree]
litNamedTrees = termname where
    termname [] = Right []
    termname (x : xs) =
        let (cs, xs2) = content xs
        in do n    <- litFlatname x
              xs2' <- termname xs2
              Right $ (n, singleTree cs) : xs2'

    content :: [TokenTree] -> ([TokenTree], [TokenTree])
    content xs@(TreeL (TTerm _ _) : _) = ([], xs)
    content [] = ([], [])
    content (x : xs) = cons1 x $ content xs






-- ----------------------  Document
{- $Types

   'litContentBy' recognizes the following types.

   [Boolean]   Boolean used for something is hold or unhold.
               Textual forms: @\#true@, @\#fasle@.

   [Nil]       Nil means that there are no values.
               i.e., universal negation on the term holds.
               Textual form is the non-quoted parens: @()@.

   [Text]      Sequence of characters.
               Textual forms is chars with apostrophe or
               doubly-quoted line: @\'abc@, @\'\'abc def@.

   [Decimal]   Decimal number.
               Textual forms is sequence of digits:
               @100@, @99.50@, @hex AF@.

   [Set]       Set is an unordered collection of contents.
               Duplication among contents is not significant.
               Textual form is a sequence of contents
               delimited by colon, enclosed in braces:
               @{ \'a : \'b : \'c }@.

   [List]      List is an ordered list of contents.
               Textual form is a sequence of contents
               delimited by colon, enclosed in square brackets:
               @[ \'abc : \'def ]@.

   [Termset]   Termset is a set of terms,
               i.e., a list of named contents.
               Textual form is a sequence of terms
               with bar-angles: @\<| \/a 10 \/b 20 |\>@.

   [Relation]  Relation is a set of same-type tuples,
               Textual form is a sequence of tuples
               enclosed in bar-braces.
               The first tuple is a heading of relation,
               and succeeding tuples are delimited by vertical bar:
               @{| \/a \/b | \'A1 : 20 | \'A3 : 40 |}@.
-}



-- ----------------------
{- $SimpleData

   Prepere some definitions.

   >>> :m +Koshucode.Baala.Vanilla.Type.Val
   >>> let trees = tokenTrees . tokens
   >>> let lit  = litList [] . trees :: String -> AbortOr [Val]

   Boolean.

     >>> lit "#true #false"
     Right [VBool True, VBool False]

   Words.

     >>> lit "a 'b c'"
     Right [VText "a", VText "b c"]

     >>> lit "a 1"
     Right [VText "a", VText "1"]

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
     Right [VSet [VText "b", VText "a", VText "c"]]

   List.

     >>> lit "[ a 10 (int 20) ]"
     Right [VList [VText "a", VText "10", VInt 20]]

   Termset.

     >>> lit "{| /a 0 /b [ a 1 ] |}"
     Right [VTermset
       [ ("/a", VText "0")
       , ("/b", VList [VText "a", VText "1"]) ]]

   Relation.

     >>> lit "[| /a /x | A1 (int 20) | A3 (int 40) | A4 (int 60) |]"
     Right [ VRel (Rel
       (Relhead [Term "/a", Term "/x"])
       [ [VText "A1", VInt 20]
       , [VText "A3", VInt 40]
       , [VText "A4", VInt 60] ])]

-}


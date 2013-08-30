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
  litNamedTrees,

  -- * Document

  -- ** Types
  -- $Types

  -- ** Simple data
  -- $SimpleData

  -- ** Compound data
  -- $CompoundData
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Content.Class


-- ----------------------  Type

{-| Make @a@ from list of token trees. -}
type LitTrees  a = B.AbMap2 [B.TokenTree] a

{-| Make @a@ from a token tree. -}
type LitTree   a = B.AbMap2 B.TokenTree a




-- ----------------------  Content

type LitOperators c = [B.Named (LitTree c -> LitTrees c)]

{-| Transform 'TokenTree' into
    internal form of term content. -}
litContentBy :: (CContent c) => LitOperators c -> LitTree c
litContentBy ops = lit where
    lit (B.TreeB t xs) = case t of
          1  ->  paren xs
          2  ->  fmap putList    $ litList    lit xs
          3  ->  fmap putSet     $ litList    lit xs
          4  ->  fmap putTermset $ litTermset lit xs
          5  ->  fmap putRel     $ litRel     lit xs
          _  ->  B.bug

    lit (B.TreeL tok) = case tok of
        B.TWord _ 0 cs@(c:_) | isDecimal c
                       ->  Right . putDec =<< B.litDecimal cs
        B.TWord _ 0 w  ->  case w of
            '#' : s    ->  litHash s
            "()"       ->  Right nil
            _          ->  Left $ B.AbortUnkWord w
        B.TWord _ _ w  ->  Right . putText $ w  -- quoted text
        _              ->  Left $ B.AbortUnknownContent (show tok)

    -- sequence of token trees
    paren (B.TreeL (B.TWord _ 0 tag@(c:_)) : xs)
        | isDecimal c
            = do xs' <- mapM litUnquoted xs
                 Right . putDec =<< B.litDecimal (concat $ tag : xs')
        | otherwise
            = case lookup tag ops of
                 Just f  -> f lit xs
                 Nothing -> Left $ B.AbortUnkCop tag
    paren [] = Right nil
    paren x  = Left $ B.AbortUnknownSymbol (show x)

    -- First letters of decimals
    isDecimal = (`elem` "0123456789+-.")



-- hash word
litHash :: (CBool c) => B.LitString c
litHash "true"    =  Right . putBool $ True
litHash "false"   =  Right . putBool $ False
litHash w         =  Left $ B.AbortUnkWord ('#' : w)

litUnquoted :: LitTree String
litUnquoted (B.TreeL (B.TWord _ 0 w)) = Right w
litUnquoted x = Left $ B.AbortUnknownSymbol (show x)



-- ----------------------  Complex data

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: LitTree String
litFlatname (B.TreeL (B.TTerm _ [n])) = Right n
litFlatname (B.TreeL (B.TTerm _ ns))  = Left $ B.AbortReqFlatname (concat ns)
litFlatname x = Left $ B.AbortMissingTermname (show x)

litList :: (CContent c) => LitTree c -> LitTrees [c]
litList _   [] = Right []
litList lit cs = mapM lt $ B.divideTreesByColon cs where
    lt []  = Right nil
    lt [x] = lit x
    lt xs  = lit $ B.TreeB 1 xs

litRel :: (CContent c) => LitTree c -> LitTrees (B.Rel c)
litRel lit cs =
    do let (h1 : b1) = B.divideTreesByBar cs
       h2 <- mapM litFlatname $ concat $ B.divideTreesByColon h1
       b2 <- mapM (litList lit) b1
       let b3 = B.unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ B.AbortOddRelation
          else Right $ B.Rel (B.headFrom h2) b3

{-| Collect term name and content. -}
litTermset :: (CContent c) => LitTree c -> LitTrees [B.Named c]
litTermset lit xs = namedC where
    namedC   = mapM p       =<< litNamedTrees xs
    p (n, c) = Right . (n,) =<< lit c

{-| Read list of termname and its content.

    >>> litNamedTrees . B.tokenTrees . B.tokens $ "/a 'A3 /b 10"
    Right [("/a", TreeB 1 [TreeL (TWord 3 0 "'"), TreeL (TWord 4 0 "A3")]),
           ("/b", TreeL (TWord 8 0 "10"))]
   -}
litNamedTrees :: LitTrees [B.Named B.TokenTree]
litNamedTrees = name where
    name [] = Right []
    name (x : xs) = let (c, xs2) = cont xs
                    in do n    <- litFlatname x
                          xs2' <- name xs2
                          Right $ (n, B.singleTree c) : xs2'

    cont :: [B.TokenTree] -> ([B.TokenTree], [B.TokenTree])
    cont xs@(B.TreeL (B.TTerm _ _) : _) = ([], xs)
    cont [] = ([], [])
    cont (x : xs) = B.cons1 x $ cont xs






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


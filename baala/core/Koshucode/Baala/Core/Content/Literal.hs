{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Literal
(
  -- * Library

  -- ** Types
  Literalize,
  LitTrees,
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
import qualified Koshucode.Baala.Core.Content.Class    as C
import qualified Koshucode.Baala.Core.Content.HashWord as C


-- ----------------------  Type

{-| Transform 'B.TokenTree' to something. -}
type Literalize a = B.TokenTree -> B.Ab a

{-| Transform list of 'B.TokenTree' to something. -}
type LitTrees a = [B.TokenTree] -> B.Ab a

type LitOperators c = [B.Named (Literalize c -> LitTrees c)]



-- ----------------------  Content

{-| Transform 'B.TokenTree' into
    internal form of content. -}
litContentBy :: (C.CContent c) => LitOperators c -> Literalize c
litContentBy ops = lit where
    lit (B.TreeB typ xs) = case typ of
          1  ->  paren xs
          2  ->  fmap C.putList    $ litList    lit xs
          3  ->  fmap C.putSet     $ litList    lit xs
          4  ->  fmap C.putTermset $ litTermset lit xs
          5  ->  fmap C.putRel     $ litRel     lit xs
          _  ->  B.bug

    lit x@(B.TreeL tok)
        | isDecimal x = do dec <- B.litDecimal $ B.tokenContent tok
                           Right . C.putDec $ dec
        | otherwise   = case tok of
              B.TWord _ 0 w -> word w
              B.TWord _ _ w -> Right . C.putText $ w  -- quoted text
              _             -> Left $ B.AbortSyntax [] $ B.ASUnkWord (show tok)

    word w = case w of
          '#' : s  ->  litHash s
          "()"     ->  Right C.nil
          _        ->  Left $ B.AbortSyntax [] $ B.ASUnkWord w

    paren xs@(x : _)
        -- quoted sequence is text
        | isQuotedOrHashed x =
            C.joinContent =<< mapM lit xs
        -- decimal
        | isDecimal x =
            do xs2 <- mapM litUnquoted xs
               dec <- B.litDecimal $ concat xs2
               Right . C.putDec $ dec

    -- tagged sequence
    paren (B.TreeL (B.TWord _ 0 tag) : xs) =
        case lookup tag ops of
          Just f  -> f lit xs
          Nothing -> Left $ B.AbortAnalysis [] $ B.AAUnkCop tag

    -- empty sequence is nil
    paren [] = Right C.nil

    -- unknown sequence
    paren x  = Left $ B.AbortSyntax [] $ B.ASUnkWord (show x)

-- First letters of decimals
isDecimalChar :: Char -> Bool
isDecimalChar = (`elem` "0123456789+-.")

isDecimal :: B.TokenTree -> Bool
isDecimal (B.TreeL (B.TWord _ 0 (c : _))) = isDecimalChar c
isDecimal _ = False

{-| Check token tree is a quoted word,
    i.e., (1) token tree includes 'B.TWord' token,
          (2) the quotation level is more than zero. -}
isQuoted :: B.TokenTree -> Bool
isQuoted (B.TreeL (B.TWord _ q _)) | q > 0 = True
isQuoted _ = False

{-| Check token tree is an hashed word.
    i.e., (1) token tree includes 'B.TWord' token,
          (2) the quotation level is zero,
    and   (3) the first character is hashsign. -}
isHashed :: B.TokenTree -> Bool
isHashed (B.TreeL (B.TWord _ 0 ('#' : _))) = True
isHashed _ = False

isQuotedOrHashed :: B.TokenTree -> Bool
isQuotedOrHashed x = isQuoted x || isHashed x

litUnquoted :: Literalize String
litUnquoted x@(B.TreeL (B.TWord _ 0 w)) | not $ isHashed x = Right w
litUnquoted x = Left $ B.AbortSyntax [] $ B.ASUnkWord (show x)

litHash :: (C.CContent c) => B.LitString c
litHash key =
    case lookup key hashAssoc of
      Just c  -> Right c
      Nothing -> Left $ B.AbortSyntax [] $ B.ASUnkWord ('#' : key)

hashAssoc :: (C.CContent c) => [B.Named c]
hashAssoc =
    [ ("true"  , C.putBool True)
    , ("false" , C.putBool False)
    , ("nil"   , C.nil)
    ] ++ map f C.hashWordTable where
    f (key, text) = (key, C.putText text)



-- ----------------------  Complex data

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: Literalize String
litFlatname (B.TreeL (B.TTerm _ [n])) = Right n
litFlatname (B.TreeL (B.TTerm _ ns))  = Left $ B.AbortAnalysis [] $ B.AAReqFlatname (concat ns)
litFlatname x = Left $ B.AbortAnalysis [] $ B.AAMissingTermname (show x)

litList :: (C.CContent c) => Literalize c -> LitTrees [c]
litList _   [] = Right []
litList lit cs = mapM lt $ B.divideTreesByColon cs where
    lt []  = Right C.nil
    lt [x] = lit x
    lt xs  = lit $ B.TreeB 1 xs

litRel :: (C.CContent c) => Literalize c -> LitTrees (B.Rel c)
litRel lit cs =
    do let (h1 : b1) = B.divideTreesByBar cs
       h2 <- mapM litFlatname $ concat $ B.divideTreesByColon h1
       b2 <- mapM (litList lit) b1
       let b3 = B.unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ B.AbortSyntax [] $ B.ASOddRelation
          else Right $ B.Rel (B.headFrom h2) b3

{-| Collect term name and content. -}
litTermset :: (C.CContent c) => Literalize c -> LitTrees [B.Named c]
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
                          Right $ (n, B.treeG c) : xs2'

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
               doubly-quoted line: @\'abc@, @\"abc def\"@.

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
               @{| \/a : \/b | \'A1 : 20 | \'A3 : 40 |}@.
-}



-- ----------------------
{- $SimpleData

   Prepere some definitions.

   >>> :m +Koshucode.Baala.Vanilla.Type
   >>> let trees = B.tokenTrees . B.tokens
   >>> let lit  = litContentBy [] :: B.TokenTree -> B.Ab VContent
   >>> let lits = litList lit . trees

   Boolean.

     >>> lits "#true : #false"
     Right [VBool True, VBool False]

   Words.

     >>> lits "'a : 'b #sp 'c"
     Right [VText "a", VText "b c"]

   Decimal.

     >>> lits "12.0"
     Right [VDec (Decimal (120, 10), 1, False)]

   Nil as no ordinary value.

     >>> lits "()"
     Right [VNil]

-}

-- ----------------------
{- $CompoundData

   Set.

     >>> lits "{ 'b : 'a : 'a : 'c : 'a }"
     Right [VSet [VText "b", VText "a", VText "c"]]

   List.

     >>> lits "[ 'a : '10 : 20 ]"
     Right [VList [VText "a", VText "10", VDec (Decimal (20, 1), 0, False)]]

   Termset.

     >>> lits "<| /a 'x  /b { 'y : 'z } |>"
     Right [VTermset
       [ ("/a", VText "x")
       , ("/b", VSet [VText "y", VText "z"])]]

   Relation.

     >>> lits "{| /a : /x | 'A1 : 20 | 'A3 : 40 | 'A4 : 60 |}"
     Right [VRel (Rel
       (Relhead [Term "/a", Term "/x"]),
       [ [VText "A1", VDec (Decimal (20,1), 0, False)]
       , [VText "A3", VDec (Decimal (40,1), 0, False)]
       , [VText "A4", VDec (Decimal (60,1), 0, False)] ])]

-}


{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Literalize
(
  -- * Library

  -- ** Types
  LiteralizeFrom,
  LiteralizeTrees,
  LiteralizeTree,
  LiteralizeString,

  -- ** Functions
  litContent,
  litList,
  litJudge,

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



-- ----------------------  Literalize

{-| Make @a@ from @b@. -}
type LiteralizeFrom b a = b -> AbOr a

{-| Make @a@ from list of token trees. -}
type LiteralizeTrees  a = LiteralizeFrom [TokenTree] a

{-| Make @a@ from a token tree. -}
type LiteralizeTree   a = LiteralizeFrom TokenTree a

{-| Make @a@ from a string. -}
type LiteralizeString a = LiteralizeFrom String a



-- ----------------------  Content

{-| Transform 'TokenTree' into
    internal form of term content. -}
litContent :: (CContent v) => LiteralizeTree v
litContent = lit where
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
          2        ->  Right . putList    =<< litList    xs
          3        ->  Right . putSet     =<< litList    xs
          4        ->  Right . putTermset =<< litTermset xs
          5        ->  Right . putRel     =<< litRel     xs
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
        case tag of
          "text"  -> Right . joinContent =<< litList xs
          "lines" -> Right . joinContent =<< litList xs  -- todo
          "int"   -> litInt xs
          _       -> Left $ AbortUnknownSymbol (show xs)
    paren [] = Right nil
    paren x  = Left $ AbortUnknownSymbol (show x)



-- ----------------------  Simple data

-- litBool
-- litString
-- litNil

litInt :: (CInt v) => LiteralizeTrees v
litInt [TreeL (TWord _ 0 digits)] = 
    Right . putInt =<< readInt digits
litInt xs = Left $ AbortNotNumber (show xs)

readInt :: LiteralizeString Int
readInt s =
    case reads s of
      [(n, "")] -> Right n
      _         -> Left $ AbortNotNumber s

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: LiteralizeTree String
litFlatname (TreeL (TTerm _ [n])) = Right n
litFlatname (TreeL (TTerm _ ns))  = Left $ AbortRequireFlatname (concat ns)
litFlatname x = Left $ AbortMissingTermName (show x)



-- ----------------------  Complex data

{-| Construct list of term contents. -}
litList :: (CContent v) => LiteralizeTrees [v]
litList = mapM litContent

{-| Collect term name and content. -}
litTermset :: (CContent v) => LiteralizeTrees [Named v]
litTermset xs = mapM lit =<< divideByName xs where
    lit (n, c) = do c' <- litContent c
                    Right (n, c')

litRel :: (CContent v) => LiteralizeTrees (Rel v)
litRel cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM (litFlatname) h1
       b2 <- mapM (litList) b1
       let b3 = unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ AbortOddRelation
          else Right $ Rel (headFrom h2) b3

divideByName :: [TokenTree] -> AbOr [Named TokenTree]
divideByName = nam where
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



-- ----------------------  Judge

{-| Construct judge from token trees.
    Judges itself are not content type.
    It can be only used in the top-level of sections. -}
litJudge :: (CContent v) => Bool -> Relsign -> LiteralizeTrees (Judge v)
litJudge q s xs =
  do xs' <- litTermset xs
     Right $ Judge q s xs'



-- ----------------------  Document
{- $Types

   'litContent' recognizes the following types.

   [Boolean]   Boolean used for something is hold or unhold.
               Textual forms: @\#true@, @\#fasle@.

   [Nil]       Nil means that there are no values.
               i.e., universal negation on the term holds.
               Textual form is the non-quoted parens: @()@.

   [String]    Sequence of characters.
               Textual forms: @abc@, @\'abc\'@, @\"abc def\"@,
               @text abc def@.

   [Number]    Number.
               Textual forms: @int 100@, @dec 99.50@, @hex AF@.

   [Set]       Set is an unordered collection of contents.
               Duplication among contents is not significant.
               Textual form is a sequence of contents
               with braces: @{ a b c }@.

   [List]      List is an ordered list of contents.
               Textual form is a sequence of contents
               with square brackets: @[ abc def ]@.

   [Termset]   Termset is a set of terms,
               i.e., a list of named contents.
               Textual form is a sequence of terms
               with bar-braces: @{| \/a 10 \/b 20 |}@.

   [Relation]  Relation is a set of same-type tuples,
               Textual form is a sequence of tuples
               enclosed in bar-bracket.
               The first tuple is a heading of relation,
               and succeeding tuples are delimited by vertical bar:
               @[| \/a \/b | 10 20 | 30 40 |]@.
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


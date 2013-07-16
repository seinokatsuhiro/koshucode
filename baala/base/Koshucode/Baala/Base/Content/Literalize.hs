{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Base.Content.Literalize
(
-- * Library

-- ** Types
  LiteralizeFrom,
  Literalize,
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

import Koshucode.Baala.Base.Content.Class



-- ----------------------  Literalize

{-| Make @a@ from @b@. -}
type LiteralizeFrom b a = [SourceLine] -> b -> AbortOr a

{-| Make @a@ from list of token trees. -}
type Literalize       a = LiteralizeFrom [TokenTree] a

{-| Make @a@ from a token tree. -}
type LiteralizeTree   a = LiteralizeFrom TokenTree a

{-| Make @a@ from a string. -}
type LiteralizeString a = LiteralizeFrom String a



-- ----------------------  Content

{-| Transform 'TokenTree' into
    internal form of term content. -}
litContent :: (Value v) => LiteralizeTree v
litContent src = lit where
    -- leaf
    lit (TreeL (TWord _ q w))
        | q >  0   =   Right . stringValue $ w  -- quoted text
        | q == 0   =   case w of
          '#' : w' ->  hash w'
          "()"     ->  Right nil
          _        ->  Right . stringValue $ w

    -- branch
    lit (TreeB t xs) = case t of
          1        ->  paren xs
          2        ->  Right . listValue    =<< litList    src xs
          3        ->  Right . setValue     =<< litList    src xs
          4        ->  Right . termsetValue =<< litTermset src xs
          5        ->  Right . relValue     =<< litRel     src xs
          _        ->  bug

    -- unknown content
    lit x          =   Left $ AbortUnknownContent src (show x)

    -- hash word
    hash "true"    =   Right . boolValue $ True
    hash "false"   =   Right . boolValue $ False
    hash w         =   case hashWord w of
                         Just w2 -> Right . stringValue $ w2
                         Nothing -> Left $ AbortUnknownSymbol src ('#' : w)

    -- sequence of token trees
    paren (TreeL (TWord _ 0 tag) : xs) =
        case tag of
          "text"  -> Right . joinContent =<< litList src xs
          "lines" -> Right . joinContent =<< litList src xs  -- todo
          "int"   -> litInt src xs
          _       -> Left $ AbortUnknownSymbol src (show xs)
    paren x = Left $ AbortUnknownSymbol src (show x)



-- ----------------------  Simple data

-- litBool
-- litString
-- litNil

litInt :: (IntValue v) => Literalize v
litInt src [TreeL (TWord _ 0 digits)] = 
    Right . intValue =<< readInt src digits
litInt src xs = Left $ AbortNotNumber src (show xs)

readInt :: LiteralizeString Int
readInt src s =
    case reads s of
      [(n, "")] -> Right n
      _         -> Left $ AbortNotNumber src s

{-| Get single term name.
    If 'TokenTree' contains nested term name, this function failed. -}
litFlatname :: LiteralizeTree String
litFlatname _   (TreeL (TTermN _ [n])) = Right n
litFlatname src (TreeL (TTermN _ ns))  = Left $ AbortRequireFlatname src (concat ns)
litFlatname src x = Left $ AbortMissingTermName src (show x)



-- ----------------------  Complex data

{-| Construct list of term contents. -}
litList :: (Value v) => Literalize [v]
litList src = mapM (litContent src)

{-| Collect term name and content. -}
litTermset :: (Value v) => Literalize [Named v]
litTermset src xs = mapM lit =<< divideByName src xs where
    lit (n, c) = do c' <- litContent src c
                    Right (n, c')

litRel :: (Value v) => Literalize (Rel v)
litRel src cs =
    do let (h1 : b1) = divideByTokenTree "|" cs
       h2 <- mapM (litFlatname src) h1
       b2 <- mapM (litList src) b1
       let b3 = unique b2
       if any (length h2 /=) $ map length b3
          then Left  $ AbortOddRelation src
          else Right $ Rel (headFrom h2) b3

divideByName :: [SourceLine] -> [TokenTree] -> AbortOr [Named TokenTree]
divideByName src = nam where
    nam [] = Right []
    nam (x : xs) =
        let (cs, xs2) = content xs
        in do n    <- litFlatname src x
              xs2' <- nam xs2
              Right $ (n, singleToken cs) : xs2'

    content :: [TokenTree] -> ([TokenTree], [TokenTree])
    content xs@(TreeL (TTermN _ _) : _) = ([], xs)
    content [] = ([], [])
    content (x : xs) = cons1 x $ content xs



-- ----------------------  Judge

{-| Construct judge from token trees.
    Judges itself are not content type.
    It can be only used in the top-level of sections. -}
litJudge :: (Value v) => Bool -> Relsign -> Literalize (Judge v)
litJudge q s src xs =
  do xs' <- litTermset src xs
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
     Right [Boolv True, Boolv False]

   Words.

     >>> lit "a 'b c'"
     Right [Stringv "a", Stringv "b c"]

     >>> lit "a 1"
     Right [Stringv "a", Stringv "1"]

   Integer.

     >>> lit "(int 12)"
     Right [Intv 12]

   Nil as no value.

     >>> lit "()"
     Right [Nov]

-}

-- ----------------------
{- $CompoundData

   Set.

     >>> lit "{ b a a c a }"
     Right [Setv [Stringv "b", Stringv "a", Stringv "c"]]

   List.

     >>> lit "[ a 10 (int 20) ]"
     Right [Listv [Stringv "a", Stringv "10", Intv 20]]

   Termset.

     >>> lit "{| /a 0 /b [ a 1 ] |}"
     Right [Termsetv [
         ("/a", Stringv "0"),
         ("/b", Listv [Stringv "a", Stringv "1"])]]

   Relation.

     >>> lit "[| /a /x | A1 (int 20) | A3 (int 40) | A4 (int 60) |]"
     Right [Relv (Rel
        (Relhead [Term "/a", Term "/x"])
        [ [Stringv "A1", Intv 20]
        , [Stringv "A3", Intv 40]
        , [Stringv "A4", Intv 60]
        ])]

-}


{-# OPTIONS_GHC -Wall #-}

-- | Relation on type 'Val'

module Koshucode.Baala.Vanilla.Value.Relval
( terms,
  unionUpTerm,
  vanillaContent,
  vanillaNamedContent,
  vanillaBinary,
  module Koshucode.Baala.Vanilla.Value.Val,
  module Koshucode.Baala.Base.Prelude,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Value.Val

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

terms :: [String] -> [Relterm]
terms = map Term

unionUpTerm :: (Name a) => [String] -> [a] -> [Relterm]
unionUpTerm ns ts = map Term $ unionUp ns $ names ts



-- ----------------------

vanillaContent :: [SourceLine] -> TokenTree -> AbortOr (Relhead -> Content Val)
vanillaContent src ts = 
    do c <- formContent vanillaOp src $ vanillaBinary ts
       Right $ \h -> posContent h c

vanillaNamedContent
  :: [SourceLine] -> Named TokenTree -> AbortOr (Named (Relhead -> Content Val))
vanillaNamedContent src (n, t) =
    do c <- vanillaContent src t
       Right (n, c)

vanillaOp :: String -> Maybe (ContentOp Val)
vanillaOp n = lookup n arithmetic

arithmetic :: [(String, ContentOp Val)]
arithmetic =
 [ namedEager  "+"    plus
 , namedEager  "*"    times
 , namedEager  "="    eq
 , namedEager  "<>"   neq
 , namedEager  "<"    lt
 , namedEager  ">"    gt
 , namedEager  "and"  logiAnd
 , namedLit    "int"  litInt
 ]

abortReason :: AbortReason -> [SourceLine] -> Abort
abortReason a src = (a, src)

litInt :: [TokenTree] -> AbOr Val
litInt [TreeL (TWord _ _ n)] = readIntVal n
litInt xs = Left . abortReason $ AbortNotNumber (show xs)

readIntVal :: String -> AbOr Val
readIntVal s =
    case reads s of
      [(n, "")] -> Right $ Intv n
      _         -> Left . abortReason $ AbortNotNumber s

readInt :: String -> AbOr Int
readInt s =
    case reads s of
      [(n, "")] -> Right $ n
      _         -> Left . abortReason $ AbortNotNumber s

eq :: [Val] -> AbOr Val
eq [x, y] = Right . boolValue $ x == y
eq _      = Left . abortReason $ AbortLookup ""

neq :: [Val] -> AbOr Val
neq [x, y] = Right . boolValue $ x /= y
neq _      = Left . abortReason $ AbortLookup ""

lt :: [Val] -> AbOr Val
lt [x, y] = Right . boolValue $ x < y
lt _      = Left . abortReason $ AbortLookup ""

gt :: [Val] -> AbOr Val
gt [x, y] = Right . boolValue $ x > y
gt _      = Left . abortReason $ AbortLookup ""

logiAnd :: [Val] -> AbOr Val
logiAnd [Boolv True, Boolv True] = Right . boolValue $ True
logiAnd _ = Right . boolValue $ False

plus :: [Val] -> AbOr Val
plus xs = fmap Intv $ loop xs where
    loop [] = Right 0
    loop (Intv n : xs2) = do m <- loop xs2
                             Right $ n + m
    loop (Stringv n : m) = do n' <- readInt n
                              m' <- loop m
                              Right $ n' + m'
    loop _ = Left . abortReason $ AbortLookup ""

times :: [Val] -> AbOr Val
times xs = fmap Intv $ loop xs where
    loop [] = Right 1
    loop (Intv n : xs2) = do m <- loop xs2
                             Right $ n * m
    loop (Stringv n : m) = do n' <- readInt n
                              m' <- loop m
                              Right $ n' * m'
    loop _ = Left . abortReason $ AbortLookup ""

-- let tree = singleToken . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runContent (e2 $ Relhead []) []
-- let Left  e3 = runContent e2 []

{-| Convert infix form to prefix form. -}
vanillaBinary :: Map TokenTree
vanillaBinary = binaryTree ht where
    unbox (TWord _ 0 w) = w
    unbox _ = ""

    right n ws = (Right n, words ws)

    ht = heightTableUnbox unbox
         [ right 8 "or"
         , right 7 "and"
         , right 6 "= <> < > <= >="
         , right 2 "+ -"
         , right 1 "* /"
         ]


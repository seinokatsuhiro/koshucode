{-# OPTIONS_GHC -Wall #-}

-- | Relation on type 'Val'

module Koshucode.Baala.Vanilla.Value.Relval
( terms,
  unionUpTerm,
  vanillaContent,
  module Koshucode.Baala.Vanilla.Value.Val,
  module Koshucode.Baala.Base.Prelude,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content.Run
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Value.Val

{-# ANN module "HLint: ignore Use import/export shortcut" #-}

terms :: [String] -> [Relterm]
terms = map Term

unionUpTerm :: (Name a) => [String] -> [a] -> [Relterm]
unionUpTerm ns ts = map Term $ unionUp ns $ names ts



-- ----------------------

vanillaContent :: [SourceLine] -> TokenTree -> AbortOr (Content Val)
vanillaContent src = formContent vanillaOp src . calcBinary

vanillaOp :: String -> Maybe (ContentOp Val)
vanillaOp n = lookup n op

op :: [(String, ContentOp Val)]
op =
 [ namedEager  "+"    plus
 , namedLit    "int"  litInt
 ]

litInt :: [TokenTree] -> AbOr Val
litInt [TreeL (TWord _ _ n)] = readIntVal n
litInt xs = Left $ \src -> AbortNotNumber src (show xs)

readIntVal :: String -> AbOr Val
readIntVal s =
    case reads s of
      [(n, "")] -> Right $ Intv n
      _         -> Left  $ \src -> AbortNotNumber src s

readInt :: String -> AbOr Int
readInt s =
    case reads s of
      [(n, "")] -> Right $ n
      _         -> Left  $ \src -> AbortNotNumber src s

plus :: [Val] -> AbOr Val
plus xs = fmap Intv $ loop xs where
    loop [] = Right 0
    loop (Intv n : xs2) = do m <- loop xs2
                             Right $ n + m
    loop (Stringv n : m) = do n' <- readInt n
                              m' <- loop m
                              Right $ n' + m'
    loop _ = Left $ \src -> AbortLookup src ""

-- let tree = singleToken . tokenTrees . tokens
-- let Right e1 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "(1 + 2 + 3 + 4 + 5)"
-- let Right e3 = runContent e2 []
-- let Left  e3 = runContent e2 []

calcBinary :: Map TokenTree
calcBinary = binaryTree ht where
    ht = heightTableUnbox unbox
         [ right 8 "or"
         , right 7 "and"
         , right 6 "= <> < > <= >="
         , right 2 "+ -"
         , right 1 "* /"
         ]

    unbox (TWord _ 0 w) = w
    unbox _ = ""

    right n ws = (Right n, words ws)



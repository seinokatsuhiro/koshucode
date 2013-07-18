{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copArith
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Value.Val



-- ----------------------

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




-- ----------------------  Arithmetic

copArith :: [Named (ContentOp Val)]
copArith =
 [ namedEager  "+"    plus
 , namedEager  "*"    times
 , namedEager  "-"    times
 , namedEager  "abs"  times
 , namedLit    "int"  litInt
 ]

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

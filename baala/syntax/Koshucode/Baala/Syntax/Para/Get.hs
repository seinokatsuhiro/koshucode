{-# OPTIONS_GHC -Wall #-}

-- | Getters for parameter.

module Koshucode.Baala.Syntax.Para.Get
  ( -- * Named parameter
    paraGet, paraGetOpt, paraGetList, paraGetSwitch,
    -- * Positional parameter
    paraGetPos, paraGetFst, paraGetSnd, paraGetTrd,
    paraGetRest, paraGetRRest,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax.Para.Para      as S
import qualified Koshucode.Baala.Base.Message          as Msg


-- ----------------------  Named

-- | Named parageter.
paraGet :: (Ord n) => S.Para n a -> n -> B.Ab [a]
paraGet p n =
    case S.paraLookup n p of
      Just [vs]  -> Right vs
      Just _     -> Msg.adlib "multiple-occurence parameter"
      Nothing    -> Msg.adlib "no named parameter"

-- | Named parameter with default content.
paraGetOpt :: (Ord n) => [a] -> S.Para n a -> n -> B.Ab [a]
paraGetOpt def p n =
    case S.paraLookup n p of
      Just [vs]  -> Right vs
      Just _     -> Msg.adlib "multiple-occurence parameter"
      Nothing    -> Right def

-- | Multiple-occurence parameter.
paraGetList :: (Ord n) => S.Para n a -> n -> B.Ab [[a]]
paraGetList p n =
    case S.paraLookup n p of
      Just vss   -> Right vss
      Nothing    -> Msg.adlib "no named parameter"

-- | S.Parameter as switch, just given or not.
paraGetSwitch :: (Ord n) => S.Para n a -> n -> B.Ab Bool
paraGetSwitch p n =
    case S.paraLookup n p of
      Just _     -> Right True
      Nothing    -> Right False


-- ----------------------  Positional

-- | Whole positional parameter list.
paraGetPos :: S.Para n a -> B.Ab [a]

-- | First positional parameter.
paraGetFst :: S.Para n a -> B.Ab a

-- | Second positional parameter.
paraGetSnd :: S.Para n a -> B.Ab a

-- | Third positional parameter.
paraGetTrd :: S.Para n a -> B.Ab a

-- | Positional parameter list but first element.
paraGetRest :: S.Para n a -> B.Ab [a]

-- | Positional parameter list but first and second element.
paraGetRRest :: S.Para n a -> B.Ab [a]

paraGetPos    = Right . S.paraPos
paraGetFst    = listFst   . S.paraPos
paraGetSnd    = listSnd   . S.paraPos
paraGetTrd    = listTrd   . S.paraPos
paraGetRest   = listRest  . S.paraPos
paraGetRRest  = listRRest . S.paraPos

listFst :: [a] -> B.Ab a
listFst (x:_)      = Right x
listFst _          = Msg.adlib "no first parameter"

listSnd :: [a] -> B.Ab a
listSnd (_:x:_)    = Right x
listSnd _          = Msg.adlib "no second parameter"

listTrd :: [a] -> B.Ab a
listTrd (_:_:x:_)  = Right x
listTrd _          = Msg.adlib "no third parameter"

listRest :: [a] -> B.Ab [a]
listRest (_:xs)    = Right xs
listRest _         = Msg.adlib "no rest parameter"

listRRest :: [a] -> B.Ab [a]
listRRest (_:_:xs) = Right xs
listRRest _        = Msg.adlib "no rest parameter"


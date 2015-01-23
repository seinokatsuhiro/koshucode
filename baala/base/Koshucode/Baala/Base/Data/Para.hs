{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Para
  ( -- * Parameter constructor
    Para (..), ParaMap,
    para,

    -- * Types of parameters
    ParaType (..), ParaPosType (..), 

    -- * Creating parameter types
    paraType, paraJust, paraMin, paraMax, paraRange,
    paraReq, paraOpt, paraMult,

    -- * Matching against parameters
    ParaUnmatch (..),
    paraSelect, paraMatch, paraUnmatch,

    -- * Getting parameter elements
    paraGet, paraGetList, paraGetSwitch,
    paraGetPos, paraGetFst, paraGetSnd, paraGetTrd,
    paraGetRest, paraGetRRest,
  ) where

import qualified Data.Map.Strict               as Map
import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Message  as Msg


-- ----------------------  Parameter

data Para a
    = Para
      { paraAll   :: [a]
      , paraPos   :: [a]
      , paraName  :: ParaMap a
      } deriving (Show, Eq, Ord)

type ParaMap a = Map.Map String [[a]]

para :: (a -> Maybe String) -> [a] -> Para a
para name xxs = pos xxs [] where
    make ps            = Para xxs (reverse ps) Map.empty
    pos [] ps          = make ps
    pos (x:xs) ps      = case name x of
                          Nothing  -> pos xs (x:ps)
                          Just n   -> let p = make ps
                                          m = val Map.empty n xs []
                                      in p { paraName = Map.map reverse m }
    val a n []     vs  = add n vs a
    val a n (x:xs) vs  = case name x of
                           Nothing  -> val a n xs (x:vs)
                           Just n2  -> let a2 = add n vs a
                                       in val a2 n2 xs []
    add n vs =
        Map.insertWith (++) n [reverse vs]

paraLookup :: String -> Para a -> Maybe [[a]]
paraLookup n Para { paraName = m } = Map.lookup n m

paraMultiples :: Para a -> [String]
paraMultiples = paraNamesOf (not . B.isSingleton)

paraNamesOf :: ([[a]] -> Bool) -> Para a -> [String]
paraNamesOf f Para { paraName = m } = Map.keys $ Map.filter f m


-- ----------------------  Parameter Type

data ParaType
    = ParaType
      { paraTypePos    :: ParaPosType
      , paraTypeReq    :: [String]
      , paraTypeOpt    :: [String]
      , paraTypeMult   :: [String]
      } deriving (Show, Eq, Ord)

data ParaPosType
    = ParaPosJust  Int
    | ParaPosMin   Int
    | ParaPosMax   Int
    | ParaPosRange Int Int
      deriving (Show, Eq, Ord)

paraType :: ParaType
paraType = ParaType (ParaPosJust 0) [] [] []

paraJust, paraMin, paraMax :: ParaType -> Int -> ParaType
paraJust  ty n = paraCheck $ ty { paraTypePos = ParaPosJust n }
paraMin   ty n = paraCheck $ ty { paraTypePos = ParaPosMin  n }
paraMax   ty n = paraCheck $ ty { paraTypePos = ParaPosMax  n }

paraRange :: ParaType -> (Int, Int) -> ParaType
paraRange ty (m, n) = paraCheck $ ty { paraTypePos = ParaPosRange m n }

paraReq, paraOpt, paraMult :: ParaType -> [String] -> ParaType
paraReq ty ns = paraCheck $ ty { paraTypeReq = ns }
paraOpt ty ns = paraCheck $ ty { paraTypeOpt = ns }
paraMult ty ns = paraCheck $ ty { paraTypeMult = ns }

paraCheck :: B.Map ParaType
paraCheck ty@(ParaType _ req opt mul)
    | null dup   = ty
    | otherwise  = B.bug $ "duplicate para names: " ++ unwords dup
    where ns     = req ++ opt ++ mul
          dup    = B.duplicates ns


-- ----------------------  Unmatch

data ParaUnmatch
    = ParaOutOfRange Int ParaPosType
    | ParaUnknown  [String]
    | ParaMissing  [String]
    | ParaMultiple [String]
      deriving (Show, Eq, Ord)

paraMatch :: Para a -> ParaType -> Bool
paraMatch p t = paraUnmatch p t == Nothing

paraUnmatch :: Para a -> ParaType -> Maybe ParaUnmatch
paraUnmatch p (ParaType pos req opt mul)
    | upos /= Nothing   = upos
    | unknowns  /= []   = Just $ ParaUnknown  unknowns
    | missings  /= []   = Just $ ParaMissing  missings
    | multiples /= []   = Just $ ParaMultiple multiples
    | otherwise         = Nothing
    where
      upos              = paraPosUnmatch (paraPos p) pos
      ns                = Map.keys $ paraName p
      ns2               = paraMultiples p
      total             = req ++ opt ++ mul
      unknowns          = ns  B.\\ total
      missings          = req B.\\ ns
      multiples         = ns2 B.\\ mul

paraPosUnmatch :: [a] -> ParaPosType -> Maybe ParaUnmatch
paraPosUnmatch ps = match where
    match (ParaPosJust c)     | n == c            = Nothing
    match (ParaPosMin  a)     | n >= a            = Nothing
    match (ParaPosMax  b)     | n <= b            = Nothing
    match (ParaPosRange a b)  | n >= a && n <= b  = Nothing
    match p                                       = Just $ ParaOutOfRange n p
    n = length ps

paraSelect :: b -> [(Para a -> b, ParaType)] -> Para a -> b
paraSelect b ps p = loop ps where
    loop [] = b
    loop ((body, ty) : ps2)
        | paraMatch p ty    = body p
        | otherwise         = loop ps2


-- ----------------------  Getters

paraGet :: Para a -> String -> B.Ab [a]
paraGet p n =
    case paraLookup n p of
      Just [vs]  -> Right vs
      Just _     -> Msg.adlib "multiple-occurence parameter"
      Nothing    -> Msg.adlib "no named parameter"

paraGetList :: Para a -> String -> B.Ab [[a]]
paraGetList p n =
    case paraLookup n p of
      Just vss   -> Right vss
      Nothing    -> Msg.adlib "no named parameter"

paraGetSwitch :: Para a -> String -> B.Ab Bool
paraGetSwitch p n =
    case paraLookup n p of
      Just _     -> Right True
      Nothing    -> Right False

paraGetPos :: Para a -> B.Ab [a]
paraGetPos = Right . paraPos

paraGetFst, paraGetSnd, paraGetTrd :: Para a -> B.Ab a
paraGetFst  = listGetFst . paraPos
paraGetSnd  = listGetSnd . paraPos
paraGetTrd  = listGetTrd . paraPos

paraGetRest, paraGetRRest :: Para a -> B.Ab [a]
paraGetRest   = listGetRest  . paraPos
paraGetRRest  = listGetRRest . paraPos

listGetFst :: [a] -> B.Ab a
listGetFst (x:_)      = Right x
listGetFst _          = Msg.adlib "no parameter at 0"

listGetSnd :: [a] -> B.Ab a
listGetSnd (_:x:_)    = Right x
listGetSnd _          = Msg.adlib "no parameter at 1"

listGetTrd :: [a] -> B.Ab a
listGetTrd (_:_:x:_)  = Right x
listGetTrd _          = Msg.adlib "no parameter at 2"

listGetRest :: [a] -> B.Ab [a]
listGetRest (_:xs)    = Right xs
listGetRest _         = Msg.adlib "no rest parameter"

listGetRRest :: [a] -> B.Ab [a]
listGetRRest (_:_:xs) = Right xs
listGetRRest _        = Msg.adlib "no rest parameter"


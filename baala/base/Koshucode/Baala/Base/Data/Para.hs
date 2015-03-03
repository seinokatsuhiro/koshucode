{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Para
  ( -- * Parameter constructor
    Para, ParaBody (..), ParaMap,
    para, paraEmpty,
    paraNameList, paraNameAdd,
    paraPosName, paraMultipleNames, paraNameMapKeys,
    paraLookupSingle,

    -- * Types of parameters
    ParaType (..), ParaPosType (..), 

    -- * Creating parameter types
    paraType, paraJust, paraMin, paraMax, paraRange,
    paraReq, paraOpt, paraMult,

    -- * Matching against parameters
    ParaUnmatch (..),
    paraSelect, paraMatch, paraUnmatch,

    -- * Getting parameter elements
    paraGet, paraGetOpt, paraGetList, paraGetSwitch,
    paraGetPos, paraGetFst, paraGetSnd, paraGetTrd,
    paraGetRest, paraGetRRest,
  ) where

import qualified Data.Generics                 as G
import qualified Data.Map.Strict               as Map
import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Message  as Msg


-- ----------------------  Parameter

type Para a = ParaBody String a

data ParaBody n a
    = ParaBody
      { paraAll   :: [a]
      , paraPos   :: [a]
      , paraName  :: ParaMap n a
      } deriving (Show, Eq, Ord, G.Data, G.Typeable)

type ParaMap n a = Map.Map n [[a]]

-- | Parse list into parameter.
para :: (Ord n) => (a -> Maybe n) -> [a] -> ParaBody n a
para name xxs = pos xxs [] where
    make ps            = ParaBody xxs (reverse ps) Map.empty
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
    add n vs = paraInsert n $ reverse vs

paraEmpty :: ParaBody n a
paraEmpty = ParaBody [] [] Map.empty

-- | Association list of named parameters.
paraNameList :: ParaBody n a -> [(n, [[a]])]
paraNameList ParaBody { paraName = m } = Map.assocs m

-- | Add named parameter.
paraNameAdd :: (Ord n) => n -> [a] -> B.Map (ParaBody n a)
paraNameAdd n vs p@ParaBody { paraName = m } =
    p { paraName = paraInsert n vs m }

paraInsert :: (Ord n) => n -> [a] -> B.Map (ParaMap n a)
paraInsert n a = Map.insertWith (++) n [a]

-- | List of names which appear more than once.
paraMultipleNames :: ParaBody n a -> [n]
paraMultipleNames = paraNamesOf (not . B.isSingleton)

paraNamesOf :: ([[a]] -> Bool) -> ParaBody n a -> [n]
paraNamesOf f ParaBody { paraName = m } = Map.keys $ Map.filter f m

-- | Give names to positional parameters.
paraPosName :: (Ord n, Monad m) => ([a] -> m [(n, [a])]) -> ParaBody n a -> m (ParaBody n a)
paraPosName pn p =
    do ns <- pn $ paraPos p
       let m = Map.fromList $ map (B.mapSnd B.li1) ns
       return $ p { paraName = paraName p `Map.union` m }

-- | Map names of named parameters.
paraNameMapKeys :: (Ord n2) => (n1 -> n2) -> ParaBody n1 a -> ParaBody n2 a
paraNameMapKeys f p@ParaBody { paraName = m } =
    p { paraName = Map.mapKeys f m }

paraLookup :: (Ord n) => n -> ParaBody n a -> Maybe [[a]]
paraLookup n ParaBody { paraName = m } = Map.lookup n m

paraLookupSingle :: (Ord n) => n -> ParaBody n a -> Maybe [a]
paraLookupSingle n p =
    case paraLookup n p of
      Just [vs]  -> Just vs
      Just _     -> Nothing
      Nothing    -> Nothing



-- ----------------------  Parameter Type

data ParaType n
    = ParaType
      { paraTypePos    :: ParaPosType
      , paraTypeReq    :: [n]
      , paraTypeOpt    :: [n]
      , paraTypeMult   :: [n]
      } deriving (Show, Eq, Ord)

data ParaPosType
    = ParaPosJust  Int
    | ParaPosMin   Int
    | ParaPosMax   Int
    | ParaPosRange Int Int
      deriving (Show, Eq, Ord)

paraType :: ParaType n
paraType = ParaType (ParaPosJust 0) [] [] []

paraJust, paraMin, paraMax :: (Show n, Ord n) => ParaType n -> Int -> ParaType n
paraJust  ty n = paraCheck $ ty { paraTypePos = ParaPosJust n }
paraMin   ty n = paraCheck $ ty { paraTypePos = ParaPosMin  n }
paraMax   ty n = paraCheck $ ty { paraTypePos = ParaPosMax  n }

paraRange :: (Show n, Ord n) => ParaType n -> (Int, Int) -> ParaType n
paraRange ty (m, n) = paraCheck $ ty { paraTypePos = ParaPosRange m n }

paraReq, paraOpt, paraMult :: (Show n, Ord n) => ParaType n -> [n] -> ParaType n
paraReq  ty ns = paraCheck $ ty { paraTypeReq = ns }
paraOpt  ty ns = paraCheck $ ty { paraTypeOpt = ns }
paraMult ty ns = paraCheck $ ty { paraTypeMult = ns }

paraCheck :: (Show n, Ord n) => B.Map (ParaType n)
paraCheck ty@(ParaType _ req opt mul)
    | null dup   = ty
    | otherwise  = B.bug $ "duplicate para names: " ++ show dup
    where ns     = req ++ opt ++ mul
          dup    = B.duplicates ns


-- ----------------------  Unmatch

data ParaUnmatch n
    = ParaOutOfRange Int ParaPosType
    | ParaUnknown  [n]
    | ParaMissing  [n]
    | ParaMultiple [n]
      deriving (Show, Eq, Ord)

paraMatch :: (Eq n) => ParaBody n a -> ParaType n -> Bool
paraMatch p t = paraUnmatch p t == Nothing

paraUnmatch :: forall n a. (Eq n) => ParaBody n a -> ParaType n -> Maybe (ParaUnmatch n)
paraUnmatch p (ParaType pos req opt mul)
    | upos /= Nothing   = upos
    | unknowns  /= []   = Just $ ParaUnknown  unknowns
    | missings  /= []   = Just $ ParaMissing  missings
    | multiples /= []   = Just $ ParaMultiple multiples
    | otherwise         = Nothing
    where
      upos              :: Maybe (ParaUnmatch n)
      upos              = paraPosUnmatch (paraPos p) pos
      ns                = Map.keys $ paraName p
      ns2               = paraMultipleNames p
      total             = req ++ opt ++ mul
      unknowns          = ns  B.\\ total
      missings          = req B.\\ ns
      multiples         = ns2 B.\\ mul

paraPosUnmatch :: [a] -> ParaPosType -> Maybe (ParaUnmatch n)
paraPosUnmatch ps = match where
    match (ParaPosJust c)     | n == c            = Nothing
    match (ParaPosMin  a)     | n >= a            = Nothing
    match (ParaPosMax  b)     | n <= b            = Nothing
    match (ParaPosRange a b)  | n >= a && n <= b  = Nothing
    match p                                       = Just $ ParaOutOfRange n p
    n = length ps

paraSelect :: (Eq n) => b -> [(ParaBody n a -> b, ParaType n)] -> ParaBody n a -> b
paraSelect b ps p = loop ps where
    loop [] = b
    loop ((body, ty) : ps2)
        | paraMatch p ty    = body p
        | otherwise         = loop ps2


-- ----------------------  Getters

paraGet :: (Ord n) => ParaBody n a -> n -> B.Ab [a]
paraGet p n =
    case paraLookup n p of
      Just [vs]  -> Right vs
      Just _     -> Msg.adlib "multiple-occurence parameter"
      Nothing    -> Msg.adlib "no named parameter"

paraGetOpt :: (Ord n) => [a] -> ParaBody n a -> n -> B.Ab [a]
paraGetOpt opt p n =
    case paraGet p n of
      Right a  -> Right a
      Left  _  -> Right opt

paraGetList :: (Ord n) => ParaBody n a -> n -> B.Ab [[a]]
paraGetList p n =
    case paraLookup n p of
      Just vss   -> Right vss
      Nothing    -> Msg.adlib "no named parameter"

paraGetSwitch :: (Ord n) => ParaBody n a -> n -> B.Ab Bool
paraGetSwitch p n =
    case paraLookup n p of
      Just _     -> Right True
      Nothing    -> Right False

paraGetPos :: ParaBody n a -> B.Ab [a]
paraGetPos = Right . paraPos

paraGetFst, paraGetSnd, paraGetTrd :: ParaBody n a -> B.Ab a
paraGetFst  = listGetFst . paraPos
paraGetSnd  = listGetSnd . paraPos
paraGetTrd  = listGetTrd . paraPos

paraGetRest, paraGetRRest :: ParaBody n a -> B.Ab [a]
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


{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter type.

module Koshucode.Baala.Syntax.Attr.Para
  ( -- * Parameter constructor
    SimplePara, Para (..), ParaMap, ParaName, paraHyphen,
    para, paraEmpty,
    paraNameList, paraNameAdd,
    paraPosName, paraMultipleNames, paraNameMapKeys,
    paraLookupSingle,

    -- * Getting parameter content
    -- ** Named parameter
    paraGet, paraGetOpt, paraGetList, paraGetSwitch,
    -- ** Positional parameter
    paraGetPos, paraGetFst, paraGetSnd, paraGetTrd,
    paraGetRest, paraGetRRest,
  ) where

import qualified Data.Generics                         as G
import qualified Data.Map.Strict                       as Map
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Base.Message          as Msg


-- ----------------------  Parameter

-- | String-named parameter.
type SimplePara a = Para String a

data Para n a
    = Para
      { paraAll   :: [a]            -- ^ All parameter elements.
      , paraPos   :: [a]            -- ^ Positional parameters.
      , paraName  :: ParaMap n a    -- ^ Named parameters.
      } deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Mapping parameter name to its contents.
type ParaMap n a = Map.Map n [[a]]

instance (Ord n) => B.Monoid (Para n a) where
    mempty        = paraEmpty
    mappend p1 p2 = Para { paraAll   = paraAll  p1 ++  paraAll p2
                             , paraPos   = paraPos  p1 ++  paraPos p2
                             , paraName  = paraName p1 `u` paraName p2 }
        where u = Map.unionWith (++)

-- | Test and take parameter name.
type ParaName n a = a -> Maybe n

paraHyphen :: ParaName String String
paraHyphen ('-' : n)  = Just n
paraHyphen _          = Nothing

-- | Parse list into parameter.
--
-- >>> para paraHyphen $ words "a b"
-- Para { paraAll = ["a","b"], paraPos = ["a","b"], paraName = fromList [] }
--
-- >>> para paraHyphen $ words "a b -x c d"
-- Para { paraAll = ["a","b","-x","c"]
--          , paraPos = ["a","b"]
--          , paraName = fromList [("x",[["c","d"]])] }

para :: (Ord n) => ParaName n a -> [a] -> Para n a
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
    add n vs = paraInsert n $ reverse vs

-- | Empty parameter.
paraEmpty :: Para n a
paraEmpty = Para [] [] Map.empty

-- | Association list of named parameters.
paraNameList :: Para n a -> [(n, [[a]])]
paraNameList Para { paraName = m } = Map.assocs m

-- | Add named parameter.
paraNameAdd :: (Ord n) => n -> [a] -> B.Map (Para n a)
paraNameAdd n vs p@Para { paraName = m } =
    p { paraName = paraInsert n vs m }

paraInsert :: (Ord n) => n -> [a] -> B.Map (ParaMap n a)
paraInsert n a = Map.insertWith (++) n [a]

-- | List of names which appear more than once.
paraMultipleNames :: Para n a -> [n]
paraMultipleNames = paraNamesOf (not . B.isSingleton)

paraNamesOf :: ([[a]] -> Bool) -> Para n a -> [n]
paraNamesOf f Para { paraName = m } = Map.keys $ Map.filter f m

-- | Give names to positional parameters.
paraPosName :: (Ord n, Monad m) => ([a] -> m [(n, [a])]) -> Para n a -> m (Para n a)
paraPosName pn p =
    do ns <- pn $ paraPos p
       let m = Map.fromList $ map (B.mapSnd B.li1) ns
       return $ p { paraName = paraName p `Map.union` m }

-- | Map names of named parameters.
paraNameMapKeys :: (Ord n2) => (n1 -> n2) -> Para n1 a -> Para n2 a
paraNameMapKeys f p@Para { paraName = m } =
    p { paraName = Map.mapKeys f m }

paraLookup :: (Ord n) => n -> Para n a -> Maybe [[a]]
paraLookup n Para { paraName = m } = Map.lookup n m

paraLookupSingle :: (Ord n) => n -> Para n a -> Maybe [a]
paraLookupSingle n p =
    case paraLookup n p of
      Just [vs]  -> Just vs
      Just _     -> Nothing
      Nothing    -> Nothing


-- ----------------------  Getters

paraGet :: (Ord n) => Para n a -> n -> B.Ab [a]
paraGet p n =
    case paraLookup n p of
      Just [vs]  -> Right vs
      Just _     -> Msg.adlib "multiple-occurence parameter"
      Nothing    -> Msg.adlib "no named parameter"

paraGetOpt :: (Ord n) => [a] -> Para n a -> n -> B.Ab [a]
paraGetOpt opt p n =
    case paraGet p n of
      Right a  -> Right a
      Left  _  -> Right opt

paraGetList :: (Ord n) => Para n a -> n -> B.Ab [[a]]
paraGetList p n =
    case paraLookup n p of
      Just vss   -> Right vss
      Nothing    -> Msg.adlib "no named parameter"

paraGetSwitch :: (Ord n) => Para n a -> n -> B.Ab Bool
paraGetSwitch p n =
    case paraLookup n p of
      Just _     -> Right True
      Nothing    -> Right False

paraGetPos :: Para n a -> B.Ab [a]
paraGetPos = Right . paraPos

paraGetFst, paraGetSnd, paraGetTrd :: Para n a -> B.Ab a
paraGetFst  = listGetFst . paraPos
paraGetSnd  = listGetSnd . paraPos
paraGetTrd  = listGetTrd . paraPos

paraGetRest, paraGetRRest :: Para n a -> B.Ab [a]
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


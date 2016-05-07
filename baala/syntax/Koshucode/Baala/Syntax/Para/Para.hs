{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter type.

module Koshucode.Baala.Syntax.Para.Para
  ( -- * Parameter constructor
    Para (..), ParaMap, ParaName,
    para,
    paraNameList,
    paraNames, paraMultipleNames,
    paraLookup, paraLookupSingle,
    paraPosName,
    paraNameAdd, paraNameMapKeys,
    paraTakeFirst, paraTakeLast,

    -- * Simple parameter
    SimplePara, paraHyphen,
  ) where

import qualified Data.Generics                         as G
import qualified Data.Map.Strict                       as Map
import qualified Koshucode.Baala.Base                  as B


-- ----------------------  Parameter

data Para n a
    = Para
      { paraAll   :: [a]            -- ^ All parameter elements.
      , paraPos   :: [a]            -- ^ Positional parameters.
      , paraName  :: ParaMap n a    -- ^ Named parameters.
      } deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Mapping parameter name to its contents.
type ParaMap n a = Map.Map n [[a]]

-- | Empty parameter
instance B.Default (Para n a) where
    def = Para { paraAll = []
               , paraPos = []
               , paraName = Map.empty }

instance (Ord n) => B.Monoid (Para n a) where
    mempty        = B.def
    mappend p1 p2 = Para { paraAll   = paraAll  p1 ++  paraAll p2
                         , paraPos   = paraPos  p1 ++  paraPos p2
                         , paraName  = paraName p1 `u` paraName p2 }
        where u = Map.unionWith (++)

-- | Test and take parameter name.
type ParaName n a = a -> Maybe n

-- | Parse list into parameter.
--
-- >>> para paraHyphen $ words "a b"
-- Para { paraAll = ["a","b"], paraPos = ["a","b"], paraName = fromList [] }
--
-- >>> para paraHyphen $ words "a b -x c d"
-- Para { paraAll = ["a","b","-x","c"]
--      , paraPos = ["a","b"]
--      , paraName = fromList [("x",[["c","d"]])] }

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

-- | Association list of named parameters.
paraNameList :: Para n a -> [(n, [[a]])]
paraNameList = Map.assocs . paraName

-- | List of parameter names.
paraNames :: Para n a -> [n]
paraNames = Map.keys . paraName

-- | List of names which appear more than once.
paraMultipleNames :: Para n a -> [n]
paraMultipleNames = paraNamesOf (not . B.isSingleton)

paraNamesOf :: ([[a]] -> Bool) -> Para n a -> [n]
paraNamesOf f = Map.keys . Map.filter f . paraName

-- | Lookup named parameter.
paraLookup :: (Ord n) => n -> Para n a -> Maybe [[a]]
paraLookup n = Map.lookup n . paraName

-- | Lookup single-occurence parameter.
paraLookupSingle :: (Ord n) => n -> Para n a -> Maybe [a]
paraLookupSingle n p =
    case paraLookup n p of
      Just [vs]  -> Just vs
      Just _     -> Nothing
      Nothing    -> Nothing

-- | Give names to positional parameters.
paraPosName :: (Ord n, Monad m) => ([a] -> m [(n, [a])]) -> Para n a -> m (Para n a)
paraPosName pn p =
    do ns <- pn $ paraPos p
       let m = Map.fromList $ map (B.mapSnd B.li1) ns
       return $ p { paraName = paraName p `Map.union` m }

-- | Add named parameter.
paraNameAdd :: (Ord n) => n -> [a] -> B.Map (Para n a)
paraNameAdd n vs p@Para { paraName = m } =
    p { paraName = paraInsert n vs m }

paraInsert :: (Ord n) => n -> [a] -> B.Map (ParaMap n a)
paraInsert n a = Map.insertWith (++) n [a]

-- | Map names of named parameters.
paraNameMapKeys :: (Ord n2) => (n1 -> n2) -> Para n1 a -> Para n2 a
paraNameMapKeys f p@Para { paraName = m } =
    p { paraName = Map.mapKeys f m }

paraTakeFirst :: (Ord n) => n -> Para n a -> Para n a
paraTakeFirst = paraAdjustName B.takeFirst

paraTakeLast :: (Ord n) => n -> Para n a -> Para n a
paraTakeLast = paraAdjustName B.takeLast

paraAdjustName :: (Ord n) => ([[a]] -> [[a]]) -> n -> Para n a -> Para n a
paraAdjustName f n p@Para {..} = p { paraName = Map.adjust f n paraName }


-- --------------------------------------------  Simple

-- | String-named parameter.
type SimplePara a = Para String a

-- | Parameter name is beginning with hyphen.
--
-- >>> paraHyphen "-foo"
-- Just "foo"
--
-- >>> paraHyphen "bar"
-- Nothing

paraHyphen :: ParaName String String
paraHyphen ('-' : n)  = Just n
paraHyphen _          = Nothing


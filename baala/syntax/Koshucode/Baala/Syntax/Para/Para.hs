{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter type.

module Koshucode.Baala.Syntax.Para.Para
  ( -- * Parameter constructor
    Para (..), ParaTag, ParaMap, ParaName,
    para,
    paraNameList,
    paraNames, paraMultipleNames,
    paraLookup, paraLookupSingle,
    paraPosName,
    paraNameAdd, paraNameMapKeys,
    paraTakeFirst, paraTakeLast,

    -- * Simple parameter
    SimplePara, paraWords, paraHyphen,
  ) where

import qualified Data.Map.Strict                       as Ms
import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B


-- ----------------------  Parameter

-- | Named and positional parameter.
data Para n a
    = Para
      { paraTags  :: [ParaTag]      -- ^ Parameter tags.
      , paraAll   :: [a]            -- ^ All parameter elements.
      , paraPos   :: [a]            -- ^ Positional parameters.
      , paraName  :: ParaMap n a    -- ^ Named parameters.
      } deriving (Show, Eq, Ord)

-- | Parameter tag.
type ParaTag = String

-- | Mapping parameter name to its contents.
type ParaMap n a = Ms.Map n [[a]]

-- | Empty parameter.
instance B.Default (Para n a) where
    def = Para { paraTags = []
               , paraAll  = []
               , paraPos  = []
               , paraName = Ms.empty }

instance (Ord n) => Monoid (Para n a) where
    mempty        = B.def
    mappend p1 p2 = Para { paraTags  = paraTags p1 ++  paraTags p2
                         , paraAll   = paraAll  p1 ++  paraAll p2
                         , paraPos   = paraPos  p1 ++  paraPos p2
                         , paraName  = paraName p1 `u` paraName p2 }
        where u = Ms.unionWith (++)

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
--
para :: (Ord n) => ParaName n a -> [a] -> Para n a
para name xxs = pos xxs [] where
    make ps            = B.def { paraAll = xxs, paraPos = reverse ps }
    pos [] ps          = make ps
    pos (x:xs) ps      = case name x of
                          Nothing  -> pos xs (x:ps)
                          Just n   -> let p = make ps
                                          m = val Ms.empty n xs []
                                      in p { paraName = Ms.map reverse m }
    val a n []     vs  = add n vs a
    val a n (x:xs) vs  = case name x of
                           Nothing  -> val a n xs (x:vs)
                           Just n2  -> let a2 = add n vs a
                                       in val a2 n2 xs []
    add n vs = paraInsert n $ reverse vs

-- | Association list of named parameters.
paraNameList :: Para n a -> [(n, [[a]])]
paraNameList = Ms.assocs . paraName

-- | List of parameter names.
paraNames :: Para n a -> [n]
paraNames = Ms.keys . paraName

-- | List of names which appear more than once.
paraMultipleNames :: Para n a -> [n]
paraMultipleNames = paraNamesOf (not . B.isSingleton)

paraNamesOf :: ([[a]] -> Bool) -> Para n a -> [n]
paraNamesOf f = Ms.keys . Ms.filter f . paraName

-- | Lookup named parameter.
paraLookup :: (Ord n) => n -> Para n a -> Maybe [[a]]
paraLookup n = Ms.lookup n . paraName

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
       let m = Ms.fromList $ map (B.mapSnd B.list1) ns
       return $ p { paraName = paraName p `Ms.union` m }

-- | Add named parameter.
paraNameAdd :: (Ord n) => n -> [a] -> O.Map (Para n a)
paraNameAdd n vs p@Para { paraName = m } =
    p { paraName = paraInsert n vs m }

paraInsert :: (Ord n) => n -> [a] -> O.Map (ParaMap n a)
paraInsert n a = Ms.insertWith (++) n [a]

-- | Map names of named parameters.
paraNameMapKeys :: (Ord n2) => (n1 -> n2) -> Para n1 a -> Para n2 a
paraNameMapKeys f p@Para { paraName = m } =
    p { paraName = Ms.mapKeys f m }

-- | Take first element from multiply given named parameter.
--
--   >>> let p = para paraHyphen $ words "a -x 1 -x 2 -x 3"
--   >>> p
--   Para { ..., paraName = fromList [("x", [["1"],["2"],["3"]])] }
--   >>> paraTakeFirst "x" p
--   Para { ..., paraName = fromList [("x", [["1"]])] }
--
paraTakeFirst :: (Ord n) => n -> Para n a -> Para n a
paraTakeFirst = paraAdjustName B.takeFirst

-- | Take last element from multiply given named parameter.
paraTakeLast :: (Ord n) => n -> Para n a -> Para n a
paraTakeLast = paraAdjustName B.takeLast

paraAdjustName :: (Ord n) => ([[a]] -> [[a]]) -> n -> Para n a -> Para n a
paraAdjustName f n p@Para {..} = p { paraName = Ms.adjust f n paraName }


-- --------------------------------------------  Simple

-- | String-named parameter.
type SimplePara a = Para String a

-- | Composition of word separation and parameter parsing.
paraWords :: (Ord n) => ParaName n String -> String -> Para n String
paraWords name = para name . words

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


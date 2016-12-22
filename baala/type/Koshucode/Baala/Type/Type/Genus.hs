{-# OPTIONS_GHC -Wall #-}

-- | Genus of contents.

module Koshucode.Baala.Type.Type.Genus
  ( -- * Constructor
    Genus (..),
    genusVoid,
    genusType, genusOf,
    genusMember,
    -- * Operator
    genusMeet, genusDiff,
    genusJoin, genusSplit,
    genusInclude, genusExclude,
  ) where

import qualified Data.Set                          as Set
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Data.Class        as D

-- --------------------------------------------  Constructor

-- | Set of various contents.
--   A genus /C/ - /X/ + /Y/ has
--   (1) /C/ is a set of types as representative contents,
--   (2) /X/ is exclusive contents from the set, and
--   (3) /Y/ is inclusive contents to the set.
--
--   Contents in /X/ must be in /C/,
--   contents in /Y/ must not be in /C/.
--   Internal function 'genusFrom' trims unnecessary contents
--   according to this conditions.
--
--   Diagram of genus /G/ = /C/ - /X/ + /Y/:
--
--   > G -------$$$-$$$$$$------$-$$$-$$$$---------$--$----
--   >             :             :   :             :  :
--   > C    T1 |$$$$$$$$$$| T2 |$$$$$$$$$$|        :  :
--   > X           x             x   x             :  :
--   > Y                                           y  y
--
data Genus c = Genus
    { genusTypes     :: Set.Set c  -- ^ Representative contents.
    , genusExclusive :: Set.Set c  -- ^ Particular exclusive contents.
    , genusInclusive :: Set.Set c  -- ^ Particular inclusive contents.
    } deriving (Eq, Ord)

instance (Show c) => Show (Genus c) where
    show (Genus cs xs ys) = gen
        where gen = "Genus " ++ s cs ++ xy " - " xs ++ xy " + " ys
              s   = show . Set.elems
              xy sign zs | Set.null zs = ""
                         | otherwise   = sign ++ s zs

-- | Create genus with trimming unnecessary contents.
genusFrom :: (D.Basis c) => Set.Set c -> Set.Set c -> Set.Set c -> Genus c
genusFrom cs xs ys = Genus cs xs' ys' where
    xs' = keepType cs xs  -- Keep related exclusives
    ys' = omitType cs ys  -- Omit absorbed inclusives

-- | Create genus of single type.
genusType :: (Ord c, D.Basis c) => c -> Genus c
genusType c = genusFrom (Set.singleton c) Set.empty Set.empty

-- | Create genus of single inclusives content.
genusOf :: (Ord c, D.Basis c) => c -> Genus c
genusOf c = genusFrom Set.empty Set.empty (Set.singleton c)

keepType :: (D.Basis c) => O.Bin (Set.Set c)
keepType ts = Set.filter p where
    p c = D.typeOf c `Set.member` Set.map D.typeOf ts

omitType :: (D.Basis c) => O.Bin (Set.Set c)
omitType ts = Set.filter p where
    p c = D.typeOf c `Set.notMember` Set.map D.typeOf ts

-- | Completely empty genus.
genusVoid :: Genus c
genusVoid = Genus Set.empty Set.empty Set.empty

-- | Test membership of content to genus.
genusMember :: (Ord c, D.Basis c) => c -> Genus c -> Bool
genusMember c (Genus ts xs ys)
    | c `Set.member` xs  = False
    | c `Set.member` ys  = True
    | otherwise          = isAnyType c ts

isAnyType :: (D.Basis c) => c -> Set.Set c -> Bool
isAnyType c ts = any (c `D.sameType`) ts


-- --------------------------------------------  Operator

-- | Meet of two genera.
--
--   Meet of /A/ - /X1/ + /Y1/
--
--   > -------$$-$$$$$$$------$-$$$$$-$$--------$---$-----------
--   >          :              :     :          :   :
--   > A  T1 |$$$$$$$$$$| T2 |$$$$$$$$$$|       :   :
--   > X1       x1             x1    x1         :   :
--   > Y1                                       y1  y1
--
--   and /B/ - /X2/ + /Y2/
--
--   > -------------$---------$$$$-$$$$$------$$-$$$$$$$---$----
--   >              :             :             :          :
--   > B            :     T2 |$$$$$$$$$$| T3 |$$$$$$$$$$|  :
--   > X2           :             x2            x2         :
--   > Y2           y2                                     y2
--
--   is:
--
--   > -------------$---------$-$$-$$-$$------------$-----------
--   >              :          :  :  :              :
--   > C            :     T2 |$$$$$$$$$$|           :
--   > X3      (x1) :          x1 x2 x1        (x2) :
--   > Y3           y2                              y1
--   >         <A * Y2 - X1>                   <B * Y1 - X2>
--
genusMeet :: (Ord c, D.Basis c) => O.Bin (Genus c)
genusMeet (Genus a x1 y1) (Genus b x2 y2) = genusFrom c x3 y3 where
    c  = Set.intersection a b
    x3 = Set.union x1 x2
    y3 = (keepType a y2 Set.\\ x1) `Set.union` (keepType b y1 Set.\\ x2)

-- | Difference of two genera.
--
--   Difference of /A/ - /X1/ + /Y1/
--
--   > -------$$$-$$$$$$------$-$$$$$-$$--------$--$-----------$----
--   >           :             :     :          :  :           :
--   > A  T1 |$$$$$$$$$$| T2 |$$$$$$$$$$|       :  :           :
--   > X1        x1            x1    x1         :  :           :
--   > Y1                                       y1 y1          y1
--
--   and /B/ - /X2/ + /Y2/
--
--   > -------------$---------$$$$-$$$$$------$$-$$$$$$$---$--------
--   >              :             :             :          :
--   > B            :     T2 |$$$$$$$$$$| T3 |$$$$$$$$$$|  :
--   > X2           :             x2            x2         :
--   > Y2           y2                                     y2
--
--   is:
--
--   > -------$$$-$$-$$$----------$-------------$--------------$----
--   >           :  :             :             :              :
--   > C  T1 |$$$$$$$$$$|         :             :              :
--   > X3        x1 y2       (x1) : (x1)        :         (y2) :
--   > Y3                         x2            y1             y1
--
genusDiff :: (Ord c, D.Basis c) => O.Bin (Genus c)
genusDiff (Genus a x1 y1) (Genus b x2 y2) = genusFrom c x3 y3 where
    c  = Set.difference a b
    x3 = Set.union x1 y2
    y3 = Set.union (omitType b y1) x2

-- | Join of two genera.
--
--   Join of /A/ - /X1/ + /Y1/
--
--   > -------$$$-$$$$$$------$-$$$$-$$$--------$---------------
--   >           :             :    :           :
--   > A  T1 |$$$$$$$$$$| T2 |$$$$$$$$$$|       :
--   > X1        x1            x1   x1          :
--   > Y1                                       y1
--
--   and /B/ - /X2/ + /Y2/
--
--   > -------------$---------$-$$$$$$$$------$$-$$$$$$$---$----
--   >              :          :                :          :
--   > B            :     T2 |$$$$$$$$$$| T3 |$$$$$$$$$$|  :
--   > X2           :          x2               x2         :
--   > Y2           y2                                     y2
--
--   is:
--
--   > -------$$$-$$$$$$------$-$$$$$$$$------$$$$$$$$$$---$----
--   >           :             :                           :
--   > C  T1 |$$$$$$$$$$| T2 |$$$$$$$$$$| T3 |$$$$$$$$$$|  :
--   > X3        x1            x1 = x2                     :
--   >       <X1 - B - Y2>    <X1 * X2>      <X2 - A - Y1> :
--   > Y3          (y2)                                    y2
--
genusJoin :: (Ord c, D.Basis c) => O.Bin (Genus c)
genusJoin (Genus a x1 y1) (Genus b x2 y2) = genusFrom c x3 y3 where
    c       = Set.union a b
    x3      = Set.unions [omitType b x1, Set.intersection x1 x2, omitType a x2]
               `Set.difference` y3
    y3      = Set.union y1 y2

-- | Split acceptable genus by actual genus.
genusSplit :: (Ord c, D.Basis c) => Genus c -> Genus c -> (Genus c, Genus c)
genusSplit p a = (genusMeet a p, genusDiff a p)

-- | Include specific contents.
genusInclude :: (Ord c, D.Basis c) => Genus c -> [c] -> Genus c
genusInclude g ys = g `genusJoin` Genus Set.empty Set.empty (Set.fromList ys)

-- | Exclude specific contents.
genusExclude :: (Ord c, D.Basis c) => Genus c -> [c] -> Genus c
genusExclude g xs = g `genusJoin` Genus Set.empty (Set.fromList xs) Set.empty


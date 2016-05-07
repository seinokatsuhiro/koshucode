{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Para.ParaSpec
  ( -- * Specification
    ParaSpec (..), ParaSpecPos (..),
    paraSpecNames,

    -- * Unmatch reason
    ParaUnmatch (..), paraMatch, 
    ParaTo, paraSelect, 

    -- * Construction
    ParaSpecMap, paraSpec,
    -- ** Positional
    paraMin, paraMax, paraJust, paraRange,
    -- ** Named positional
    para0, para1, para2, para3,
    paraItem, paraItemOpt,
    -- ** Required / Optional
    paraReq, paraOpt,
    -- ** Multiple-occurence
    paraFirst, paraLast, paraMulti,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax.Para.Para  as S


-- --------------------------------------------  Specification

-- | Parameter specification.
data ParaSpec n
    = ParaSpec
      { paraSpecPos    :: ParaSpecPos n -- ^ Positional parameter
      , paraSpecReq    :: [n]           -- ^ Required parameter
      , paraSpecOpt    :: [n]           -- ^ Optional parameter
      , paraSpecFirst  :: [n]           -- ^ Allow multiple-occurence, use first parameter
      , paraSpecLast   :: [n]           -- ^ Allow multiple-occurence, use last parameter
      , paraSpecMulti  :: [n]           -- ^ Allow multiple-occurence, use all parameters
      } deriving (Show, Eq, Ord)

-- | Positional parameter specification.
data ParaSpecPos n
    = ParaPosMin   Int      -- ^ Lower bound of parameter length
    | ParaPosRange Int Int  -- ^ Lower and upper bound of parameter length
    | ParaItem     [n]
    | ParaItemOpt  [n] n
      deriving (Show, Eq, Ord)

-- | No parameters
instance B.Default (ParaSpec n) where
    def = ParaSpec { paraSpecPos    = ParaPosRange 0 0
                   , paraSpecReq    = []
                   , paraSpecOpt    = []
                   , paraSpecFirst  = []
                   , paraSpecLast   = []
                   , paraSpecMulti  = [] }

-- | List of named parameters.
paraSpecNames :: ParaSpec n -> [n]
paraSpecNames ParaSpec {..} = paraSpecReq ++ paraSpecOpt


-- --------------------------------------------  Unmatch

-- | Unmatch reason of real parameter and its specifition.
data ParaUnmatch n
    = ParaOutOfRange Int (ParaSpecPos n) -- ^ Positional parameter is unmatched.
    | ParaUnknown  [n]    -- ^ Unknown parameter is specified.
    | ParaMissing  [n]    -- ^ Required parameter is missing.
    | ParaMultiple [n]    -- ^ Parameter occurs more than once.
      deriving (Show, Eq, Ord)

-- | Test and revise parameter to satisfy specification.

paraMatch :: (Eq n, Ord n) => ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatch spec p =
    case paraMatchPos spec p of
      Left u -> Left u
      Right p' -> paraMatchNamed spec $ paraReviseNamed spec p'

paraMatchPos :: (Ord n) => ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatchPos spec p = match pos ps where
    match (ParaPosMin  a) _    | n >= a            = Right p
    match (ParaPosRange a b) _ | n >= a && n <= b  = Right p
    match (ParaItem ns) xs
        | length ns == length xs      = Right $ paraAdd ns xs p
    match (ParaItemOpt ns opt) xs
        | length ns     == length xs  = Right $ paraAdd ns xs p
        | length ns + 1 == length xs  = Right $ paraAdd (ns ++ [opt]) xs p
    match _ _                         = Left $ ParaOutOfRange n pos

    pos = paraSpecPos spec
    ps  = S.paraPos p
    n   = length ps

paraAdd :: (Ord n) => [n] -> [a] -> S.Para n a -> S.Para n a
paraAdd (n:ns) (x:xs) p = paraAdd ns xs $ S.paraNameAdd n [x] p
paraAdd [] _ p = p
paraAdd _ [] p = p

paraReviseNamed :: (Ord n) => ParaSpec n -> S.Para n a -> S.Para n a
paraReviseNamed ParaSpec {..} p = p3 where
    p2 = foldr S.paraTakeFirst p paraSpecFirst
    p3 = foldr S.paraTakeLast p2 paraSpecLast

paraMatchNamed :: (Eq n) => ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatchNamed spec@ParaSpec {..} p
    | unknowns  /= []  = Left $ ParaUnknown  unknowns
    | missings  /= []  = Left $ ParaMissing  missings
    | multiples /= []  = Left $ ParaMultiple multiples
    | otherwise        = Right p
    where
      ns          = S.paraNames p
      ns2         = S.paraMultipleNames p
      total       = paraSpecNames spec
      unknowns    = ns B.\\ total
      missings    = paraSpecReq B.\\ ns
      multiples   = ns2 B.\\ paraSpecMulti

-- | Map parameter to some value.
type ParaTo n a b = S.Para n a -> b

-- | Select matched specification and apply 'ParaTo' function.
paraSelect :: (Eq n, Ord n) => b -> [(ParaSpec n, ParaTo n a b)] -> ParaTo n a b
paraSelect b ps p = loop ps where
    loop [] = b
    loop ((spec, paraTo) : ps2) =
        case paraMatch spec p of
          Right p' -> paraTo p'
          Left _   -> loop ps2


-- --------------------------------------------  Construct

type ParaSpecMap n = B.Map (ParaSpec n)

paraSpec :: (Show n, Ord n) => ParaSpecMap n -> ParaSpec n
paraSpec edit = paraCheck $ edit B.def

paraCheck :: (Show n, Ord n) => ParaSpecMap n
paraCheck spec@ParaSpec {..}
    | null dup   = spec
    | otherwise  = B.bug $ "duplicate para names: " ++ show dup
    where dup    = B.duplicates $ paraSpecNames spec

-- ----------------------  Positional

-- | Lower bound of length of positional parameter.
--
-- >>> let s = paraSpec $ paraMin 1
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b"
-- Right Para ...
-- >>> paraMatch s $ S.paraWords S.paraHyphen ""
-- Left ParaOutOfRange ...
paraMin :: Int -> ParaSpecMap n

-- | Upper bound of length of positional parameter.
--
-- >>> let s = paraSpec $ paraMax 1
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a"
-- Right Para ...
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b"
-- Left ParaOutOfRange ...
paraMax :: Int -> ParaSpecMap n

-- | Fixed-length positional parameter.
--
-- >>> let s = paraSpec $ paraJust 1
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a"
-- Right Para ...
-- >>> paraMatch s $ S.paraWords S.paraHyphen ""
-- Left ParaOutOfRange ...
paraJust :: Int -> ParaSpecMap n

-- | Lower and upper bound of length of positional parameter.
--
-- >>> let s = paraSpec $ paraRange 1 2
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a"
-- Right Para ...
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b c"
-- Left ParaOutOfRange ...
paraRange :: Int -> Int -> ParaSpecMap n

paraMin   n   = paraPos $ ParaPosMin   n
paraMax   n   = paraPos $ ParaPosRange 0 n
paraJust  n   = paraPos $ ParaPosRange n n
paraRange m n = paraPos $ ParaPosRange m n

paraPos :: ParaSpecPos n -> ParaSpecMap n
paraPos pos spec = spec { paraSpecPos = pos }

-- ----------------------  Named positonal

-- | No positional parameter.
para0 :: ParaSpecMap n
-- | Named one positional parameter.
para1 :: n -> ParaSpecMap n
-- | Named two positional parameters.
para2 :: n -> n -> ParaSpecMap n
-- | Named three positional parameters.
para3 :: n -> n -> n -> ParaSpecMap n

para0         = paraItem []
para1 a       = paraItem [a]
para2 a b     = paraItem [a,b]
para3 a b c   = paraItem [a,b,c]

-- | Named arbitrary positional parameters.
--
-- >>> let s = paraSpec $ paraItem ["x", "y", "z", "w"]
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b c d"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]), ..., ("w",[["d"]])] })
paraItem :: [n] -> ParaSpecMap n
paraItem ns spec@ParaSpec {..} =
    spec { paraSpecPos = ParaItem ns
         , paraSpecReq = ns ++ paraSpecReq }

-- | Named arbitrary and one optional parameters.
--
-- >>> let s = paraSpec $ paraItemOpt ["x", "y"] "z"
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]),("y",[["b"]])] })
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b c"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]),("y",[["b"]]),("z",[["c"]])] })
paraItemOpt :: [n] -> n -> ParaSpecMap n
paraItemOpt ns n spec@ParaSpec {..} =
    spec { paraSpecPos = ParaItemOpt ns n
         , paraSpecReq = ns ++ paraSpecReq
         , paraSpecOpt = n : paraSpecOpt }


-- ----------------------  Required/optional

-- | Required named parameter.
--
-- >>> let p = S.paraWords S.paraHyphen "-x a -y b"
-- >>> let s = paraSpec $ paraReq ["x"]
-- >>> paraMatch s p
-- Left (ParaUnknown ["y"])
paraReq :: [n] -> ParaSpecMap n

-- | Optional named parameter.
--
-- >>> let p = S.paraWords S.paraHyphen "-x a -y b"
-- >>> let s = paraSpec $ paraReq ["x"] . paraOpt ["y"]
-- >>> paraMatch s p
-- Right (Para { ..., paraName = fromList [("x", [["a"]]), ("y", [["b"]])] })
paraOpt :: [n] -> ParaSpecMap n

-- ----------------------  Multiple-occurence

-- | Allow multiple-occurence and use first parameter.
--
-- >>> let s = paraSpec $ paraReq ["x"] . paraFirst ["x"]
-- >>> paraMatch s $ S.paraWords S.paraHyphen "-x a -x b c"
-- Right (Para { ..., paraName = fromList [("x", [["a"]])] })
paraFirst :: [n] -> ParaSpecMap n

-- | Allow multiple-occurence and use last parameter.
--
-- >>> let s = paraSpec $ paraReq ["x"] . paraLast ["x"]
-- >>> paraMatch s $ S.paraWords S.paraHyphen "-x a -x b c"
-- Right (Para { ..., paraName = fromList [("x", [["b","c"]])] })
paraLast :: [n] -> ParaSpecMap n

-- | Multiple-occurence parameter.
--
-- >>> let s = paraSpec $ paraReq ["x"] . paraMulti ["x"]
-- >>> paraMatch s $ S.paraWords S.paraHyphen "-x a -x b c"
-- Right (Para { ..., paraName = fromList [("x", [["a"],["b","c"]])] })
paraMulti :: [n] -> ParaSpecMap n

paraReq   ns spec  = spec { paraSpecReq   = ns }
paraOpt   ns spec  = spec { paraSpecOpt   = ns }
paraFirst ns spec  = spec { paraSpecFirst = ns }
paraLast  ns spec  = spec { paraSpecLast  = ns }
paraMulti ns spec  = spec { paraSpecMulti = ns }


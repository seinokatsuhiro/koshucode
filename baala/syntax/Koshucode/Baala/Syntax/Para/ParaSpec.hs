{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Para.ParaSpec
  ( -- * Specification
    ParaSpec (..), ParaSpecPos (..),
    paraSpecNames,

    -- * Construction
    ParaSpecMap, paraSpec,
    -- ** Positional parameter
    paraMin, paraMax, paraJust, paraRange,
    -- ** Named parameter
    paraReq, paraOpt, paraFirst, paraLast, paraMulti,

    -- * Unmatch reason
    ParaUnmatch (..), paraMatch, 
    ParaTo, paraSelect, 
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax.Para.Para  as S


-- --------------------------------------------  Specification

-- | Parameter specification.
data ParaSpec n
    = ParaSpec
      { paraSpecPos    :: ParaSpecPos   -- ^ Positional parameter
      , paraSpecReq    :: [n]           -- ^ Required parameter
      , paraSpecOpt    :: [n]           -- ^ Optional parameter
      , paraSpecFirst  :: [n]           -- ^ Allow multiple-occurence, use first parameter
      , paraSpecLast   :: [n]           -- ^ Allow multiple-occurence, use last parameter
      , paraSpecMulti  :: [n]           -- ^ Allow multiple-occurence, use all parameters
      } deriving (Show, Eq, Ord)

-- | Positional parameter specification.
data ParaSpecPos
    = ParaPosMin   Int      -- ^ Lower bound of parameter length
    | ParaPosRange Int Int  -- ^ Lower and upper bound of parameter length
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
paraMin :: Int -> ParaSpecMap n
-- | Upper bound of length of positional parameter.
paraMax :: Int -> ParaSpecMap n
-- | Fixed-length positional parameter.
paraJust :: Int -> ParaSpecMap n
-- | Lower and upper bound of length of positional parameter.
paraRange :: Int -> Int -> ParaSpecMap n

paraMin   n   = paraPos $ ParaPosMin   n
paraMax   n   = paraPos $ ParaPosRange 0 n
paraJust  n   = paraPos $ ParaPosRange n n
paraRange m n = paraPos $ ParaPosRange m n

paraPos :: ParaSpecPos -> ParaSpecMap n
paraPos pos spec = spec { paraSpecPos = pos }

-- ----------------------  Named

-- | Required named parameter.
--
-- >>> let p = S.para S.paraHyphen $ words "-x a -y b"
-- >>> let s = paraSpec $ paraReq ["x"]
-- >>> paraMatch s p
-- Left (ParaUnknown ["y"])
paraReq :: [n] -> ParaSpecMap n

-- | Optional named parameter.
--
-- >>> let p = S.para S.paraHyphen $ words "-x a -y b"
-- >>> let s = paraSpec $ paraReq ["x"] . paraOpt ["y"]
-- >>> paraMatch s p
-- Right (Para { ..., paraName = fromList [("x", [["a"]]), ("y", [["b"]])] })
paraOpt :: [n] -> ParaSpecMap n

-- | Allow multiple-occurence and use first parameter.
--
-- >>> let p = S.para S.paraHyphen $ words "-x a -x b"
-- >>> let s = paraSpec $ paraReq ["x"] . paraFirst ["x"]
-- >>> paraMatch s p
-- Right (Para { ..., paraName = fromList [("x", [["a"]])] })
paraFirst :: [n] -> ParaSpecMap n

-- | Allow multiple-occurence and use last parameter.
--
-- >>> let p = S.para S.paraHyphen $ words "-x a -x b"
-- >>> let s = paraSpec $ paraReq ["x"] . paraLast ["x"]
-- >>> paraMatch s p
-- Right (Para { ..., paraName = fromList [("x", [["b"]])] })
paraLast :: [n] -> ParaSpecMap n

-- | Multiple-occurence parameter.
paraMulti :: [n] -> ParaSpecMap n

paraReq   ns spec  = spec { paraSpecReq   = ns }
paraOpt   ns spec  = spec { paraSpecOpt   = ns }
paraFirst ns spec  = spec { paraSpecFirst = ns }
paraLast  ns spec  = spec { paraSpecLast  = ns }
paraMulti ns spec  = spec { paraSpecMulti = ns }


-- --------------------------------------------  Unmatch

-- | Unmatch reason of real parameter and its specifition.
data ParaUnmatch n
    = ParaOutOfRange Int ParaSpecPos -- ^ Positional parameter is unmatched.
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

paraMatchPos :: ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatchPos spec p = match pos where
    match (ParaPosMin  a)    | n >= a            = Right p
    match (ParaPosRange a b) | n >= a && n <= b  = Right p
    match _                                      = Left $ ParaOutOfRange n pos
    pos = paraSpecPos spec
    n   = length $ S.paraPos p

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


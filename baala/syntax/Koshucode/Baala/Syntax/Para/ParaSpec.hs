{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Parameter specification.

module Koshucode.Baala.Syntax.Para.ParaSpec
  ( -- * Specification
    -- ** ParaSpec
    ParaSpec (..),
    paraSpecNames,
    paraSpecNamesP, paraSpecNamesN,
    -- ** ParaSpecPos
    ParaSpecPos (..),
    paraMinLength,

    -- * Unmatch reason
    ParaUnmatch (..), paraMatch, 
    ParaTo, paraSelect,

    -- * Construction
    ParaSpecMap, paraSpec,
    -- ** Positional
    paraMin, paraMax, paraJust, paraRange,
    -- ** Named positional
    para0, para1, para2, para3,
    paraItem, paraItemOpt, paraItemRest,
    -- ** Required / Optional
    paraReq, paraOpt,
    -- ** Multiple-occurence
    paraFirst, paraLast, paraMulti,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax.Para.Para  as S


-- --------------------------------------------  Specification

-- ----------------------  ParaSpec

-- | Parameter specification.
data ParaSpec n
    = ParaSpec
      { paraSpecPos    :: ParaSpecPos n -- ^ Positional parameter
      , paraSpecReqP   :: [n]  -- ^ Positional required parameters
      , paraSpecOptP   :: [n]  -- ^ Positional optional parameters
      , paraSpecReqN   :: [n]  -- ^ Explicitly-named required parameters
      , paraSpecOptN   :: [n]  -- ^ Explicitly-named optional parameters
      , paraSpecFirst  :: [n]  -- ^ Allow multiple-occurence, use first parameters
      , paraSpecLast   :: [n]  -- ^ Allow multiple-occurence, use last parameters
      , paraSpecMulti  :: [n]  -- ^ Allow multiple-occurence, use all parameters
      } deriving (Show, Eq, Ord)

-- | No parameters
instance B.Default (ParaSpec n) where
    def = ParaSpec { paraSpecPos     = ParaRange 0 0
                   , paraSpecReqP    = []
                   , paraSpecOptP    = []
                   , paraSpecReqN    = []
                   , paraSpecOptN    = []
                   , paraSpecFirst   = []
                   , paraSpecLast    = []
                   , paraSpecMulti   = [] }

instance Functor ParaSpec where
    fmap f spec@ParaSpec {..} =
        spec { paraSpecPos   = fmap f paraSpecPos
             , paraSpecReqP  = fmap f paraSpecReqP
             , paraSpecOptP  = fmap f paraSpecOptP
             , paraSpecReqN  = fmap f paraSpecReqN
             , paraSpecOptN  = fmap f paraSpecOptN
             , paraSpecFirst = fmap f paraSpecFirst
             , paraSpecLast  = fmap f paraSpecLast
             , paraSpecMulti = fmap f paraSpecMulti }

-- | Name list of all parameters.
paraSpecNames :: ParaSpec n -> [n]
paraSpecNames spec = paraSpecNamesP spec ++ paraSpecNamesN spec

-- | Name list of positional parameters.
paraSpecNamesP :: ParaSpec n -> [n]
paraSpecNamesP ParaSpec {..} = paraSpecReqP ++ paraSpecOptP

-- | Name list of explicitly-named parameters.
paraSpecNamesN :: ParaSpec n -> [n]
paraSpecNamesN ParaSpec {..} = paraSpecReqN ++ paraSpecOptN

-- ----------------------  ParaSpecPos

-- | Positional parameter specification.
data ParaSpecPos n
    = ParaItem     Int [n]      -- ^ Named positional parameters
    | ParaItemOpt  Int [n] [n]  -- ^ Named positional and optional parameters
    | ParaItemRest Int [n] n    -- ^ Named positional and rest parameters
    | ParaMin   Int             -- ^ Lower bound of parameter length
    | ParaRange Int Int         -- ^ Lower and upper bound of parameter length
      deriving (Show, Eq, Ord)

instance Functor ParaSpecPos where
    fmap f (ParaItem     a ns)      = ParaItem     a (fmap f ns)
    fmap f (ParaItemOpt  a ns opt)  = ParaItemOpt  a (fmap f ns) (fmap f opt)
    fmap f (ParaItemRest a ns rest) = ParaItemRest a (fmap f ns) (f rest)
    fmap _ (ParaMin a)              = ParaMin a
    fmap _ (ParaRange a b)          = ParaRange a b

-- | Minimal length of positional parameters.
paraMinLength :: ParaSpecPos n -> Int
paraMinLength (ParaItem     a _)    = a
paraMinLength (ParaItemOpt  a _ _)  = a
paraMinLength (ParaItemRest a _ _)  = a
paraMinLength (ParaMin      a)      = a
paraMinLength (ParaRange    a _)    = a


-- --------------------------------------------  Unmatch

-- | Unmatch reason of real parameter and its specifition.
data ParaUnmatch n
    = ParaOutOfRange Int (ParaSpecPos n) -- ^ Positional parameter is unmatched.
    | ParaUnknown  [n]    -- ^ Unknown parameter is specified.
    | ParaMissing  [n]    -- ^ Required parameter is missing.
    | ParaMultiple [n]    -- ^ Parameter occurs more than once.
      deriving (Show, Eq, Ord)

instance Functor ParaUnmatch where
    fmap f (ParaOutOfRange a pos) = ParaOutOfRange a (fmap f pos)
    fmap f (ParaUnknown  ns)      = ParaUnknown      (fmap f ns)
    fmap f (ParaMissing  ns)      = ParaMissing      (fmap f ns)
    fmap f (ParaMultiple ns)      = ParaMultiple     (fmap f ns)

-- | Test and revise parameter to satisfy specification.

paraMatch :: (Eq n, Ord n) => ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatch spec p =
    case paraMatchPos spec p of
      Left u   -> Left u
      Right p' -> paraMatchNamed spec $ paraReviseNamed spec p'

paraMatchPos :: (Ord n) => ParaSpec n -> S.Para n a -> Either (ParaUnmatch n) (S.Para n a)
paraMatchPos spec p = m pos where
    m (ParaItem a ns)        | l == a      = Right $ paraAdd ns ps p
    m (ParaItemOpt a ns opt) | l == a      = Right $ paraAdd ns ps p
                             | l > a && l <= a + length opt
                                           = Right $ paraAdd (ns ++ opt) ps p
    m (ParaItemRest a ns n)  | l >= a      = Right $ paraAddRest ns n ps p
    m (ParaMin a)            | l >= a      = Right p
    m (ParaRange a b)  | l >= a && l <= b  = Right p
    m _                                    = Left $ ParaOutOfRange l pos

    pos = paraSpecPos spec
    ps  = S.paraPos p
    l   = length ps

paraAdd :: (Ord n) => [n] -> [a] -> S.Para n a -> S.Para n a
paraAdd (n:ns) (x:xs) p = paraAdd ns xs $ S.paraNameAdd n [x] p
paraAdd [] _ p = p
paraAdd _ [] p = p

paraAddRest :: (Ord n) => [n] -> n -> [a] -> S.Para n a -> S.Para n a
paraAddRest nns rest xxs = loop nns xxs where
    loop (n:ns) (x:xs) p = loop ns xs $ S.paraNameAdd n [x] p
    loop [] xs p = S.paraNameAdd rest xs p
    loop _ [] p = p

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
      missings    = paraSpecReqN B.\\ ns
      multiples   = ns2 B.\\ paraSpecMulti

-- | Map parameter to some value.
type ParaTo n a b = S.Para n a -> b

-- | Select matched specification and apply 'ParaTo' function.
paraSelect :: (Eq n, Ord n) => b -> [(ParaSpec n, ParaTo n a b)] -> ParaTo n a b
paraSelect b specs p = loop specs where
    loop [] = b
    loop ((spec, paraTo) : specs2) =
        case paraMatch spec p of
          Right p' -> paraTo p'
          Left _   -> loop specs2


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

paraMin   n   = paraPos $ ParaMin   n
paraMax   n   = paraPos $ ParaRange 0 n
paraJust  n   = paraPos $ ParaRange n n
paraRange m n = paraPos $ ParaRange m n

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
    spec { paraSpecPos  = ParaItem (length ns) ns
         , paraSpecReqP = ns ++ paraSpecReqP }

-- | Named arbitrary and one optional parameters.
--
-- >>> let s = paraSpec $ paraItemOpt ["x", "y"] ["z"]
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]),("y",[["b"]])] })
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b c"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]),("y",[["b"]]),("z",[["c"]])] })
paraItemOpt :: [n] -> [n] -> ParaSpecMap n
paraItemOpt ns opt spec@ParaSpec {..} =
    spec { paraSpecPos  = ParaItemOpt (length ns) ns opt
         , paraSpecReqP = ns ++ paraSpecReqP
         , paraSpecOptP = opt ++ paraSpecOptP }

-- | Named arbitrary and one optional parameters.
--
-- >>> let s = paraSpec $ paraItemRest ["x", "y"] "z"
-- >>> paraMatch s $ S.paraWords S.paraHyphen "a b c d"
-- Right (Para { ..., paraName = fromList [("x",[["a"]]),("y",[["b"]]),("z",[["c","d"]])] })
paraItemRest :: [n] -> n -> ParaSpecMap n
paraItemRest ns n spec@ParaSpec {..} =
    spec { paraSpecPos    = ParaItemRest (length ns) ns n
         , paraSpecReqP = ns ++ paraSpecReqP
         , paraSpecOptP = n : paraSpecOptP }


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

paraReq   ns spec  = spec { paraSpecReqN   = ns }
paraOpt   ns spec  = spec { paraSpecOptN   = ns }
paraFirst ns spec  = spec { paraSpecFirst  = ns }
paraLast  ns spec  = spec { paraSpecLast   = ns }
paraMulti ns spec  = spec { paraSpecMulti  = ns }


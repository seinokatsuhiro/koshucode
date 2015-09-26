{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Relkit constructors.

module Koshucode.Baala.Core.Relkit.Construct
  ( -- * General constructor
    relkit, relkitId,
    relkitJust, relkitNothing,
    relkitSetSource,

    -- * Source of relation
    relkitConst, relkitConstEmpty,
    relkitConstSingleton, relkitConstBody,
    relkitSource,

    -- * Relation reference
    relkitCopy, relkitNest, relkitNestVar,

    -- * Local
    Local, Lexical,
    localsLines,
    a2lookup,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Data                as B
import qualified Koshucode.Baala.Core.Relkit.Relkit  as C



-- ----------------------  General constructor

relkit :: Maybe B.Head -> C.RelkitCore c -> C.Relkit c
relkit ho = C.Relkit Nothing ho . B.Sourced []

relkitId :: Maybe B.Head -> C.Relkit c
relkitId ho = relkit ho C.RelkitId

relkitJust :: B.Head -> C.RelkitCore c -> C.Relkit c
relkitJust ho = relkit $ Just ho

relkitNothing :: C.Relkit c
relkitNothing = relkit Nothing C.RelkitId

relkitSetSource :: (B.CodePtr a) => a -> B.Map (C.Relkit c)
relkitSetSource src (C.Relkit hi ho (B.Sourced _ core)) =
    C.Relkit hi ho $ B.Sourced (B.codePtList src) core

instance B.Monoid (C.Relkit c) where
    mempty = relkitConst B.reldee
    mappend (C.Relkit _ _ bo1) (C.Relkit _ ho2 bo2) =
        relkit ho2 $ C.RelkitAppend bo1 bo2


-- ----------------------  Source of relation

relkitConst :: B.Rel c -> C.Relkit c
relkitConst (B.Rel he bo) = relkitJust he $ C.RelkitConst bo

relkitConstEmpty :: [B.TermName] -> C.Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.TermName] -> [c] -> C.Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.TermName] -> [[c]] -> C.Relkit c
relkitConstBody ns bo = kit where
    he  = B.headFrom ns
    kit = relkitJust he $ C.RelkitConst bo

relkitSource :: B.JudgePat -> [B.TermName] -> C.Relkit c
relkitSource p ns = relkitJust he kit where
    he  = B.headFrom ns
    kit = C.RelkitSource p ns


-- ----------------------  Relation reference

relkitCopy :: B.Token -> String -> B.Map (C.Relkit c)
relkitCopy p n (C.Relkit _ ho kitb) = relkit ho $ C.RelkitCopy p n kitb

relkitNest :: B.Token -> [(String, Int)] -> B.Map (C.Relkit c)
relkitNest p nest (C.Relkit _ ho kitb) = relkit ho $ C.RelkitNest p nest kitb

relkitNestVar :: B.Token -> String -> B.Head -> C.Relkit c
relkitNestVar p n he = relkitJust he $ C.RelkitNestVar p n



-- ----------------------  Local relations

type Local a = Lexical [B.Named a]

type Lexical a = (B.Token, a)

localsLines :: [Local a] -> [String]
localsLines xs = map desc $ a2keys xs where
    desc (a, bs) = B.tokenContent a ++ " / " ++ unwords bs

a2keys :: [(a, [(b, c)])] -> [(a, [b])]
a2keys = B.mapSndTo (map fst)

a2expand :: [(a, [(b, c)])] -> [((a, b), c)]
a2expand = concatMap f where
    f (a, bc)   = map (g a) bc
    g a (b, c)  = ((a, b), c)

a2lookup :: (Eq a, Eq b) => a -> b -> [(a, [(b, c)])] -> Maybe c
a2lookup a b = lookup a B.>=> lookup b


{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Relmap.Unary
( -- * Projection
  project
, relEmpty, relmapEmpty

  -- * Naming
, relmapRename
, relRename

  -- * Current
, enclose
, conf
, size
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Minimal.OpKit as Kit
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple



-- ----------------------  projection

project :: (Ord v) => ([Int] -> Listmap v) -> [String] -> a -> Map (Rel v)
project f ns2 _ (Rel h1 b1) = Rel h2 b2 where
    pos = List.sort $ Kit.headPoss h1 (map singleton ns2)
    pj  = f $ Kit.posPoss pos
    h2  = Kit.rehead pj h1
    b2  = unique $ map pj b1

-- | Throw away all tuples in a relation.
relEmpty :: Map (Rel v)
relEmpty (Rel h1 _) = Rel h1 []

relmapEmpty :: Kit.OpUse v -> Kit.Relmap v
relmapEmpty use = Kit.relmapCalc use "empty" sub where
    sub _ = relEmpty



-- ----------------------  Naming

{-| Change term names -}
relmapRename
    :: OpUse v            -- ^ Use of operator
    -> [(String, String)] -- ^ List of term name (/to/, /from/)
    -> Relmap v           -- ^ Rename relmap
relmapRename use np = Kit.relmapCalc use "rename" sub where
    sub _ r1 = relRename np r1

{-| Change terms names -}
relRename :: [(String, String)] -> Map (Rel v)
relRename np (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map re) h1
    pn = map Tuple.swap np
    re p = Maybe.fromMaybe p $ lookup p pn



-- ----------------------  current

-- | Enclose the current relation in a term.
enclose :: (RelValue v) => String -> Kit.Relmap v
enclose n = flow "enclose" $ Kit.withN1 enclose2 n

enclose2 :: (RelValue v) => [String] -> Rel v -> Rel v
enclose2 [n] r@(Rel h1 _) = Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[relValue r]]
enclose2 _ _ = undefined

-- | Current term configuration.
conf :: (StringValue v) => String -> Kit.Relmap v
conf n = flow "conf" $ Kit.withN1 conf2 n

conf2 :: (StringValue v) => [String] -> Rel t -> Rel v
conf2 [n] (Rel h1 _) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[stringValue $ show s]]
    s  = show $ docParen $ doc h1
conf2 _ _ = undefined

-- | Current cardinality.
size :: (IntValue v) => String -> Kit.Relmap v
size n = flow "size" $ Kit.withN1 size2 n

size2 :: (IntValue v) => [String] -> Rel t -> Rel v
size2 [n] (Rel _ b1) = Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[intValue $ length b1]]
size2 _ _ = undefined

flow :: String -> RelmapFun v -> Kit.Relmap v
flow = undefined

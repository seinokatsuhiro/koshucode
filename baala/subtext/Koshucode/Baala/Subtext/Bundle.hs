{-# OPTIONS_GHC -Wall #-}

-- | Bundle of expressions.

module Koshucode.Baala.Subtext.Bundle
  ( -- * Bundle
    Bundle (..), BundleMap,
    NameDepth,
    bundle,
    startExpr,
  ) where

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Subtext.Expr      as S
import qualified Koshucode.Baala.Subtext.Fn        as S
import qualified Koshucode.Baala.Subtext.MinMax    as S
import qualified Koshucode.Baala.Subtext.Operator  as S


-- | Bundle of named expressions.
data Bundle a = Bundle
    { bundleExpr      :: [(S.Name, S.Expr a)]
    , bundleStart     :: S.Expr a
    , bundleSubmatch  :: [NameDepth]
    } deriving (Show, Eq, Ord)

-- | Map version of expression bundle.
type BundleMap a = Map.Map S.Name (S.Expr a)

-- | Submatch name and its depth level.
--   Depth is incremented when entering repeatable expressions.
--   Submatch of non-zero depth is collected in list.
type NameDepth = (S.Name, Int)

-- | Create bundle of subtext expressions.
bundle :: [(S.Name, S.Expr a)] -> Bundle a
bundle exprs =
    Bundle { bundleExpr      = exprs
           , bundleStart     = S.fail
           , bundleSubmatch  = submatches exprs start }
    where
      start = startExpr exprs

-- | The start expression of bundle.
startExpr :: [(S.Name, S.Expr a)] -> S.Expr a
startExpr ((_, e) : _)  = e
startExpr []            = S.fail

-- | List of submatch names.
submatches :: [(S.Name, S.Expr a)] -> S.Expr a -> [NameDepth]
submatches exprs start = Map.assocs $ expr [] 0 start where
    expr ns d (S.ERec e)               = rec ns d e
    expr ns d (S.EBase (S.EChange n))  = ch ns d n
    expr _ _ _                         = Map.empty

    ch ns d n | n `elem` ns        = Map.empty
              | otherwise          = case lookup n exprs of
                                       Nothing -> Map.empty
                                       Just e  -> expr (n:ns) d e
    rec ns d ex =
        case ex of
          S.EOr    es  -> Map.unions (expr ns d <$> es)
          S.ESeq   es  -> Map.unions (expr ns d <$> es)
          S.EAnd   es  -> Map.unions (expr ns d <$> es)
          S.ENot    e  -> expr ns d e
          S.ERep  m e  | S.atMost 1 m  -> expr ns d e
                       | otherwise     -> expr ns (d + 1) e
          S.ELast   e  -> expr ns d e
          S.ESub  n e  -> Map.insertWith max n d $ expr ns d e
          S.EAs   _ e  -> expr ns d e
          S.EGath _ e  -> expr ns d e
          S.EPeek   e  -> expr ns d e


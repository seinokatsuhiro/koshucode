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
import qualified Koshucode.Baala.Overture.Fn       as O
import qualified Koshucode.Baala.Subtext.Expr      as T
import qualified Koshucode.Baala.Subtext.MinMax    as T
import qualified Koshucode.Baala.Subtext.Operator  as T


-- | Bundle of named expressions.
data Bundle a = Bundle
    { bundleExpr      :: [(O.Name, T.Expr a)]
    , bundleStart     :: T.Expr a
    , bundleSubmatch  :: [NameDepth]
    } deriving (Show, Eq, Ord)

-- | Map version of expression bundle.
type BundleMap a = Map.Map O.Name (T.Expr a)

-- | Submatch name and its depth level.
--   Depth is incremented when entering repeatable expressions.
--   Submatch of non-zero depth is collected in list.
type NameDepth = (O.Name, Int)

-- | Create bundle of subtext expressions.
bundle :: [(O.Name, T.Expr a)] -> Bundle a
bundle exprs =
    Bundle { bundleExpr      = exprs
           , bundleStart     = T.fail
           , bundleSubmatch  = submatches exprs start }
    where
      start = startExpr exprs

-- | The start expression of bundle.
startExpr :: [(O.Name, T.Expr a)] -> T.Expr a
startExpr ((_, e) : _)  = e
startExpr []            = T.fail

-- | List of submatch names.
submatches :: [(O.Name, T.Expr a)] -> T.Expr a -> [NameDepth]
submatches exprs start = Map.assocs $ expr [] 0 start where
    expr ns d (T.ERec e)               = rec ns d e
    expr ns d (T.EBase (T.EChange n))  = ch ns d n
    expr _ _ _                         = Map.empty

    ch ns d n | n `elem` ns        = Map.empty
              | otherwise          = case lookup n exprs of
                                       Nothing -> Map.empty
                                       Just e  -> expr (n:ns) d e
    rec ns d ex =
        case ex of
          T.EOr    es  -> Map.unions (expr ns d <$> es)
          T.ESeq   es  -> Map.unions (expr ns d <$> es)
          T.EAnd   es  -> Map.unions (expr ns d <$> es)
          T.ENot    e  -> expr ns d e
          T.ERep  m e  | T.atMost 1 m  -> expr ns d e
                       | otherwise     -> expr ns (d + 1) e
          T.ELast   e  -> expr ns d e
          T.ESub  n e  -> Map.insertWith max n d $ expr ns d e
          T.EAs   _ e  -> expr ns d e
          T.EGath _ e  -> expr ns d e



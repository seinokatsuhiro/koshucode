{-# OPTIONS_GHC -Wall #-}

-- | Bundle of expressions.

module Koshucode.Baala.Subtext.Bundle
  ( -- * Bundle
    Bundle (..), BundleMap,
    NameDepth,
    bundle,
    startExpr,
  ) where

import qualified Data.Map.Strict                   as Ms
import qualified Koshucode.Baala.Overture.Fn       as O
import qualified Koshucode.Baala.Subtext.Expr      as T
import qualified Koshucode.Baala.Subtext.MinMax    as T
import qualified Koshucode.Baala.Subtext.Operator  as T


-- | Bundle of named expressions.
data Bundle as a = Bundle
    { bundleExpr      :: [(O.Name, T.Expr as a)]
    , bundleStart     :: T.Expr as a
    , bundleSubmatch  :: [NameDepth]
    } deriving (Show, Eq, Ord)

-- | Map version of expression bundle.
type BundleMap as a = Ms.Map O.Name (T.Expr as a)

-- | Submatch name and its depth level.
--   Depth is incremented when entering repeatable expressions.
--   Submatch of non-zero depth is collected in list.
type NameDepth = (O.Name, Int)

-- | Create bundle of subtext expressions.
bundle :: [(O.Name, T.Expr as a)] -> Bundle as a
bundle exprs =
    Bundle { bundleExpr      = exprs
           , bundleStart     = T.fail
           , bundleSubmatch  = submatches exprs start }
    where
      start = startExpr exprs

-- | The start expression of bundle.
startExpr :: [(O.Name, T.Expr as a)] -> T.Expr as a
startExpr ((_, e) : _)  = e
startExpr []            = T.fail

-- | List of submatch names.
submatches :: [(O.Name, T.Expr as a)] -> T.Expr as a -> [NameDepth]
submatches exprs start = Ms.assocs $ expr [] 0 start where
    expr ns d (T.ERec e)               = rec ns d e
    expr ns d (T.EBase (T.EChange n))  = ch ns d n
    expr _ _ _                         = Ms.empty

    ch ns d n | n `elem` ns        = Ms.empty
              | otherwise          = case lookup n exprs of
                                       Nothing -> Ms.empty
                                       Just e  -> expr (n:ns) d e
    rec ns d ex =
        case ex of
          T.EOr    es  -> Ms.unions (expr ns d <$> es)
          T.ESeq   es  -> Ms.unions (expr ns d <$> es)
          T.EAnd   es  -> Ms.unions (expr ns d <$> es)
          T.ENot    e  -> expr ns d e
          T.ERep  m e  | T.atMost 1 m  -> expr ns d e
                       | otherwise     -> expr ns (d + 1) e
          T.ELast   e  -> expr ns d e
          T.ESub  n e  -> Ms.insertWith max n d $ expr ns d e
          T.EAs   _ e  -> expr ns d e
          T.EGath _ e  -> expr ns d e



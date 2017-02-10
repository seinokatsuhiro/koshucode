{-# OPTIONS_GHC -Wall #-}

-- | Matching parameter.

module Koshucode.Baala.Subtext.Para
  ( -- * Parameter
    Para (..),
    Submatch,
    createPara,
  ) where

import Prelude hiding (seq, and)

import qualified Data.Map.Strict                   as Ms
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Overture.Fn       as O
import qualified Koshucode.Baala.Subtext.Bundle    as T
import qualified Koshucode.Baala.Subtext.Expr      as T
import qualified Koshucode.Baala.Subtext.MinMax    as T
import qualified Koshucode.Baala.Subtext.Operator  as T


-- | Matching parameter.
data Para as a = Para
  { paraBundle     :: T.BundleMap as a  -- ^ Expression bundle
  , paraRawSubs    :: [Submatch as a]   -- ^ Submatches.
  , paraGather     :: Bool              -- ^ Gather or skip match result
  , paraExpr       :: T.Expr as a       -- ^ Match expression
  , paraPos        :: Int               -- ^ Position on input sequence
  , paraInput      :: as                -- ^ Input sequence
  , paraPrev       :: Maybe a           -- ^ Previous element
  , paraRawOutput  :: as                -- ^ Match result
  } deriving (Show, Eq, Ord)

-- | Submatch result, its name and matched sequence.
type Submatch as a = (O.Name, as)

-- | Create matching parameter from
--   expression bundle and input sequence.
createPara :: (O.List as a) => T.Bundle as a -> as -> Para as a
createPara bun s =
    let bun' = simplify bun
    in Para { paraBundle     = Ms.fromList $ T.bundleExpr bun'
            , paraRawSubs    = []
            , paraGather     = True
            , paraExpr       = T.bundleStart bun'
            , paraPos        = 0
            , paraInput      = s
            , paraPrev       = Nothing
            , paraRawOutput  = O.empty }

-- | Simplify bundle of match expressions.
simplify :: T.Bundle as a -> T.Bundle as a
simplify bun@T.Bundle { T.bundleExpr = es } =
    let es' = fmap (what . reduce) <$> es
    in bun { T.bundleExpr  = es'
           , T.bundleStart = T.startExpr es' }

-- | Remove redundant expressions.
reduce :: T.Expr as a -> T.Expr as a
reduce = top where
    top (T.ERec  r)    = rec r
    top (T.EBase b)    = T.EBase b

    rec (T.EOr    [])                = T.fail
    rec (T.ESeq   [])                = T.succ
    rec (T.EAnd   [])                = T.succ
    rec (T.ERep (T.MinMax _ 0) _)    = T.succ

    rec (T.EOr   [e])                = top e
    rec (T.ESeq  [e])                = top e
    rec (T.EAnd  [e])                = top e
    rec (T.ERep (T.MinMax 1 1) e)    = top e

    rec (T.ERep m1 (T.ERec (T.ERep m2 e)))
                       = top $ T.ERec $ T.ERep (T.times m1 m2) e

    rec (T.EOr    es)  = T.or          (top <$> es)
    rec (T.ESeq   es)  = T.seq         (top <$> es)
    rec (T.EAnd   es)  = T.and         (top <$> es)
    rec (T.ENot    e)  = T.not              (top e)
    rec (T.ERep  m e)  = T.ERec $ T.ERep  m (top e)
    rec (T.ELast   e)  = T.ERec $ T.ELast   (top e)
    rec (T.ESub  n e)  = T.ERec $ T.ESub  n (top e)
    rec (T.EAs   f e)  = T.ERec $ T.EAs   f (top e)
    rec (T.EGath b e)  = T.ERec $ T.EGath b (top e)

-- | Replace context-dependent operator.
what :: T.Expr as a -> T.Expr as a
what = top where
    top (T.ERec r)     = T.ERec $ rec r
    top e              = case replaceWhat T.anySeq e of
                           Nothing -> e
                           Just e' -> e'  -- ?? = { ? }

    rec (T.EOr  es)    = T.EOr  (top <$> es)
    rec (T.ESeq es)    = T.ESeq (seq es)
    rec (T.EAnd es)    = T.EAnd (top <$> es)
    rec (T.ENot e)     = T.ENot (top e)

    rec (T.ERep  m e)  = T.ERep  m (top e)
    rec (T.ELast   e)  = T.ELast   (top e)
    rec (T.ESub  n e)  = T.ESub  n (top e)
    rec (T.EAs   f e)  = T.EAs   f (top e)
    rec (T.EGath b e)  = T.EGath b (top e)
    
    seq (e1 : es@(e2 : _)) =
        let rep = replaceWhat $ T.before e2
        in case (rep e1, rep e2) of
             (Nothing, _)        -> top e1 : seq es
             (Just e1', Nothing) -> e1'    : seq es  -- ?? ++ E = before E ++ E
             (Just _, Just _)    ->          seq es  -- ?? ++ ?? = ??
    seq [e] = [top e]
    seq []  = []

replaceWhat :: T.Expr as a -> T.Expr as a -> Maybe (T.Expr as a)
replaceWhat rep = berryM loop where
    loop (T.EBase T.EWhat)  = Just rep
    loop _                  = Nothing

-- | Map to berry expression.
berryM :: (Monad m) => (T.Expr as a -> m (T.Expr as a)) -> T.Expr as a -> m (T.Expr as a)
berryM f = loop where
    loop (T.ERec (T.ESub n e))  = return . T.ERec . T.ESub  n O.# loop e
    loop (T.ERec (T.EGath b e)) = return . T.ERec . T.EGath b O.# loop e
    loop e = f e

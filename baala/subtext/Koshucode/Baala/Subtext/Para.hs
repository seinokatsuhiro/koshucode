{-# OPTIONS_GHC -Wall #-}

-- | Matching parameter.

module Koshucode.Baala.Subtext.Para
  ( -- * Parameter
    Para (..),
    Submatch,
    createPara,
  ) where

import Prelude hiding (seq, and)

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Subtext.Bundle    as S
import qualified Koshucode.Baala.Subtext.Expr      as S
import qualified Koshucode.Baala.Subtext.Fn        as S
import qualified Koshucode.Baala.Subtext.MinMax    as S
import qualified Koshucode.Baala.Subtext.Operator  as S


-- | Matching parameter.
data Para a = Para
  { paraBundle     :: S.BundleMap a  -- ^ Expression bundle
  , paraRawSubs    :: [Submatch a]   -- ^ Submatches.
  , paraGather     :: Bool           -- ^ Gather or skip match result
  , paraExpr       :: S.Expr a       -- ^ Match expression
  , paraPos        :: Int            -- ^ Position on input sequence
  , paraInput      :: [a]            -- ^ Input sequence
  , paraPrev       :: Maybe a        -- ^ Previous element
  , paraRawOutput  :: [a]            -- ^ Match result
  } deriving (Show, Eq, Ord)

-- | Submatch result, its name and matched sequence.
type Submatch a = (S.Name, [a])

-- | Create matching parameter from
--   expression bundle and input sequence.
createPara :: S.Bundle a -> [a] -> Para a
createPara bun s =
    let bun' = simplify bun
    in Para { paraBundle     = Map.fromList $ S.bundleExpr bun'
            , paraRawSubs    = []
            , paraGather     = True
            , paraExpr       = S.bundleStart bun'
            , paraPos        = 0
            , paraInput      = s
            , paraPrev       = Nothing
            , paraRawOutput  = [] }

-- | Simplify bundle of match expressions.
simplify :: S.Bundle a -> S.Bundle a
simplify bun@S.Bundle { S.bundleExpr = es } =
    let es' = fmap (what . reduce) <$> es
    in bun { S.bundleExpr  = es'
           , S.bundleStart = S.startExpr es' }

-- | Remove redundant expressions.
reduce :: S.Expr a -> S.Expr a
reduce = top where
    top (S.ERec  r)    = rec r
    top (S.EBase b)    = S.EBase b

    rec (S.EOr    [])                = S.fail
    rec (S.ESeq   [])                = S.succ
    rec (S.EAnd   [])                = S.succ
    rec (S.ERep (S.MinMax _ 0) _)    = S.succ

    rec (S.EOr   [e])                = top e
    rec (S.ESeq  [e])                = top e
    rec (S.EAnd  [e])                = top e
    rec (S.ERep (S.MinMax 1 1) e)    = top e

    rec (S.EOr    es)  = S.or          (top <$> es)
    rec (S.ESeq   es)  = S.seq         (top <$> es)
    rec (S.EAnd   es)  = S.and         (top <$> es)
    rec (S.ENot    e)  = S.not              (top e)
    rec (S.ERep  m e)  = S.ERec $ S.ERep  m (top e)
    rec (S.ELast   e)  = S.ERec $ S.ELast   (top e)
    rec (S.ESub  n e)  = S.ERec $ S.ESub  n (top e)
    rec (S.EAs   f e)  = S.ERec $ S.EAs   f (top e)
    rec (S.EGath b e)  = S.ERec $ S.EGath b (top e)

-- | Replace context-dependent operator.
what :: S.Expr a -> S.Expr a
what = top where
    top (S.ERec r)     = S.ERec $ rec r
    top e              = case replaceWhat S.anySeq e of
                           Nothing -> e
                           Just e' -> e'  -- ?? = { ? }

    rec (S.EOr  es)    = S.EOr  (top <$> es)
    rec (S.ESeq es)    = S.ESeq (seq es)
    rec (S.EAnd es)    = S.EAnd (top <$> es)
    rec (S.ENot e)     = S.ENot (top e)

    rec (S.ERep  m e)  = S.ERep  m (top e)
    rec (S.ELast   e)  = S.ELast   (top e)
    rec (S.ESub  n e)  = S.ESub  n (top e)
    rec (S.EAs   f e)  = S.EAs   f (top e)
    rec (S.EGath b e)  = S.EGath b (top e)
    
    seq (e1 : es@(e2 : _)) =
        let rep = replaceWhat $ S.before e2
        in case (rep e1, rep e2) of
             (Nothing, _)        -> top e1 : seq es
             (Just e1', Nothing) -> e1'    : seq es  -- ?? ++ E = before E ++ E
             (Just _, Just _)    ->          seq es  -- ?? ++ ?? = ??
    seq [e] = [top e]
    seq []  = []

replaceWhat :: S.Expr a -> S.Expr a -> Maybe (S.Expr a)
replaceWhat rep = berryM loop where
    loop (S.EBase S.EWhat)  = Just rep
    loop _                  = Nothing

-- | Map to berry expression.
berryM :: (Monad m) => (S.Expr a -> m (S.Expr a)) -> S.Expr a -> m (S.Expr a)
berryM f = loop where
    loop (S.ERec (S.ESub n e))  = return . S.ERec . S.ESub  n =<< loop e
    loop (S.ERec (S.EGath b e)) = return . S.ERec . S.EGath b =<< loop e
    loop e = f e

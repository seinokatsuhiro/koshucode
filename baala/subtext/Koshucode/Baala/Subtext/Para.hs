{-# OPTIONS_GHC -Wall #-}

-- | Matching parameter.

module Koshucode.Baala.Subtext.Para
  ( -- * Parameter
    Para (..),
    Submatch,
    Bundle, BundleMap,
    createPara,
  ) where

import Prelude hiding (seq)

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Subtext.Expr      as S
import qualified Koshucode.Baala.Subtext.Fn        as S
import qualified Koshucode.Baala.Subtext.Operator  as S


-- | Matching parameter.
data Para a = Para
  { paraBundle     :: BundleMap a    -- ^ Expression bundle
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

-- | Bundle of named expressions.
type Bundle a = [(S.Name, S.Expr a)]

-- | Map version of expression bundle.
type BundleMap a = Map.Map S.Name (S.Expr a)

-- | Create matching parameter from
--   expression bundle and input sequence.
createPara :: Bundle a -> [a] -> Para a
createPara es s =
    let es' = matchSimplifyBundle es
    in Para { paraBundle     = Map.fromList es'
            , paraRawSubs    = []
            , paraGather     = True
            , paraExpr       = matchStart es'
            , paraPos        = 0
            , paraInput      = s
            , paraPrev       = Nothing
            , paraRawOutput  = [] }

-- | Select start expression.
matchStart :: Bundle a -> S.Expr a
matchStart ((_, e) : _)  = e
matchStart []            = S.fail

-- | Simplify bundle of match expressions.
matchSimplifyBundle :: Bundle a -> Bundle a
matchSimplifyBundle = fmap $ fmap matchSimplify

-- | Simplify match expression.
matchSimplify :: S.Expr a -> S.Expr a
matchSimplify = top where
    top (S.ERec r)    = S.ERec $ rec r
    top (S.EBase b)   = S.EBase b

    rec (S.EOr es)    = S.EOr  (top <$> es)
    rec (S.ESeq es)   = S.ESeq (seq es)
    rec (S.ENot e es) =
        case replaceWhat S.any e of
          Nothing -> S.ENot (top e) (top <$> es)
          Just e' -> S.ENot e'      (top <$> es) -- what not E = any not E
    rec (S.ERep  m e)  = S.ERep  m $ top e
    rec (S.ESub  n e)  = S.ESub  n $ top e
    rec (S.EGath b e)  = S.EGath b $ top e
    rec (S.EPeek   e)  = S.EPeek   $ top e
    
    seq (e1 : e2 : es) =
        let e2'  = top e2
            rest = e2' : es
            rep  = replaceWhat $ S.before e2'
        in case (rep e1, rep e2') of
             (Nothing, _)        -> top e1 : seq rest
             (Just e1', Nothing) -> e1'    : seq rest  -- what ++ E = before E ++ E
             (Just _, Just _)    ->          seq rest  -- what ++ what = what
    seq [e] = case replaceWhat (S.many S.any) e of
                Nothing -> [top e]                     -- E ++ what = E ++ { ? }
                Just e' -> [e']
    seq []  = []

-- | Replace context-dependent operator.
replaceWhat :: S.Expr a -> S.Expr a -> Maybe (S.Expr a)
replaceWhat e' (S.ERec (S.ESub n e))  = do e2 <- replaceWhat e' e
                                           Just $ S.ERec (S.ESub n e2)
replaceWhat e' (S.ERec (S.EGath b e)) = do e2 <- replaceWhat e' e
                                           Just $ S.ERec (S.EGath b e2)
replaceWhat e' (S.EBase S.EWhat)      = Just e'
replaceWhat _ _                       = Nothing


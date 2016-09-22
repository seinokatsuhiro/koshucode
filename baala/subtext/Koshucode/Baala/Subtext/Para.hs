{-# OPTIONS_GHC -Wall #-}

-- | Matching parameter.

module Koshucode.Baala.Subtext.Para
  ( -- * Parameter
    Para (..),
    Submatch,
    Bundle (..), BundleMap,
    NameDepth,
    bundle,
    createPara,
  ) where

import Prelude hiding (seq, and)

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Subtext.Expr      as S
import qualified Koshucode.Baala.Subtext.Fn        as S
import qualified Koshucode.Baala.Subtext.MinMax    as S
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
data Bundle a = Bundle
    { bundleExpr      :: [(S.Name, S.Expr a)]
    , bundleStart     :: S.Expr a
    , bundleSubmatch  :: [NameDepth]
    } deriving (Show, Eq, Ord)

-- | Map version of expression bundle.
type BundleMap a = Map.Map S.Name (S.Expr a)

-- | Name and depth level.
type NameDepth = (S.Name, Int)

-- | Create bundle of subtext expressions.
bundle :: [(S.Name, S.Expr a)] -> Bundle a
bundle exprs =
    Bundle { bundleExpr      = exprs
           , bundleStart     = S.fail
           , bundleSubmatch  = submatches exprs start }
    where
      start = startExpr exprs

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
          S.EGath _ e  -> expr ns d e
          S.EPeek   e  -> expr ns d e

-- | Create matching parameter from
--   expression bundle and input sequence.
createPara :: Bundle a -> [a] -> Para a
createPara es s =
    let es' = matchSimplifyBundle es
    in Para { paraBundle     = Map.fromList $ bundleExpr es'
            , paraRawSubs    = []
            , paraGather     = True
            , paraExpr       = bundleStart es'
            , paraPos        = 0
            , paraInput      = s
            , paraPrev       = Nothing
            , paraRawOutput  = [] }

-- | Simplify bundle of match expressions.
matchSimplifyBundle :: Bundle a -> Bundle a
matchSimplifyBundle bun@Bundle { bundleExpr = es } =
    let es' = fmap (what . reduce) <$> es
    in bun { bundleExpr  = es'
           , bundleStart = startExpr es' }

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
    rec (S.EGath b e)  = S.ERec $ S.EGath b (top e)
    rec (S.EPeek   e)  = S.ERec $ S.EPeek   (top e)

-- | Replace context-dependent operator.
what :: S.Expr a -> S.Expr a
what = top where
    top (S.ERec r)     = S.ERec $ rec r
    top (S.EBase b)    = S.EBase b

    rec (S.EOr  es)    = S.EOr  (top <$> es)
    rec (S.ESeq es)    = S.ESeq (seq es)
    rec (S.EAnd es)    = S.EAnd (top <$> es)
    rec (S.ENot e)     = S.ENot (top e)

    rec (S.ERep  m e)  = S.ERep  m (top e)
    rec (S.ELast   e)  = S.ELast   (top e)
    rec (S.ESub  n e)  = S.ESub  n (top e)
    rec (S.EGath b e)  = S.EGath b (top e)
    rec (S.EPeek   e)  = S.EPeek   (top e)
    
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

replaceWhat :: S.Expr a -> S.Expr a -> Maybe (S.Expr a)
replaceWhat rep = loop where
    loop (S.ERec (S.ESub n e))  = do e' <- loop e
                                     Just $ S.ERec (S.ESub n e')
    loop (S.ERec (S.EGath b e)) = do e' <- loop e
                                     Just $ S.ERec (S.EGath b e')
    loop (S.EBase S.EWhat)      = Just rep
    loop _                      = Nothing


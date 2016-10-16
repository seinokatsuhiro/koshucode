{-# OPTIONS_GHC -Wall #-}

-- | Matching procedure.

module Koshucode.Baala.Subtext.Match
  ( -- * Match
    GeneralMatch,
    CharMatch,
    MatchResult,
    matchExpr,
    matchBundle,
  ) where

import Prelude hiding (seq)

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Overture.Fn       as O
import qualified Koshucode.Baala.Subtext.Bundle    as T
import qualified Koshucode.Baala.Subtext.Expr      as T
import qualified Koshucode.Baala.Subtext.MinMax    as T
import qualified Koshucode.Baala.Subtext.Operator  as T
import qualified Koshucode.Baala.Subtext.Para      as T

-- | Function from input sequence to match result.
--   When unmatched, 'Nothing' is returned.
type GeneralMatch a = [a] -> Maybe (MatchResult a)

-- | Character sequence (string) version of 'GeneralMatch'.
type CharMatch = GeneralMatch Char

-- | Main and submatches.
type MatchResult a = ([a], [T.Submatch a])

-- | Apply match expression to input sequence.
matchExpr :: (Show a) => T.Expr a -> GeneralMatch a
matchExpr e = matchBundle $ T.bundle [("start", e)]

-- | Apply expression bundle to input sequence.
matchBundle :: (Show a) => T.Bundle a -> GeneralMatch a
matchBundle es s =
    do result <- match $ T.createPara es s
       Just $ matchResult result

-- | Extract match result from parameter.
matchResult :: T.Para a -> MatchResult a
matchResult result = (mainMatch result, subMatches result)

-- | Extract matched text.
mainMatch :: T.Para a -> [a]
mainMatch = reverse . T.paraRawOutput

-- | Extract submatches.
subMatches :: T.Para a -> [T.Submatch a]
subMatches = reverse . T.paraRawSubs

-- | Match procedure.
match :: (Show a) => T.Para a -> Maybe (T.Para a)
match pa@T.Para { T.paraBundle    = bundle
                , T.paraGather    = gather
                , T.paraExpr      = expr
                , T.paraPos       = pos
                , T.paraInput     = s
                , T.paraPrev      = prev
                , T.paraRawOutput = o } = result
    where
      result = case expr of
                 T.ERec  r -> rec r
                 T.EBase b -> base b

      para ? e = match $ para { T.paraExpr = e }

      -- ----------------------  recursive

      rec (T.EOr [])        = Nothing
      rec (T.EOr (e:es))    = case pa ? e of
                                Nothing  -> pa ? T.or es
                                Just pa' -> Just pa'

      rec (T.ESeq [])       = Just pa
      rec (T.ESeq (e:es))   = do pa' <- pa ? e
                                 pa' ? T.seq es

      rec (T.EAnd [])       = Just pa
      rec (T.EAnd (e:es))   = do pa' <- pa ? e
                                 case all matched es of
                                   True  -> Just pa'
                                   False -> Nothing

      rec (T.ENot e)        = case pa ? e of
                                Nothing  -> Just pa
                                Just _   -> Nothing

      rec (T.ERep m e)      = rep m e
      rec (T.ELast  e)      = let m s' = pa { T.paraInput = s' } ? e
                              in firstJust (m <$> reverse (tails s))
      rec (T.ESub n e)      = do pa' <- pa { T.paraRawOutput = [] } ? e
                                 let o'   = T.paraRawOutput pa'
                                     subs = T.paraRawSubs pa'
                                 Just $ pa' { T.paraRawSubs   = (n, reverse o') : subs
                                            , T.paraRawOutput = o' ++ o }
      rec (T.EAs (O.Fn _ f) e)
                            = do pa' <- pa { T.paraRawOutput = [] } ? e
                                 let o' = T.paraRawOutput pa'
                                 Just $ pa' { T.paraRawOutput = reverse (f o') ++ o }
      rec (T.EGath b e)     = do pa' <- pa { T.paraGather = b } ? e
                                 Just $ pa' { T.paraGather = gather }

      matched e             = case pa ? e of
                                 Nothing -> False
                                 Just _  -> True

      -- ----------------------  repetition

      -- { E } ( A to )    = { E } ( A - 1 to )
      -- { E } ( A to B )  = { E } ( A - 1 to B - 1 )

      rep (T.Min a) e = case pa ? e of
                          Nothing  -> lower a
                          Just pa' -> next pa' $ T.min (a - 1) e

      rep (T.MinMax a b) e
          | b < a     = Nothing
          | b <= 0    = Just pa
          | otherwise = case pa ? e of
                           Nothing  -> lower a
                           Just pa' -> next pa' $ T.minMax (a - 1) (b - 1) e

      lower a | a <= 0     = Just pa
              | otherwise  = Nothing

      next pa' e' | pos == T.paraPos pa'  = Just pa'   -- matched but not consumed
                  | otherwise             = pa' ? e'

      -- ----------------------  base

      base (T.EElem (O.Fn _ f)) =
          case s of
            x : s' | f x   -> Just $ pa { T.paraPos       = pos + 1
                                        , T.paraInput     = s'
                                        , T.paraPrev      = Just x
                                        , T.paraRawOutput = output $ x : o }
            _              -> Nothing

      base (T.ESpan (O.Fn _ f)) =
          do (w, s') <- f s
             Just $ pa { T.paraPos       = pos + length w
                       , T.paraInput     = s'
                       , T.paraPrev      = case reverse w of
                                             x : _ -> Just x
                                             []    -> prev
                       , T.paraRawOutput = output $ w ++ o }

      base (T.EInter (O.Fn2 _ f)) =
          case s of
            r : _  | f prev (Just r)   -> Just pa
            []     | f prev (Nothing)  -> Just pa
            _                          -> Nothing

      base (T.EChange n) =
          do e' <- Map.lookup n bundle
             pa ? e'

      base (T.EAlways b)       = when b
      base T.EWhat             = Nothing

      output o' | gather    = o'
                | otherwise = o

      when True   = Just pa
      when False  = Nothing

tails :: [a] -> [[a]]
tails []         = [[]]
tails xs@(_:xs') = xs : tails xs'

firstJust :: [Maybe a] -> Maybe a
firstJust []              = Nothing
firstJust (Just x  : _)   = Just x
firstJust (Nothing : xs ) = firstJust xs


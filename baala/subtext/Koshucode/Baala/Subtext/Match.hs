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
import qualified Koshucode.Baala.Subtext.Bundle    as S
import qualified Koshucode.Baala.Subtext.Expr      as S
import qualified Koshucode.Baala.Subtext.Fn        as S
import qualified Koshucode.Baala.Subtext.MinMax    as S
import qualified Koshucode.Baala.Subtext.Operator  as S
import qualified Koshucode.Baala.Subtext.Para      as S

-- | Function from input sequence to match result.
--   When unmatched, 'Nothing' is returned.
type GeneralMatch a = [a] -> Maybe (MatchResult a)

-- | Character sequence (string) version of 'GeneralMatch'.
type CharMatch = GeneralMatch Char

-- | Main and submatches.
type MatchResult a = ([a], [S.Submatch a])

-- | Apply match expression to input sequence.
matchExpr :: (Show a) => S.Expr a -> GeneralMatch a
matchExpr e = matchBundle $ S.bundle [("start", e)]

-- | Apply expression bundle to input sequence.
matchBundle :: (Show a) => S.Bundle a -> GeneralMatch a
matchBundle es s =
    do result <- match $ S.createPara es s
       Just $ matchResult result

-- | Extract match result from parameter.
matchResult :: S.Para a -> MatchResult a
matchResult result = (mainMatch result, subMatches result)

-- | Extract matched text.
mainMatch :: S.Para a -> [a]
mainMatch = reverse . S.paraRawOutput

-- | Extract submatches.
subMatches :: S.Para a -> [S.Submatch a]
subMatches = reverse . S.paraRawSubs

-- | Match procedure.
match :: (Show a) => S.Para a -> Maybe (S.Para a)
match pa@S.Para { S.paraBundle    = bundle
                , S.paraGather    = gather
                , S.paraExpr      = expr
                , S.paraPos       = p
                , S.paraInput     = s
                , S.paraPrev      = prev
                , S.paraRawOutput = o } = result
    where
      result = case expr of
                 S.ERec  r -> rec r
                 S.EBase b -> base b

      -- ----------------------  recursive

      rec (S.EOr [])        = Nothing
      rec (S.EOr (e:es))    = case match $ pa { S.paraExpr = e } of
                                Nothing  -> match $ pa { S.paraExpr = S.or es }
                                Just pa' -> Just pa'

      rec (S.ESeq [])       = Just pa
      rec (S.ESeq (e:es))   = do pa' <- match $ pa { S.paraExpr = e }
                                 match $ pa' { S.paraExpr = S.seq es }

      rec (S.EAnd [])       = Just pa
      rec (S.EAnd (e:es))   = do pa' <- match $ pa { S.paraExpr = e }
                                 case all matched es of
                                   True  -> Just pa'
                                   False -> Nothing

      rec (S.ENot e)        = case match $ pa { S.paraExpr = e } of
                                Nothing  -> Just pa
                                Just _   -> Nothing

      rec (S.ERep m e)      = rep m e
      rec (S.ELast  e)      = let m s' = match $ pa { S.paraExpr = e, S.paraInput = s' }
                              in firstJust (m <$> reverse (tails s))
      rec (S.ESub n e)      = do pa' <- match $ pa { S.paraExpr = e, S.paraRawOutput = [] }
                                 let o'   = S.paraRawOutput pa'
                                     subs = S.paraRawSubs pa'
                                 Just $ pa' { S.paraRawSubs   = (n, reverse o') : subs
                                            , S.paraRawOutput = o' ++ o }
      rec (S.EAs (S.Fn _ f) e)
                            = do pa' <- match $ pa { S.paraExpr = e, S.paraRawOutput = [] }
                                 let o' = S.paraRawOutput pa'
                                 Just $ pa' { S.paraRawOutput = reverse (f o') ++ o }
      rec (S.EGath b e)     = do pa' <- match $ pa { S.paraGather = b, S.paraExpr = e }
                                 Just $ pa' { S.paraGather = gather }

      matched e             = case match $ pa { S.paraExpr = e } of
                                 Nothing -> False
                                 Just _  -> True

      -- ----------------------  repetition

      -- { E } ( A to )    = { E } ( A - 1 to )
      -- { E } ( A to B )  = { E } ( A - 1 to B - 1 )

      rep (S.Min a) e = case match $ pa { S.paraExpr = e } of
                          Nothing  -> lower a
                          Just pa' -> next pa' $ S.min (a - 1) e

      rep (S.MinMax a b) e
          | b < a     = Nothing
          | b <= 0    = Just pa
          | otherwise = case match $ pa { S.paraExpr = e } of
                           Nothing  -> lower a
                           Just pa' -> next pa' $ S.minMax (a - 1) (b - 1) e

      lower a | a <= 0     = Just pa
              | otherwise  = Nothing

      next pa' e' | p == S.paraPos pa'  = Just pa'   -- matched but not consumed
                  | otherwise           = match $ pa' { S.paraExpr = e' }

      -- ----------------------  base

      base (S.EElem (S.Fn _ f)) =
          case s of
            x : s' | f x   -> Just $ pa { S.paraPos       = p + 1
                                        , S.paraInput     = s'
                                        , S.paraPrev      = Just x
                                        , S.paraRawOutput = output $ x : o }
            _              -> Nothing

      base (S.ESpan (S.Fn _ f)) =
          do (w, s') <- f s
             Just $ pa { S.paraPos       = p + length w
                       , S.paraInput     = s'
                       , S.paraPrev      = case reverse w of
                                             x : _ -> Just x
                                             []    -> prev
                       , S.paraRawOutput = output $ w ++ o }

      base (S.EInter (S.Fn2 _ f)) =
          case s of
            r : _  | f prev (Just r)   -> Just pa
            []     | f prev (Nothing)  -> Just pa
            _                          -> Nothing

      base (S.EChange n) =
          do e' <- Map.lookup n bundle
             match $ pa { S.paraExpr = e' }

      base (S.EAlways b)       = when b
      base S.EWhat             = Nothing

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


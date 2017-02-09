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

import qualified Data.Map.Strict                   as Ms
import qualified Koshucode.Baala.Overture          as O
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
type MatchResult a = ([a], [T.Submatch [a] a])

-- | Apply match expression to input sequence.
matchExpr :: (Show a) => T.Expr [a] a -> GeneralMatch a
matchExpr e = matchBundle $ T.bundle [("start", e)]

-- | Apply expression bundle to input sequence.
matchBundle :: (Show a) => T.Bundle [a] a -> GeneralMatch a
matchBundle es s =
    do result <- match $ T.createPara es s
       Just $ matchResult result

-- | Extract match result from parameter.
matchResult :: T.Para [a] a -> MatchResult a
matchResult result = (mainMatch result, subMatches result)

-- | Extract matched text.
mainMatch :: (O.List as a) => T.Para as a -> as
mainMatch = O.reverse . T.paraRawOutput

-- | Extract submatches.
subMatches :: T.Para as a -> [T.Submatch as a]
subMatches = O.reverse . T.paraRawSubs

-- | Match procedure.
match :: (O.List as a, Show a) => T.Para as a -> Maybe (T.Para as a)
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
                              in firstJust (m <$> O.reverse (tails s))
      rec (T.ESub n e)      = do pa' <- pa { T.paraRawOutput = O.empty } ? e
                                 let o'   = T.paraRawOutput pa'
                                     subs = T.paraRawSubs pa'
                                 Just $ pa' { T.paraRawSubs   = (n, O.reverse o') : subs
                                            , T.paraRawOutput = o' O.++ o }
      rec (T.EAs (O.Fn _ f) e)
                            = do pa' <- pa { T.paraRawOutput = O.empty } ? e
                                 let o' = T.paraRawOutput pa'
                                 Just $ pa' { T.paraRawOutput = O.reverse (f o') O.++ o }
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
          case O.cut s of
            Just (x, s') | f x
                 -> Just $ pa { T.paraPos       = pos + 1
                              , T.paraInput     = s'
                              , T.paraPrev      = Just x
                              , T.paraRawOutput = output $ x O.<:> o }
            _    -> Nothing

      base (T.ESpan (O.Fn _ f)) =
          do (w, s') <- f s
             Just $ pa { T.paraPos       = pos + O.length w
                       , T.paraInput     = s'
                       , T.paraPrev      = case O.cut $ O.reverse w of
                                             Just (x, _) -> Just x
                                             Nothing     -> prev
                       , T.paraRawOutput = output $ w O.++ o }

      base (T.EInter (O.Fn2 _ f)) =
          case O.cut s of
            Just (r, _) | f prev (Just r)  -> Just pa
            Nothing     | f prev (Nothing) -> Just pa
            _                              -> Nothing

      base (T.EChange n) =
          do e' <- Ms.lookup n bundle
             pa ? e'

      base (T.EAlways b)       = when b
      base T.EWhat             = Nothing

      output o' | gather    = o'
                | otherwise = o

      when True   = Just pa
      when False  = Nothing

tails :: (O.List es e) => es -> [es]
tails es = case O.cut es of
             Just (_, es') -> es O.<:> tails es'
             Nothing       -> [O.empty]

firstJust :: [Maybe a] -> Maybe a
firstJust []              = Nothing
firstJust (Just x  : _)   = Just x
firstJust (Nothing : xs ) = firstJust xs


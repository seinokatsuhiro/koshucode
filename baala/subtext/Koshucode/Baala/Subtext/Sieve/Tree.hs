{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token tree for sieve pattern. 

module Koshucode.Baala.Subtext.Sieve.Tree
  ( SivTree,
    sivTrees,
    SivExpr,
    ToSivExpr (..),
    toSivExprOr,
    sivMatch,
    sivMatchExpr,
  ) where

import Prelude hiding (seq, min, max)
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Subtext.Match          as S
import qualified Koshucode.Baala.Subtext.Operator       as S
import qualified Koshucode.Baala.Subtext.Sieve.Token    as S
import qualified Koshucode.Baala.Base.Message           as Msg

-- | Sieve tree.
type SivTree t = B.CodeTree S.SivBracket S.SivToken t

-- | Parse sieve token list.
--
--   >>> mapM_ B.printTree O.# B.abortLeft (sivTrees $ S.sivTokens "foo{ba_}[0-9]baz_")
--   - SivText "foo"
--   > SivRepeat 0 Just (SivOpen (SivRepeat 0), SivClose (SivRepeat 0))
--     - SivText "ba"
--     - SivKey SivAnyChar
--   > SivOption Just (SivOpen SivOption, SivClose SivOption)
--     - SivText "0"
--     - SivKey SivRange
--     - SivText "9"
--   - SivText "baz_"
--
sivTrees :: [S.SivToken t] -> B.Ab [SivTree t]
sivTrees = B.codeTrees bracket B.BracketNone where
    bracket (S.SivOpen  b) = B.BracketOpen  b
    bracket (S.SivClose b) = B.BracketClose b
    bracket _              = B.BracketNone

-- | Sieve expression.
type SivExpr = S.CharExpr

-- | Convert to sieve expression.
class ToSivExpr a where
    toSivExpr :: a -> B.Ab SivExpr

-- | Convert to sieve expression with replacement.
toSivExprOr :: (ToSivExpr a) => SivExpr -> a -> SivExpr
toSivExprOr e0 a = case toSivExpr a of
                     Right e -> e
                     Left _  -> e0

instance ToSivExpr SivExpr where
    toSivExpr = Right

instance ToSivExpr [S.SivToken String] where
    toSivExpr = sivExpr O.#. sivTrees

instance ToSivExpr String where
    toSivExpr = toSivExpr . S.sivTokens

-- | Convet sieve tree to sieve expression.
sivExpr :: [SivTree String] -> B.Ab SivExpr
sivExpr = top where
    top ts = do e <- seq ts
                Right $ S.seq [e, S.end]

    seq ts = do es <- expr ts
                Right $ S.seq es

    expr (Lt a : Lr : Lt b : ts)
                             = case (O.tCut2 a, O.tCut2 b) of 
                                 (Just (a0, Nothing), Just (b0, Nothing))
                                   -> S.to a0 b0 <:> ts
                                 _ -> Msg.adlib "Malformed range"
    expr (Lt t : ts)         = S.equal t     <:> ts
    expr (Lat : ts)          = S.many S.any  <:> ts
    expr (Lac : ts)          = S.any         <:> ts

    expr (Bg [Lat] : ts)     = repAny S.min 0 ts
    expr (Bg xs : ts)        = do e <- seq xs
                                  e <:> ts

    expr (Br n [Lat] : ts)   = repAny S.min n ts
    expr (Br n [Lac] : ts)   = repAny S.min n ts
    expr (Br n xs : ts)      = do e <- seq xs
                                  S.min n e <:> ts

    expr (Bo [Lat] : ts)     = repAny S.max 1 ts
    expr (Bo [Lac] : ts)     = repAny S.max 1 ts
    expr (Bo xs : ts)        = do e <- seq xs
                                  S.maybe e <:> ts

    expr []                  = Right []
    expr _                   = Msg.adlib "Unknown sieve"

    e <:> ts        = do es <- expr ts
                         Right $ e : es

    repAny f n []   = Right [f n S.any]
    repAny f n ts   = do es <- expr ts
                         Right $ f n (S.anyNot $ S.seq es) : es

pattern L tok     <- B.TreeL tok
pattern B b xs    <- B.TreeB b _ xs

pattern Lt t      <- L (S.SivText t)
pattern Lr        <- L (S.SivKey S.SivRange)
pattern Lat       <- L (S.SivKey S.SivAnyText)
pattern Lac       <- L (S.SivKey S.SivAnyChar)

pattern Bg   xs   <- B S.SivGroup xs
pattern Br n xs   <- B (S.SivRepeat n) xs
pattern Bo   xs   <- B S.SivOption xs

-- | Test sieve pattern matches text.
--
--   >>> sivMatch "foo(*).k" O.<#> ["foo.k", "foo.hs", "foobar.k", "bar.k"]
--   Right [True, True, False]
--
sivMatch :: (ToSivExpr siv) => siv -> String -> B.Ab Bool
sivMatch siv t = do e <- toSivExpr siv
                    Right $ sivMatchExpr e t

-- | Test sieve pattern matches text.
sivMatchExpr :: SivExpr -> String -> Bool
sivMatchExpr e t = case S.matchExpr e t of
                     Just _  -> True
                     Nothing -> False


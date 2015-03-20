{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Element
  ( resourceElem
    -- $Pattern
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

infixr 0 -:-
(-:-) :: a -> b -> (a, b)
(-:-) = (,)

-- | Retrive constituents of sections.
resourceElem :: (C.CContent c) => C.Resource c -> [B.Judge c]
resourceElem res = map art js where
    art  = B.judgeCons ("point" -:- path)
    path = C.pText $ B.codeNameText $ B.codeName $ head $ C.resIncluded res
    ass  = map B.shortBody $ C.resAssert res
    js   = concat [ elemJudge       $ C.resJudge res
                  , elemAssert      $ ass
                  , elemNamedRelmap $ concatMap C.assLinks ass ]

elemJudge :: (C.CContent c) => B.Map [B.Judge c]
elemJudge = B.unique . concatMap f where
    f j = map (term $ B.judgePat j) (B.judgeTerms j)
    term p (n, c) = B.affirm "KOSHU-JUDGE-TERM"
                    [ "pat"     -:- C.pText p
                    , "name"    -:- C.pText n
                    , "content" -:- c ]

elemAssert :: (C.CContent c) => [C.Assert c] -> [B.Judge c]
elemAssert = B.unique . concatMap f where
    f (C.Assert _ _ _ _ _ Nothing _) = B.bug "elemAssert"
    f (C.Assert _ t pat _ _ (Just r) _) =
        let p     = "name" -:- C.pText pat
            ass   = B.affirm (quality t) [p]
            rmaps = B.judgeCons p `map` elemRelmap r
        in ass : rmaps

    quality C.AssertAffirm       = "KOSHU-AFFIRM"
    quality C.AssertDeny         = "KOSHU-DENY"
    quality C.AssertMultiDeny    = "KOSHU-MULTI-DENY"
    quality C.AssertChange       = "KOSHU-CHANGE"
    quality C.AssertMultiChange  = "KOSHU-MULTI-CHANGE"
    quality C.AssertViolate      = "KOSHU-VIOLATE"

elemNamedRelmap :: (C.CContent c) => C.RelmapLinkTable c -> [B.Judge c]
elemNamedRelmap = B.unique . concatMap f where
    f (lx, relmap) = B.judgeCons ("name" -:- C.pText $ C.lexName lx)
                        `map` elemRelmap relmap

elemRelmap :: (C.CContent c) => C.Relmap c -> [B.Judge c]
elemRelmap relmap = name : f relmap where
    name          = B.affirm "KOSHU-RELMAP-NAME" []
    op lx         = let p = opType lx
                        n = C.lexName lx
                    in B.affirm p [ "rop" -:- C.pText n ]
    opType lx     = case C.lexType lx of
                      C.LexmapBase    -> "KOSHU-RELMAP-BASE"
                      C.LexmapDerived -> "KOSHU-RELMAP-DERIV"
                      C.LexmapLocal   -> "KOSHU-RELMAP-LOCAL"
    src p xs      = B.affirm "KOSHU-RELMAP-SOURCE"
                      [ "pat"   -:- C.pText p
                      , "terms" -:- C.pTermSet xs ]

    f (C.RelmapAppend r1 r2)     = f r1 ++ f r2
    f (C.RelmapCalc   lx _ rs)   = op lx : concatMap f rs
    f (C.RelmapCopy   lx _ r1)   = op lx : f r1
    f (C.RelmapNest   lx r1)     = op lx : f r1
    f (C.RelmapSource lx p xs)   = [op lx, src p xs]
    f (C.RelmapLink   lx)        = [op lx]
    f (C.RelmapHook   lx _)      = [op lx]
    f (C.RelmapConst  lx _)      = [op lx]


-- ------------------------------------------------------------------
-- $Pattern
--
-- 'resourceElem' may output judges of the following patterns.
--
-- [@KOSHU-AFFIRM@]
--   There is affirmed relmap for pattern @\/pat@
--   in the resource @\/res@.
--
-- [@KOSHU-DENY@]
--   There is denied relmap for pattern @\/pat@
--   in the resource @\/res@.
--
-- [@KOSHU-JUDGE-TERM@]
--   In the resource @\/res@,
--   there are judges of pattern @\/pat@
--   with term named @\/name@ having @\/content@.
--
-- [@KOSHU-RELMAP-NAME@]
--   There is a relmap named @\/name@ in the resource @\/res@.
--
-- [@KOSHU-RELMAP-REF@]
--   Relmap @\/name@ in the resource @\/res@
--   refers the other relmap @\/ref@.
--
-- [@KOSHU-RELMAP-ROP@]
--   Relmap @\/name@ in the resource @\/res@
--   uses the relmap operator @\/rop@.
--
-- [@KOSHU-RELMAP-SOURCE@]
--   Judges of pattern @\/pat@ with terms @\/terms@
--   are read from the resource @\/res@.
--


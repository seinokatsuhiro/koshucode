{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Element
  ( resourceElem
    -- $Pattern
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Data as D
import qualified Koshucode.Baala.Core as C

infixr 0 -:-
(-:-) :: a -> b -> (a, b)
(-:-) = (,)

-- | Retrive constituents of sections.
resourceElem :: (D.CContent c) => C.Resource c -> [D.Judge c]
resourceElem res = map art js where
    art  = D.judgeCons ("point" -:- path)
    path = D.pText $ B.ioPointText $ B.codeName $ head $ C.resIncluded res
    ass  = map D.shortBody $ C.resAssert res
    js   = concat [ elemJudge       $ C.resJudge res
                  , elemAssert      $ ass
                  , elemNamedRelmap $ concatMap C.assLinks ass ]

elemJudge :: (D.CContent c) => B.Map [D.Judge c]
elemJudge = B.unique . concatMap f where
    f j = map (term $ D.judgePat j) (D.judgeTerms j)
    term p (n, c) = D.affirm "KOSHU-JUDGE-TERM"
                    [ "pat"     -:- D.pText p
                    , "name"    -:- D.pText n
                    , "content" -:- c ]

elemAssert :: (D.CContent c) => [C.Assert c] -> [D.Judge c]
elemAssert = B.unique . concatMap f where
    f (C.Assert _ _ _ _ _ Nothing _) = B.bug "elemAssert"
    f (C.Assert _ t pat _ _ (Just r) _) =
        let p     = "name" -:- D.pText pat
            ass   = D.affirm (quality t) [p]
            rmaps = D.judgeCons p `map` elemRelmap r
        in ass : rmaps

    quality D.AssertAffirm       = "KOSHU-AFFIRM"
    quality D.AssertDeny         = "KOSHU-DENY"
    quality D.AssertMultiDeny    = "KOSHU-MULTI-DENY"
    quality D.AssertChange       = "KOSHU-CHANGE"
    quality D.AssertMultiChange  = "KOSHU-MULTI-CHANGE"
    quality D.AssertViolate      = "KOSHU-VIOLATE"

elemNamedRelmap :: (D.CContent c) => C.RelmapLinkTable c -> [D.Judge c]
elemNamedRelmap = B.unique . concatMap f where
    f (lx, relmap) = D.judgeCons ("name" -:- D.pText $ C.lexName lx)
                        `map` elemRelmap relmap

elemRelmap :: (D.CContent c) => C.Relmap c -> [D.Judge c]
elemRelmap relmap = name : f relmap where
    name          = D.affirm "KOSHU-RELMAP-NAME" []
    op lx         = let p = opType lx
                        n = C.lexName lx
                    in D.affirm p [ "rop" -:- D.pText n ]
    opType lx     = case C.lexType lx of
                      C.LexmapBase    -> "KOSHU-RELMAP-BASE"
                      C.LexmapDerived -> "KOSHU-RELMAP-DERIV"
                      C.LexmapLocal   -> "KOSHU-RELMAP-LOCAL"
    src p xs      = D.affirm "KOSHU-RELMAP-SOURCE"
                      [ "pat"   -:- D.pText p
                      , "terms" -:- D.pTermSet xs ]

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


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Elements of data resource.

module Koshucode.Baala.Toolkit.Library.Element
  ( resourceElem
  ) where

import qualified Koshucode.Baala.Overture  as O
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S
import qualified Koshucode.Baala.Type      as T
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C

infixr 0 //
(//) :: n -> c -> (n, c)
(//) n c = (n, c)

-- | Retrive elements of data resource.
--   This function may output judges of the following classes.
--
--   [@KOSHU-AFFIRM@]
--     There is affirmed relmap for class @\/pat@
--     in the resource @\/res@.
--
--   [@KOSHU-DENY@]
--     There is denied relmap for class @\/pat@
--     in the resource @\/res@.
--
--   [@KOSHU-JUDGE-TERM@]
--     In the resource @\/res@,
--     there are judges of class @\/pat@
--     with term named @\/name@ having @\/content@.
--
--   [@KOSHU-RELMAP-NAME@]
--     There is a relmap named @\/name@ in the resource @\/res@.
--
--   [@KOSHU-RELMAP-REF@]
--     Relmap @\/name@ in the resource @\/res@
--     refers the other relmap @\/ref@.
--
--   [@KOSHU-RELMAP-ROP@]
--     Relmap @\/name@ in the resource @\/res@
--     uses the relmap operator @\/rop@.
--
--   [@KOSHU-RELMAP-SOURCE@]
--     Judges of class @\/pat@ with terms @\/terms@
--     are read from the resource @\/res@.
--
resourceElem :: (D.CContent c) => C.Resource c -> [T.Judge c]
resourceElem res = map art js where
    art  = T.judgeAdd ("point" // path)
    path = D.pText $ B.ioPointText $ B.nioPoint $ head $ C.resIncluded res
    ass  = map S.shortBody $ C.resAssert res
    js   = concat [ elemJudge       $ C.resJudge res
                  , elemAssert      $ ass
                  , elemNamedRelmap $ concatMap C.assLinks ass ]

elemJudge :: (D.CContent c) => O.Map [T.Judge c]
elemJudge = B.unique . concatMap f where
    f j = map (term $ T.getClass j) (T.getTerms j)
    term p (n, c) = T.affirm "KOSHU-JUDGE-TERM"
                    [ "pat"     // D.pText p
                    , "name"    // D.pText $ S.termNameContent n
                    , "content" // c ]

elemAssert :: (D.CContent c) => [C.Assert c] -> [T.Judge c]
elemAssert = B.unique . concatMap f where
    f (C.Assert _ _ _ _ _ Nothing _) = B.bug "elemAssert"
    f (C.Assert _ t pat _ _ (Just r) _) =
        let p     = "name" // D.pText pat
            ass   = T.affirm (quality t) [p]
            rmaps = T.judgeAdd p `map` elemRelmap r
        in ass : rmaps

    quality T.AssertAffirm       = "KOSHU-AFFIRM"
    quality T.AssertDeny         = "KOSHU-DENY"
    quality T.AssertMultiDeny    = "KOSHU-MULTI-DENY"
    quality T.AssertChange       = "KOSHU-CHANGE"
    quality T.AssertMultiChange  = "KOSHU-MULTI-CHANGE"
    quality T.AssertViolate      = "KOSHU-VIOLATE"

elemNamedRelmap :: (D.CContent c) => C.RelmapLinkTable c -> [T.Judge c]
elemNamedRelmap = B.unique . concatMap f where
    f (lx, relmap) = T.judgeAdd ("name" // D.pText $ C.lexName lx)
                        `map` elemRelmap relmap

elemRelmap :: (D.CContent c) => C.Relmap c -> [T.Judge c]
elemRelmap relmap = name : f relmap where
    name          = T.affirm "KOSHU-RELMAP-NAME" []
    op lx         = let p = opType lx
                        n = C.lexName lx
                    in T.affirm p [ "rop" // D.pText n ]
    opType lx     = case C.lexType lx of
                      C.LexmapBase    -> "KOSHU-RELMAP-BASE"
                      C.LexmapDerived -> "KOSHU-RELMAP-DERIV"
                      C.LexmapLocal   -> "KOSHU-RELMAP-LOCAL"
    src p xs      = T.affirm "KOSHU-RELMAP-SOURCE"
                      [ "pat"   // D.pText p
                      , "terms" // D.pTermSet xs ]

    f (C.RelmapAppend r1 r2)     = f r1 ++ f r2
    f (C.RelmapCalc   lx _ rs)   = op lx : concatMap f rs
    f (C.RelmapCopy   lx _ r1)   = op lx : f r1
    f (C.RelmapNest   lx r1)     = op lx : f r1
    f (C.RelmapSource lx p xs)   = [op lx, src p xs]
    f (C.RelmapLink   lx)        = [op lx]
    f (C.RelmapHook   lx _)      = [op lx]
    f (C.RelmapConst  lx _)      = [op lx]


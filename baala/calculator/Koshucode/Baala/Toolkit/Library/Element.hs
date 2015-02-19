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
    art = B.judgeCons ("/article" -:- C.pText $ B.codeNameText $ B.codeName $ head $ C.resIncluded res)
    js  = concat [ elemJudge       $ C.resJudge res
                 , elemAssert      $ map B.shortBody $ C.resAssert res
                 , elemNamedRelmap [] ]

elemJudge :: (C.CContent c) => B.Map [B.Judge c]
elemJudge = B.unique . concatMap f where
    f j = map (term $ B.judgePat j) (B.judgeTerms j)
    term p (n, c) = B.affirm "KOSHU-JUDGE-TERM"
                    [ "pat"     -:- C.pText p
                    , "name"    -:- C.pText n
                    , "content" -:- c ]

elemAssert :: (C.CContent c) => [C.Assert c] -> [B.Judge c]
elemAssert = B.unique . concatMap f where
    f (C.Assert _ _ _ _ _ _ Nothing _) = B.bug "elemAssert"
    f (C.Assert _ t pat _ _ _ (Just r) _) =
        B.affirm (quality t) [ "/pat" -:- C.pText pat ]
             : elemRelmap r

    quality C.AssertAffirm       = "KOSHU-AFFIRM"
    quality C.AssertDeny         = "KOSHU-DENY"
    quality C.AssertMultiDeny    = "KOSHU-MULTI-DENY"
    quality C.AssertChange       = "KOSHU-CHANGE"
    quality C.AssertMultiChange  = "KOSHU-MULTI-CHANGE"
    quality C.AssertViolate      = "KOSHU-VIOLATE"

elemNamedRelmap :: (C.CContent c) => C.RelmapLinkTable c -> [B.Judge c]
elemNamedRelmap = B.unique . concatMap f where
    f (lx, relmap) = map (B.judgeCons ("/name" -:- C.pText $ C.lexRopName lx))
                            $ elemRelmap relmap

elemRelmap :: (C.CContent c) => C.Relmap c -> [B.Judge c]
elemRelmap relmap = name : f relmap where
    name     = B.affirm "KOSHU-RELMAP-NAME" []
    ref n    = B.affirm "KOSHU-RELMAP-REF"  [ "/ref" -:- C.pText n ]
    rop n    = B.affirm "KOSHU-RELMAP-ROP"  [ "/rop" -:- C.pText n ]
    src p xs = B.affirm "KOSHU-RELMAP-SOURCE"
               [ "/pat"   -:- C.pText p
               , "/terms" -:- C.pTextSet xs ]

    f (C.RelmapAppend r1 r2)     = f r1 ++ f r2
    f (C.RelmapSource _ p xs)    = [ rop "source", src p xs ]
    f (C.RelmapLink   lx)        = [ ref $ C.lexRopName lx ]
    f (C.RelmapCalc   _ _ rs)    = rop (B.name relmap) : concatMap f rs
    f (C.RelmapHook   _ _)       = [ rop (B.name relmap) ]
    f (C.RelmapConst  _ _)       = [ rop (B.name relmap) ]
    f (C.RelmapCopy   _ _ r1)    = f r1
    f (C.RelmapNest   _ r1)      = f r1


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


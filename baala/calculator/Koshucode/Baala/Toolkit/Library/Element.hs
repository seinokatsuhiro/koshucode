{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Element
( sectionElem
  -- $Pattern
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

infixr 0 -:-
(-:-) :: a -> b -> (a, b)
(-:-) = (,)

-- | Retrive constituents of sections.
sectionElem :: (C.CContent c) => C.Section c -> [B.Judge c]
sectionElem sec = map res js where
    res = judgeCons ("/res" -:- C.pText $ B.resourceName $ C.sectionResource sec)
    js  = concat [ elemJudge       $ C.sectionJudge  sec
                 , elemAssert      $ concatMap B.shortBody $ C.sectionAssert sec
                 , elemNamedRelmap $ C.sectionRelmap sec ]

judgeCons :: B.Named c -> B.Map (B.Judge c)
judgeCons x (B.Judge q p xs) = B.Judge q p $ x : xs

elemJudge :: (C.CContent c) => B.Map [B.Judge c]
elemJudge = B.unique . concatMap f where
    f (B.Judge _ p xs) = map (term p) xs
    term p (n, c) = B.affirm "KOSHU-JUDGE-TERM"
                    [ "/pat"     -:- C.pText p
                    , "/name"    -:- C.pText n
                    , "/content" -:- c ]

elemAssert :: (C.CContent c) => [C.Assert c] -> [B.Judge c]
elemAssert = B.unique . concatMap f where
    f (C.Assert t pat _ r _) =
        B.affirm (name $ C.assertQuality t) [ "/pat" -:- C.pText pat ]
             : elemRelmap r

    name True  = "KOSHU-AFFIRM"
    name False = "KOSHU-DENY"

elemNamedRelmap :: (C.CContent c) => [C.RelmapAssoc c] -> [B.Judge c]
elemNamedRelmap = B.unique . concatMap f where
    f ((name, _), relmap) = map (judgeCons ("/name" -:- C.pText name))
                            $ elemRelmap relmap

elemRelmap :: (C.CContent c) => C.Relmap c -> [B.Judge c]
elemRelmap relmap = name : f relmap where
    name     = B.affirm "KOSHU-RELMAP-NAME" []
    ref n    = B.affirm "KOSHU-RELMAP-REF"  [ "/ref" -:- C.pText n ]
    rop n    = B.affirm "KOSHU-RELMAP-ROP"  [ "/rop" -:- C.pText n ]
    src p xs = B.affirm "KOSHU-RELMAP-SOURCE"
               [ "/pat"   -:- C.pText p
               , "/terms" -:- C.pTextSet xs ]

    f (C.RelmapAppend r1 r2)   =  f r1 ++ f r2
    f (C.RelmapSource _ p xs)  =  [ rop "source", src p xs ]
    f (C.RelmapLink   _ n _)   =  [ ref n ]
    f (C.RelmapCalc   _ _ rs)  =  rop (B.name relmap) : concatMap f rs
    f (C.RelmapGlobal _ _)     =  [ rop (B.name relmap) ]
    f (C.RelmapConst  _ _)     =  [ rop (B.name relmap) ]
    f (C.RelmapCopy   _ _ r1)  =  f r1
    f (C.RelmapWith   _ _ r1)  =  f r1


-- ------------------------------------------------------------------
-- $Pattern
--
-- 'sectionElem' may output judges of the following patterns.
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


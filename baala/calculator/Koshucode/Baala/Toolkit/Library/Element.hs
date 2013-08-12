{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Element
( sectionElem
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C

sectionElem :: (C.CContent c) => C.Section c -> [B.Judge c]
sectionElem sec = map res js where
    res = judgeCons ("/res", C.putText $ C.sectionResource sec)
    js  = concat [ judgeElem       $ C.sectionJudge  sec
                 , affirmElem      $ C.sectionAssert sec
                 , namedRelmapElem $ C.sectionRelmap sec ]

judgeElem :: (C.CContent c) => B.Map [B.Judge c]
judgeElem = B.unique . concatMap f where
    f (B.Judge _ p xs) = map (term p) xs

    term p (n, c) =
        affirm "KOSHU-JUDGE-TERM"
             [ ("/pat"     , C.putText p)
             , ("/name"    , C.putText n)
             , ("/content" , c) ]

affirm :: B.JudgePattern -> [B.Named c] -> B.Judge c
affirm = B.Judge True

affirmElem :: (C.CContent c) => [C.Assert c] -> [B.Judge c]
affirmElem = B.unique . concatMap f where
    f (C.Assert q p r) =
        affirm (pat q) [ ("/pat", C.putText p) ]
             : relmapElem r

    pat True  = "KOSHU-AFFIRM"
    pat False = "KOSHU-DENY"

namedRelmapElem :: (C.CContent c) => [B.Named (C.Relmap c)] -> [B.Judge c]
namedRelmapElem = B.unique . concatMap f where
    f (name, relmap) = map (judgeCons ("/name", C.putText name))
                              $ relmapElem relmap

relmapElem :: (C.CContent c) => C.Relmap c -> [B.Judge c]
relmapElem relmap = aName : f relmap where
    aName     = affirm "KOSHU-RELMAP-NAME" []
    aRef n    = affirm "KOSHU-RELMAP-REF"  [ ("/ref"  , C.putText n) ]
    aRop n    = affirm "KOSHU-RELMAP-ROP"  [ ("/rop"  , C.putText n) ]
    aSrc p xs = affirm "KOSHU-RELMAP-SOURCE"
                [ ("/pat"  , C.putText p)
                , ("/terms", C.putList $ map C.putText xs) ]

    f (C.RelmapAppend r1 r2)     =  f r1 ++ f r2
    f (C.RelmapAlias  _ r1)      =  f r1
    f (C.RelmapSource _ p xs)    =  [ aSrc p xs ]
    f (C.RelmapName   _ n)       =  [ aRef n ]
    f (C.RelmapCalc   _ n _ rs)  =  aRop n : concatMap f rs
    f (C.RelmapConst  _ n _)     =  [ aRop n ]

judgeCons :: B.Named c -> B.Map (B.Judge c)
judgeCons x (B.Judge q p xs) = B.Judge q p $ x : xs


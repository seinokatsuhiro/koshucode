{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators on lists.

module Koshucode.Baala.Cop.List
  ( copsList
    -- $Operators
  ) where

import qualified Data.List                       as List
import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Cop.Coxhand     as H
import qualified Koshucode.Baala.Cop.Replace     as Cop
import qualified Koshucode.Baala.Cop.Message     as Msg



-- ----------------------
-- $Operators
--
--  [@list@]       Construct list.
--
--  [@total@]      Calculate total amount of elements in list,
--                 i.e., summation.
--
--  [@length@]     Number of elements.
--
--  [@min@]        Minimal element.
--
--  [@max@]        Maximal element.
--
--  [@++@]         Append lists.
--
--  [@intersect@]  Common elements.
--
--  [@minus@]      Remove elements.
--

-- | Content operators on lists.
copsList :: (D.CContent c) => [D.Cop c]
copsList =
    [ D.CopCalc  (D.copInfix "++")              copAppend
    , D.CopCalc  (D.copInfix "in")              copFunIn
    , D.CopCalc  (D.copInfix "intersect")       copIntersect

    , D.CopCox   (D.copPrefix "in")             copCoxIn

    , D.CopCalc  (D.copNormal "++")             copAppend
    , D.CopCalc  (D.copNormal "drop")           copDrop
    , D.CopCalc  (D.copNormal "drop-tail")      copDropTail
    , D.CopCalc  (D.copNormal "drop-take")      copDropTake
    , D.CopCalc  (D.copNormal "drop-take-tail") copDropTakeTail
    , D.CopCalc  (D.copNormal "intersect")      copIntersect
    , D.CopCalc  (D.copNormal "length")         copLength
    , D.CopCalc  (D.copNormal "list")           copList
    , D.CopCalc  (D.copNormal "max")            copMax
    , D.CopCalc  (D.copNormal "min")            copMin
    , D.CopCalc  (D.copNormal "minus")          copMinus
    , D.CopCalc  (D.copNormal "push")           copPush
    , D.CopCalc  (D.copNormal "push-tail")      copPushTail
    , D.CopCalc  (D.copNormal "replace-all")    copReplaceAll
    , D.CopCalc  (D.copNormal "replace-begin")  copReplaceBegin
    , D.CopCalc  (D.copNormal "replace-end")    copReplaceEnd
    , D.CopCalc  (D.copNormal "replace-first")  copReplaceFirst
    , D.CopCalc  (D.copNormal "replace-last")   copReplaceLast
    , D.CopCalc  (D.copNormal "reverse")        copReverse
    , D.CopCalc  (D.copNormal "sort")           copSort
    , D.CopCalc  (D.copNormal "take")           copTake
    , D.CopCalc  (D.copNormal "take-tail")      copTakeTail
    , D.CopCalc  (D.copNormal "take-odd")     $ copTakeOddEven (B.takeOdd,  B.takeOdd)
    , D.CopCalc  (D.copNormal "take-even")    $ copTakeOddEven (B.takeEven, B.takeEven)
    , D.CopCalc  (D.copNormal "term-set")       copTermSet
    , D.CopCalc  (D.copNormal "total")          copTotal
    ]

copList :: (D.CList c) => D.CopCalc c
copList argC = do arg <- sequence argC
                  D.putList arg


-- ----------------------  aggregation

copTotal :: (D.CContent c) => D.CopCalc c
copTotal = op where
    op [Right c] | D.isList c = D.putDec =<< D.decimalSum (map D.gDec $ D.gList c)
    op xs = typeUnmatch xs

copMin :: (D.CContent c) => D.CopCalc c
copMin = op where
    op [Right c] | D.isList c = Right $ D.contMinimum $ D.gList c
    op xs = typeUnmatch xs

copMax :: (D.CContent c) => D.CopCalc c
copMax = op where
    op [Right c] | D.isList c = Right $ D.contMaximum $ D.gList c
    op xs = typeUnmatch xs

copLength :: (D.CContent c) => D.CopCalc c
copLength = op where
    op [Right c] | D.isList c  = Right . D.pInt $ length (D.gList c)
                 | D.isText c  = Right . D.pInt $ length (D.gText c)
                 | D.isRel c   = Right . D.pInt $ length (D.relBody $ D.gRel c)
    op xs = typeUnmatch xs

typeUnmatch :: D.CTypeOf c => [B.Ab c] -> B.Ab c
typeUnmatch _ = Msg.unmatchType ""



-- ----------------------  set-like operation

copAppend :: (D.CContent c) => D.CopCalc c
copAppend [] = Right D.empty
copAppend xs@(x : _) = op x where
    op (Right c) | D.isText c = D.putText . concat =<< mapM D.getText xs
                 | D.isSet  c = D.putSet  . concat =<< mapM D.getSet  xs
                 | D.isList c = D.putList . concat =<< mapM D.getList xs
    op _ = typeUnmatch xs

copIntersect :: (D.CContent c) => D.CopCalc c
copIntersect [] = Right D.empty
copIntersect xs@(x : _) = op x where
    op (Right c) | D.isSet  c = D.putSet  . intersectLists =<< mapM D.getSet  xs
                 | D.isList c = D.putList . intersectLists =<< mapM D.getList xs
    op _ = typeUnmatch xs

copMinus :: (D.CContent c) => D.CopCalc c
copMinus = op where
    op [Right a,  Right b]
        | D.isSet  a && D.isSet  b = D.putSet  (D.gSet  a List.\\ D.gSet  b)
        | D.isList a && D.isList b = D.putList (D.gList a List.\\ D.gList b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ List.intersect a b : xs


-- ----------------------  arrangement

copSort :: (D.CContent c) => D.CopCalc c
copSort = op where
    op [Right c] | D.isList c  = D.putList $ B.sort $ D.gList c
                 | D.isSet  c  = D.putSet  $ B.sort $ D.gSet  c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copReverse :: (D.CContent c) => D.CopCalc c
copReverse = op where
    op [Right c] | D.isText c  = D.putText $ reverse $ D.gText c
                 | D.isList c  = D.putList $ reverse $ D.gList c
    op xs = typeUnmatch xs


-- ----------------------  take-odd & take-even

copTakeOddEven :: (D.CContent c) => (String -> String, [c] -> [c]) -> D.CopCalc c
copTakeOddEven fg arg = D.getRightArg1 arg >>= collMap fg

collMap :: (D.CContent c) => (String -> String, [c] -> [c]) -> c -> B.Ab c
collMap (f, g) xs
    | D.isText  xs  = gpMap D.gpText    f xs
    | D.isList  xs  = gpMap D.gpList    g xs
    | D.isSet   xs  = gpMap D.gpSetSort g xs
    | D.isEmpty xs  = Right D.empty
    | otherwise     = typeUnmatch [Right xs]

gpMap :: D.CGetPut [a] c -> O.Map [a] -> c -> B.Ab c
gpMap (get, put) f = Right . put . f . get


-- ----------------------  take & drop

type TakeDrop a   = Int -> [a] -> [a]
type TakeDrop2 a  = (TakeDrop Char, TakeDrop a)

copTake :: (D.CContent c) => D.CopCalc c
copTake = copTakeOrDrop (take, take)

copDrop :: (D.CContent c) => D.CopCalc c
copDrop = copTakeOrDrop (drop, drop)

copTakeTail :: (D.CContent c) => D.CopCalc c
copTakeTail = copTakeOrDrop (takeTail, takeTail)

copDropTail :: (D.CContent c) => D.CopCalc c
copDropTail = copTakeOrDrop (dropTail, dropTail)

takeTail :: Int -> [a] -> [a]
takeTail = B.reverseMap . take

dropTail :: Int -> [a] -> [a]
dropTail = B.reverseMap . drop

copTakeOrDrop :: (D.CContent c) => TakeDrop2 c -> D.CopCalc c
copTakeOrDrop (f, g) arg =
    do (n', xs') <- D.getRightArg2 arg
       if D.isDec n'
          then collMap (f $ int n', g $ int n') xs'
          else typeUnmatch arg
    where
      int = fromInteger . D.decimalNum . D.gDec


-- ----------------------  drop-take

type DropTake a   = Int -> Int -> [a] -> [a]
type DropTake2 a  = (DropTake Char, DropTake a)

copDropTake :: (D.CContent c) => D.CopCalc c
copDropTake = dropTakeCop (dropTake, dropTake)

copDropTakeTail :: (D.CContent c) => D.CopCalc c
copDropTakeTail = dropTakeCop (dropTake', dropTake') where
    dropTake' d t = B.reverseMap $ dropTake d t

dropTake :: DropTake a
dropTake d t = take t . drop d

dropTakeCop :: (D.CContent c) => DropTake2 c -> D.CopCalc c
dropTakeCop fg arg =
    do (d', t', xs') <- D.getRightArg3 arg
       if D.isDec d' && D.isDec t'
          then dropTakeDispatch fg arg (int d') (int t') xs'
          else typeUnmatch arg
    where
      int = fromInteger . D.decimalNum . D.gDec

dropTakeDispatch :: (D.CContent c) => DropTake2 c -> [B.Ab c] -> Int -> Int -> c -> B.Ab c
dropTakeDispatch (f, g) arg d t xs'
    | D.isText  xs'  = gpMap D.gpText    (f d t) xs'
    | D.isList  xs'  = gpMap D.gpList    (g d t) xs'
    | D.isSet   xs'  = gpMap D.gpSetSort (g d t) xs'
    | D.isEmpty xs'  = Right D.empty
    | otherwise      = typeUnmatch arg


-- ----------------------  push

copPush :: (D.CContent c) => D.CopCalc c
copPush = push (:) B.<=< D.getRightArg2

copPushTail :: (D.CContent c) => D.CopCalc c
copPushTail = push f B.<=< D.getRightArg2 where
    f c = B.reverseMap (c:)

push :: (D.CContent c) => (c -> [c] -> [c]) -> (c, c) -> B.Ab c
push f (c, cs)
    | D.isList cs = D.putList $ c `f` D.gList cs
    | D.isSet  cs = D.putSet  $ c `f` D.gSet  cs
    | otherwise   = Msg.reqCollection


-- --------------------------------------------  in

-- syntax
copCoxIn :: D.CopCox c
copCoxIn [xs]    = Right $ H.f1 $ H.bin "in" H.b1 xs
copCoxIn [x, xs] = Right $ H.f1 $ H.bin "in" x    xs
copCoxIn _       = Msg.adlib "require operand"

-- function
copFunIn :: (D.CContent c) => D.CopCalc c
copFunIn = f B.<=< D.getRightArg2 where
    f (c, cs)
        | D.isSet  cs  = D.putBool $ c `elem` D.gSet  cs
        | D.isList cs  = D.putBool $ c `elem` D.gList cs
        | otherwise    = Msg.reqCollection


-- ----------------------  replace

copReplaceAll :: (D.CContent c) => D.CopCalc c
copReplaceAll = copReplace Cop.replaceAll

copReplaceFirst :: (D.CContent c) => D.CopCalc c
copReplaceFirst = copReplace Cop.replaceFirst

copReplaceLast :: (D.CContent c) => D.CopCalc c
copReplaceLast = copReplace Cop.replaceLast

copReplaceBegin :: (D.CContent c) => D.CopCalc c
copReplaceBegin = copReplace Cop.replaceBegin

copReplaceEnd :: (D.CContent c) => D.CopCalc c
copReplaceEnd = copReplace Cop.replaceEnd

copReplace :: (D.CContent c) => Cop.Replace Char -> D.CopCalc c
copReplace rep = op where
    op [Right from, Right to, Right text]
        | D.isText from && D.isText to && D.isText text =
            D.contApTextToText (D.gText from `rep` D.gText to) text
    op xs = typeUnmatch xs


-- ----------------------  term-set

-- term-set {| aaa /x bbb /y ccc |} => { '/x | '/y }
copTermSet :: (D.CContent c) => D.CopCalc c
copTermSet [Right c] | D.isInterp c = D.putSet ts where
                     ts = map D.pTerm $ D.interpTerms $ D.gInterp c
copTermSet xs = typeUnmatch xs


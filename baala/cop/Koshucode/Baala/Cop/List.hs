{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.List
  ( copsList
    -- $Operators
  ) where

import qualified Data.List                       as List
import qualified Data.Char                       as Char
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Cop.Coxhand     as H
import qualified Koshucode.Baala.Rop.Flat.Message     as Msg



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

copsList :: (D.CContent c) => [D.Cop c]
copsList =
    [ D.CopCalc  (D.copInfix "++")              copAppend
    , D.CopCalc  (D.copInfix "*=")              copEndWithInfix
    , D.CopCalc  (D.copInfix "*=*")             copContainInfix
    , D.CopCalc  (D.copInfix "=*")              copBeginWithInfix
    , D.CopCalc  (D.copInfix "in")              copFunIn
    , D.CopCalc  (D.copInfix "intersect")       copIntersect

    , D.CopCox   (D.copPrefix "in")             copCoxIn

    , D.CopCalc  (D.copNormal "++")             copAppend
    , D.CopCalc  (D.copNormal "base-part")      copBasePart
    , D.CopCalc  (D.copNormal "char")           copChar
    , D.CopCalc  (D.copNormal "char-group")     copCharGroup
    , D.CopCalc  (D.copNormal "char-group-1")   copCharGroup1
    , D.CopCalc  (D.copNormal "code-list")      copCodeList
    , D.CopCalc  (D.copNormal "dir-part")       copDirPart
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
    , D.CopCalc  (D.copNormal "term-set")       copTermSet
    , D.CopCalc  (D.copNormal "total")          copTotal
    , D.CopCalc  (D.copNormal "unwords")        copUnwords
    , D.CopCalc  (D.copNormal "unwords-by")     copUnwordsBy
    , D.CopCalc  (D.copNormal "words")          copWords
    , D.CopCalc  (D.copNormal "words-by")       copWordsBy

    , D.CopCalc  (D.copNormal "match-beg")      copBeginWithNormal
    , D.CopCalc  (D.copNormal "match-end")      copEndWithNormal
    , D.CopCalc  (D.copNormal "match-mid")      copContainNormal
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
    op [Right c] | D.isList c = Right $ minimum (D.gList c)
    op xs = typeUnmatch xs

copMax :: (D.CContent c) => D.CopCalc c
copMax = op where
    op [Right c] | D.isList c = Right $ maximum (D.gList c)
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


-- ----------------------  take & drop

type TakeDrop a   = Int -> [a] -> [a]
type TakeDrop2 a  = (TakeDrop Char, TakeDrop a)

copTake :: (D.CContent c) => D.CopCalc c
copTake = copTakeOrDrop (take, take)

copDrop :: (D.CContent c) => D.CopCalc c
copDrop = copTakeOrDrop (drop, drop)

copTakeTail :: (D.CContent c) => D.CopCalc c
copTakeTail = copTakeOrDrop (take', take') where
    take' n = B.reverseMap $ take n

copDropTail :: (D.CContent c) => D.CopCalc c
copDropTail = copTakeOrDrop (drop', drop') where
    drop' n = B.reverseMap $ drop n

copTakeOrDrop :: (D.CContent c) => TakeDrop2 c -> D.CopCalc c
copTakeOrDrop fg arg =
    do (n', xs') <- D.getRightArg2 arg
       if D.isDec n'
          then takeOrDropDispatch fg arg (int n') xs'
          else typeUnmatch arg
    where
      int = fromInteger . D.decimalNum . D.gDec

takeOrDropDispatch :: (D.CContent c) => TakeDrop2 c -> [B.Ab c] -> Int -> c -> B.Ab c
takeOrDropDispatch (f, g) arg n xs'
    | D.isText  xs'  = gpMap D.gpText    (f n) xs'
    | D.isList  xs'  = gpMap D.gpList    (g n) xs'
    | D.isSet   xs'  = gpMap D.gpSetSort (g n) xs'
    | D.isEmpty xs'  = Right D.empty
    | otherwise      = typeUnmatch arg

gpMap :: D.CGetPut [a] c -> B.Map [a] -> c -> B.Ab c
gpMap (get, put) f = Right . put . f . get


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


-- ----------------------  begin-with / end-with

copBeginWithNormal, copEndWithNormal, copContainNormal :: (D.CContent c) => D.CopCalc c
copBeginWithNormal  = copMatchNormal B.isPrefixOf B.isPrefixOf
copEndWithNormal    = copMatchNormal B.isSuffixOf B.isSuffixOf
copContainNormal    = copMatchNormal B.isInfixOf  B.isInfixOf

copBeginWithInfix, copEndWithInfix, copContainInfix :: (D.CContent c) => D.CopCalc c
copBeginWithInfix  = copMatchInfix B.isPrefixOf B.isPrefixOf
copEndWithInfix    = copMatchInfix B.isSuffixOf B.isSuffixOf
copContainInfix    = copMatchInfix B.isInfixOf  B.isInfixOf

copMatchNormal :: (D.CContent c) => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> D.CopCalc c
copMatchNormal bf1 bf2 arg =
    do (part, whole) <- D.getRightArg2 arg
       copMatch bf1 bf2 (part, whole)

copMatchInfix :: (D.CContent c) => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> D.CopCalc c
copMatchInfix bf1 bf2 arg =
    do (whole, part) <- D.getRightArg2 arg
       copMatch bf1 bf2 (part, whole)

copMatch :: (D.CContent c)
  => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> (c, c) -> B.Ab c
copMatch bf1 bf2 (part, whole)
    | isText2 part whole = match bf1 D.gText
    | isList2 part whole = match bf2 D.gList
    | otherwise          = D.putFalse
    where match bf g = D.putBool $ bf (g part) (g whole)

isList2 :: (D.CList c) => c -> c -> Bool
isList2 x y = D.isList x && D.isList y

isText2 :: (D.CText c) => c -> c -> Bool
isText2 x y = D.isText x && D.isText y


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


-- ----------------------  text

-- char 70 => "F"
copChar :: (D.CContent c) => D.CopCalc c
copChar = op where
    op [Right c] | D.isDec c = D.putText [Char.chr $ fromInteger $ D.decimalNum $ D.gDec c]
    op xs = typeUnmatch xs

-- code-list "abc" => [ 97 : 98 : 99 ]
copCodeList :: (D.CContent c) => D.CopCalc c
copCodeList = op where
    op [Right t] | D.isText t = D.contMapTextToList contOrd t
    op xs = typeUnmatch xs

    contOrd :: (D.CDec c) => Char -> c
    contOrd = D.pInt . Char.ord

-- char-group "a!" => [ 'letter : 'punct ]
copCharGroup :: (D.CContent c) => D.CopCalc c
copCharGroup = op where
    op [Right t] | D.isText t = D.contMapTextToList contGroup t
    op xs = typeUnmatch xs

    contGroup :: (D.CText c) => Char -> c
    contGroup = D.pText . charGroup

-- char-group-1 "a!" => 'letter
copCharGroup1 :: (D.CContent c) => D.CopCalc c
copCharGroup1 = op where
    op [Right t] | D.isText t = case D.gText t of
                                  (c : _) -> D.putText $ charGroup c
                                  _       -> Right D.empty
    op xs = typeUnmatch xs

charGroup :: Char -> String
charGroup = B.generalCategoryName . B.generalCategoryGroup


-- ----------------------  base-part / dir-part

-- dir-part "/" "aa/bb/cc.k" => "aa/bb"
copDirPart :: (D.CContent c) => D.CopCalc c
copDirPart = copDirBasePart fst

-- base-part "/" "aa/bb/cc.k" => "aa/bb"
copBasePart :: (D.CContent c) => D.CopCalc c
copBasePart = copDirBasePart snd

copDirBasePart :: (D.CContent c) => ((String, String) -> String) -> D.CopCalc c
copDirBasePart part = op where
    op [Right sep, Right t]
        | D.isText sep && D.isText t =
            D.contApTextToText (part . dirBasePart (head $ D.gText sep)) t
    op xs = typeUnmatch xs

dirBasePart :: Char -> String -> (String, String)
dirBasePart sep s =
    case reverse $ B.divide sep s of
      (base : dir) -> (B.intercalate [sep] $ reverse dir, base)
      []           -> ("", "")


-- ----------------------  replace

copReplaceAll :: (D.CContent c) => D.CopCalc c
copReplaceAll = copReplace replaceAll

copReplaceFirst :: (D.CContent c) => D.CopCalc c
copReplaceFirst = copReplace replaceFirst

copReplaceLast :: (D.CContent c) => D.CopCalc c
copReplaceLast = copReplace replaceLast

copReplaceBegin :: (D.CContent c) => D.CopCalc c
copReplaceBegin = copReplace replaceBegin

copReplaceEnd :: (D.CContent c) => D.CopCalc c
copReplaceEnd = copReplace replaceEnd

copReplace :: (D.CContent c) => (String -> String -> String -> String) -> D.CopCalc c
copReplace rep = op where
    op [Right from, Right to, Right text]
        | D.isText from && D.isText to && D.isText text =
            D.contApTextToText (D.gText from `rep` D.gText to) text
    op xs = typeUnmatch xs

replaceAll :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceAll from to = loop where
    n           = length from
    loop []     = []
    loop xxs@(x:xs)
        | from `B.isPrefixOf` xxs = to ++ loop (drop n xxs)
        | otherwise               = x : loop xs

replaceFirst :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceFirst from to = loop where
    n           = length from
    loop []     = []
    loop xxs@(x:xs)
        | from `B.isPrefixOf` xxs = to ++ drop n xxs
        | otherwise               = x : loop xs

replaceLast :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceLast from to xs = reverse $ replaceFirst from' to' xs' where
    from' = reverse from
    to'   = reverse to
    xs'   = reverse xs

replaceBegin :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceBegin from to = loop where
    n           = length from
    loop []     = []
    loop xxs
        | from `B.isPrefixOf` xxs = to ++ drop n xxs
        | otherwise               = xxs

replaceEnd :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceEnd from to xs = reverse $ replaceBegin from' to' xs' where
    from' = reverse from
    to'   = reverse to
    xs'   = reverse xs


-- ----------------------  term-set

-- term-set <<< aaa /x bbb /y ccc => { '/x : '/y }
copTermSet :: (D.CContent c) => D.CopCalc c
copTermSet [Right c] | D.isInterp c = D.putSet ts where
                     ts = map D.pTerm $ D.interpTerms $ D.gInterp c
copTermSet xs = typeUnmatch xs


-- ----------------------  words words-by

copWords :: (D.CContent c) => D.CopCalc c
copWords arg =
    do ws <- D.getRightArg1 arg
       if D.isText ws
          then D.putList $ map D.pText $ words $ D.gText ws
          else typeUnmatch arg

copWordsBy :: (D.CContent c) => D.CopCalc c
copWordsBy arg =
    do (sep, ws) <- D.getRightArg2 arg
       case D.isText ws && D.isText sep of
         False -> typeUnmatch arg
         True  -> let isSep = (`elem` D.gText sep)
                  in D.putList $ map D.pText $ wordsBy isSep $ D.gText ws

wordsBy :: B.Pred Char -> String -> [String]
wordsBy p s = case dropWhile p s of
                "" -> []
                s' -> let (w, s'') = break p s'
                      in w : wordsBy p s''


-- ----------------------  unwords unwords-by

copUnwords :: forall c. (D.CContent c) => D.CopCalc c
copUnwords arg =
    do x <- D.getRightArg1 arg
       D.putText $ unwords $ wordList x

copUnwordsBy :: forall c. (D.CContent c) => D.CopCalc c
copUnwordsBy arg =
    do (sep, x) <- D.getRightArg2 arg
       case D.isText sep of
         True  -> let sep' = D.gText sep
                  in D.putText $ B.intercalate sep' $ wordList x
         False -> typeUnmatch arg

wordList :: (D.CContent c) => c -> [String]
wordList c
    | D.isList c  = concatMap wordList $ D.gList c
    | D.isText c  = [D.gText c]
    | D.isDec c   = [D.decimalString $ D.gDec c]
    | otherwise   = []


{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.List
  ( copsList
    -- $Operators
  ) where

import qualified Data.List                          as List
import qualified Data.Char                          as Char
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Op.Cop.Coxhand     as H
import qualified Koshucode.Baala.Op.Message         as Msg



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

copsList :: (C.CContent c) => [C.Cop c]
copsList =
    [ C.CopCalc  (C.copInfix "++")              copAppend
    , C.CopCalc  (C.copInfix "intersect")       copIntersect
    , C.CopCalc  (C.copNormal "++")             copAppend
    , C.CopCalc  (C.copNormal "base-part")      copBasePart
    , C.CopCalc  (C.copNormal "char")           copChar
    , C.CopCalc  (C.copNormal "char-group")     copCharGroup
    , C.CopCalc  (C.copNormal "char-group-1")   copCharGroup1
    , C.CopCalc  (C.copNormal "code-list")      copCodeList
    , C.CopCalc  (C.copNormal "dir-part")       copDirPart
    , C.CopCalc  (C.copNormal "intersect")      copIntersect
    , C.CopCalc  (C.copNormal "length")         copLength
    , C.CopCalc  (C.copNormal "list")           copList
    , C.CopCalc  (C.copInfix  "begin-with")     copBeginWithInfix
    , C.CopCalc  (C.copInfix  "end-with")       copEndWithInfix
    , C.CopCalc  (C.copInfix  "contain")        copContainInfix
    , C.CopCalc  (C.copNormal "match-beg")      copBeginWithNormal
    , C.CopCalc  (C.copNormal "match-end")      copEndWithNormal
    , C.CopCalc  (C.copNormal "match-mid")      copContainNormal
    , C.CopCalc  (C.copNormal "max")            copMax
    , C.CopCalc  (C.copNormal "min")            copMin
    , C.CopCalc  (C.copNormal "minus")          copMinus
    , C.CopCalc  (C.copNormal "part")           copPart
    , C.CopCalc  (C.copNormal "push")           copPush
    , C.CopCalc  (C.copNormal "reverse")        copReverse
    , C.CopCalc  (C.copNormal "replace-all")    copReplaceAll
    , C.CopCalc  (C.copNormal "replace-begin")  copReplaceBegin
    , C.CopCalc  (C.copNormal "replace-end")    copReplaceEnd
    , C.CopCalc  (C.copNormal "replace-first")  copReplaceFirst
    , C.CopCalc  (C.copNormal "replace-last")   copReplaceLast
    , C.CopCalc  (C.copNormal "sort")           copSort
    , C.CopCalc  (C.copNormal "total")          copTotal
    , C.CopCalc  (C.copNormal "sub-index")      copSubIndex
    , C.CopCalc  (C.copNormal "sub-length")     copSubLength
    , C.CopCalc  (C.copNormal "term-set")       copTermSet

    , C.CopCalc  (C.copInfix  "in") copFunIn
    , C.CopCox   (C.copPrefix "in") copCoxIn
    ]

copList :: (C.CList c) => C.CopCalc c
copList argC = do arg <- sequence argC
                  C.putList arg


-- ----------------------  aggregation

copTotal :: (C.CContent c) => C.CopCalc c
copTotal = op where
    op [Right c] | C.isList c = C.putDec =<< B.decimalSum (map C.gDec $ C.gList c)
    op xs = typeUnmatch xs

copMin :: (C.CContent c) => C.CopCalc c
copMin = op where
    op [Right c] | C.isList c = Right $ minimum (C.gList c)
    op xs = typeUnmatch xs

copMax :: (C.CContent c) => C.CopCalc c
copMax = op where
    op [Right c] | C.isList c = Right $ maximum (C.gList c)
    op xs = typeUnmatch xs

copLength :: (C.CContent c) => C.CopCalc c
copLength = op where
    op [Right c] | C.isList c  = Right . C.pInt $ length (C.gList c)
                 | C.isText c  = Right . C.pInt $ length (C.gText c)
                 | C.isRel c   = Right . C.pInt $ length (B.relBody $ C.gRel c)
    op xs = typeUnmatch xs

typeUnmatch :: C.CTypeOf c => [B.Ab c] -> B.Ab c
typeUnmatch _ = Msg.unmatchType ""



-- ----------------------  set-like operation

copAppend :: (C.CContent c) => C.CopCalc c
copAppend [] = Right C.empty
copAppend xs@(x : _) = op x where
    op (Right c) | C.isText c = C.putText . concat =<< mapM C.getText xs
                 | C.isSet  c = C.putSet  . concat =<< mapM C.getSet  xs
                 | C.isList c = C.putList . concat =<< mapM C.getList xs
    op _ = typeUnmatch xs

copIntersect :: (C.CContent c) => C.CopCalc c
copIntersect [] = Right C.empty
copIntersect xs@(x : _) = op x where
    op (Right c) | C.isSet  c = C.putSet  . intersectLists =<< mapM C.getSet  xs
                 | C.isList c = C.putList . intersectLists =<< mapM C.getList xs
    op _ = typeUnmatch xs

copMinus :: (C.CContent c) => C.CopCalc c
copMinus = op where
    op [Right a,  Right b]
        | C.isSet  a && C.isSet  b = C.putSet  (C.gSet  a List.\\ C.gSet  b)
        | C.isList a && C.isList b = C.putList (C.gList a List.\\ C.gList b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ List.intersect a b : xs


-- ----------------------  arrangement

copSort :: (C.CContent c) => C.CopCalc c
copSort = op where
    op [Right c] | C.isList c  = C.putList $ B.sort $ C.gList c
                 | C.isSet  c  = C.putSet  $ B.sort $ C.gSet  c
                 | otherwise   = Right c
    op xs = typeUnmatch xs

copReverse :: (C.CContent c) => C.CopCalc c
copReverse = op where
    op [Right c] | C.isText c  = C.putText $ reverse $ C.gText c
                 | C.isList c  = C.putList $ reverse $ C.gList c
    op xs = typeUnmatch xs


-- ----------------------  others

copSubIndex :: (C.CContent c) => C.CopCalc c
copSubIndex = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right xs', Right from', Right to')
                      | C.isText xs' && C.isDec from' && C.isDec to'
                      -> let xs   = C.gText xs'
                             from = C.gDec from'
                             to   = C.gDec to'
                         in C.putText (subIndexDecimal from to xs)
                  _ -> typeUnmatch arg

copSubLength :: (C.CContent c) => C.CopCalc c
copSubLength = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right xs', Right from', Right to')
                      | C.isText xs' && C.isDec from' && C.isDec to'
                      -> let xs   = C.gText xs'
                             from = C.gDec from'
                             to   = C.gDec to'
                         in C.putText (subLengthDecimal from to xs)
                  _ -> typeUnmatch arg

subIndexDecimal :: B.Decimal -> B.Decimal -> [a] -> [a]
subIndexDecimal from to = subIndex intFrom intTo where
    intFrom = B.decimalNum from
    intTo   = B.decimalNum to

subLengthDecimal :: B.Decimal -> B.Decimal -> [a] -> [a]
subLengthDecimal from len = subLength intFrom intLen where
    intFrom = B.decimalNum from
    intLen  = B.decimalNum len

subIndex :: Int -> Int -> [a] -> [a]
subIndex from to xs = xs2 where
    xs2 = take to $ drop (from - 1) xs

subLength :: Int -> Int -> [a] -> [a]
subLength from len xs = xs2 where
    xs2 = take len $ drop (from - 1) xs

copPush :: (C.CContent c) => C.CopCalc c
copPush arg =
    do arg2 <- C.getArg2 arg
       case arg2 of
         (Right c, Right cs)
             | C.isSet cs -> C.putSet $ c : C.gSet cs
             | otherwise  -> Msg.reqCollection
         _ -> typeUnmatch arg


-- --------------------------------------------  begin-with / end-with

copBeginWithNormal, copEndWithNormal, copContainNormal :: (C.CContent c) => C.CopCalc c
copBeginWithNormal  = copMatchNormal B.isPrefixOf B.isPrefixOf
copEndWithNormal    = copMatchNormal B.isSuffixOf B.isSuffixOf
copContainNormal    = copMatchNormal B.isInfixOf  B.isInfixOf

copBeginWithInfix, copEndWithInfix, copContainInfix :: (C.CContent c) => C.CopCalc c
copBeginWithInfix  = copMatchInfix B.isPrefixOf B.isPrefixOf
copEndWithInfix    = copMatchInfix B.isSuffixOf B.isSuffixOf
copContainInfix    = copMatchInfix B.isInfixOf  B.isInfixOf

copMatchNormal :: (C.CContent c) => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> C.CopCalc c
copMatchNormal bf1 bf2 arg =
    do arg2  <- C.getArg2 arg
       whole <- snd arg2
       part  <- fst arg2
       copMatch bf1 bf2 part whole

copMatchInfix :: (C.CContent c) => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> C.CopCalc c
copMatchInfix bf1 bf2 arg =
    do arg2  <- C.getArg2 arg
       whole <- fst arg2
       part  <- snd arg2
       copMatch bf1 bf2 part whole

copMatch :: (C.CContent c)
  => (String -> String -> Bool) -> ([c] -> [c] -> Bool) -> c -> c -> B.Ab c
copMatch bf1 bf2 part whole
    | isText2 part whole = match bf1 C.gText
    | isList2 part whole = match bf2 C.gList
    | otherwise          = C.putFalse
    where match bf g = C.putBool $ bf (g part) (g whole)

isList2 :: (C.CList c) => c -> c -> Bool
isList2 x y = C.isList x && C.isList y

isText2 :: (C.CText c) => c -> c -> Bool
isText2 x y = C.isText x && C.isText y


-- --------------------------------------------  in

copFunIn :: (C.CContent c) => C.CopCalc c
copFunIn arg =
    do arg2 <- C.getArg2 arg
       case arg2 of
         (Right c, Right cs)
             | C.isSet  cs -> C.putBool $ c `elem` C.gSet  cs
             | C.isList cs -> C.putBool $ c `elem` C.gList cs
             | otherwise   -> Msg.reqCollection
         _ -> typeUnmatch arg

copCoxIn :: C.CopCox c
copCoxIn [xs]    = Right $ H.f1 $ H.bin "in" H.b1 xs
copCoxIn [x, xs] = Right $ H.f1 $ H.bin "in" x    xs
copCoxIn _       = Msg.adlib "require operand"


-- ----------------------  text

-- char 70 => "F"
copChar :: (C.CContent c) => C.CopCalc c
copChar = op where
    op [Right c] | C.isDec c = C.putText [Char.chr $ B.decimalNum $ C.gDec c]
    op xs = typeUnmatch xs

-- code-list "abc" => [ 97 : 98 : 99 ]
copCodeList :: (C.CContent c) => C.CopCalc c
copCodeList = op where
    op [Right t] | C.isText t = C.contMapTextToList contOrd t
    op xs = typeUnmatch xs

    contOrd :: (C.CDec c) => Char -> c
    contOrd = C.pInt . Char.ord

-- char-group "a!" => [ 'letter : 'punct ]
copCharGroup :: (C.CContent c) => C.CopCalc c
copCharGroup = op where
    op [Right t] | C.isText t = C.contMapTextToList contGroup t
    op xs = typeUnmatch xs

    contGroup :: (C.CText c) => Char -> c
    contGroup = C.pText . charGroup

-- char-group-1 "a!" => 'letter
copCharGroup1 :: (C.CContent c) => C.CopCalc c
copCharGroup1 = op where
    op [Right t] | C.isText t = case C.gText t of
                                  (c : _) -> C.putText $ charGroup c
                                  _       -> Right C.empty
    op xs = typeUnmatch xs

charGroup :: Char -> String
charGroup = B.generalCategoryName . B.generalCategoryGroup


-- ----------------------  base-part / dir-part

-- dir-part "/" "aa/bb/cc.k" => "aa/bb"
copDirPart :: (C.CContent c) => C.CopCalc c
copDirPart = copDirBasePart fst

-- base-part "/" "aa/bb/cc.k" => "aa/bb"
copBasePart :: (C.CContent c) => C.CopCalc c
copBasePart = copDirBasePart snd

copDirBasePart :: (C.CContent c) => ((String, String) -> String) -> C.CopCalc c
copDirBasePart part = op where
    op [Right sep, Right t]
        | C.isText sep && C.isText t =
            C.contApTextToText (part . dirBasePart (head $ C.gText sep)) t
    op xs = typeUnmatch xs

dirBasePart :: Char -> String -> (String, String)
dirBasePart sep s =
    case reverse $ B.divide sep s of
      (base : dir) -> (B.intercalate [sep] $ reverse dir, base)
      []           -> ("", "")


-- ----------------------  replace

copReplaceAll :: (C.CContent c) => C.CopCalc c
copReplaceAll = copReplace replaceAll

copReplaceFirst :: (C.CContent c) => C.CopCalc c
copReplaceFirst = copReplace replaceFirst

copReplaceLast :: (C.CContent c) => C.CopCalc c
copReplaceLast = copReplace replaceLast

copReplaceBegin :: (C.CContent c) => C.CopCalc c
copReplaceBegin = copReplace replaceBegin

copReplaceEnd :: (C.CContent c) => C.CopCalc c
copReplaceEnd = copReplace replaceEnd

copReplace :: (C.CContent c) => (String -> String -> String -> String) -> C.CopCalc c
copReplace rep = op where
    op [Right from, Right to, Right text]
        | C.isText from && C.isText to && C.isText text =
            C.contApTextToText (C.gText from `rep` C.gText to) text
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


-- ----------------------  part

-- part "bc" "abcde" => <1>
copPart :: (C.CContent c) => C.CopCalc c
copPart = op where
    op [Right part, Right whole]
        | C.isText part && C.isText whole =
            let p  = C.gText part
                w  = C.gText whole
                ws = B.tails w
            in C.putBool $ List.isPrefixOf p `any` ws
    op xs = typeUnmatch xs


-- ----------------------  term-set

-- term-set <<< aaa /x bbb /y ccc => { '/x : '/y }
copTermSet :: (C.CContent c) => C.CopCalc c
copTermSet [Right c] | C.isInterp c = C.putSet ts where
                     ts = map C.pTerm $ B.interpTerms $ C.gInterp c
copTermSet xs = typeUnmatch xs


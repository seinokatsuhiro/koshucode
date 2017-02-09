{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators on texts.

module Koshucode.Baala.Cop.Text
  ( copsText,
    copSiv,
    copPad,
    copSuffix, copUnsuffix,
  ) where

import qualified Data.Char                        as Ch
import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Type             as T
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Subtext          as P
import qualified Koshucode.Baala.Rop.Base.Message as Msg

-- | Content operators on texts.
copsText :: (D.CContent c) => [D.Cop c]
copsText =
    [ D.CopCalc  (D.copInfix "*=")                copEndWithInfix
    , D.CopCalc  (D.copInfix "*=*")               copContainInfix
    , D.CopCalc  (D.copInfix "=*")                copBeginWithInfix

    , D.CopCalc  (D.copInfix "=?")                $ copSiv True
    , D.CopCalc  (D.copInfix "=!")                $ copSiv False

    , D.CopCalc  (D.copNormal "base-part")        copBasePart
    , D.CopCalc  (D.copNormal "char")             copChar
    , D.CopCalc  (D.copNormal "char-group")       copCharGroup
    , D.CopCalc  (D.copNormal "check")            copCheck
    , D.CopCalc  (D.copNormal "from-bool")        copFromBool
    , D.CopCalc  (D.copNormal "char-group-1")     copCharGroup1
    , D.CopCalc  (D.copNormal "code-list")        copCodeList
    , D.CopCalc  (D.copNormal "dir-part")         copDirPart
    , D.CopCalc  (D.copNormal "general-symbol?")  copGeneralSymbol
    , D.CopCalc  (D.copNormal "numeric-symbol?")  copNumericSymbol
    , D.CopCalc  (D.copNormal "plain-symbol?")    copPlainSymbol
    , D.CopCalc  (D.copNormal "short-symbol?")    copShortSymbol

    , D.CopCalc  (D.copNormal "trim-begin")       copTrimBegin
    , D.CopCalc  (D.copNormal "trim-end")         copTrimEnd
    , D.CopCalc  (D.copNormal "trim-both")        copTrimBoth
    , D.CopCalc  (D.copNormal "trim-text-begin")  copTrimTextBegin
    , D.CopCalc  (D.copNormal "trim-text-end")    copTrimTextEnd
    , D.CopCalc  (D.copNormal "trim-text-both")   copTrimTextBoth

    , D.CopCalc  (D.copNormal "divide-text-begin") $ copDivideTextBy divideBegin
    , D.CopCalc  (D.copNormal "divide-text-first") $ copDivideTextBy divideFirst
    , D.CopCalc  (D.copNormal "divide-text-last")  $ copDivideTextBy divideLast
    , D.CopCalc  (D.copNormal "divide-text-end")   $ copDivideTextBy divideEnd
    , D.CopCalc  (D.copNormal "divide-text-all")   $ copDivideTextBy divideAll

    , D.CopCalc  (D.copNormal "pad-begin")         $ copPad O.padBeginWith
    , D.CopCalc  (D.copNormal "pad-end")           $ copPad O.padEndWith

    , D.CopCalc  (D.copNormal "unwords")          copUnwords
    , D.CopCalc  (D.copNormal "unwords-by")       copUnwordsBy
    , D.CopCalc  (D.copNormal "words")            copWords
    , D.CopCalc  (D.copNormal "words-by")         copWordsBy

    , D.CopCalc  (D.copNormal "suffix")           copSuffix
    , D.CopCalc  (D.copNormal "unsuffix")         copUnsuffix

    , D.CopCalc  (D.copNormal "match-beg")        copBeginWithNormal
    , D.CopCalc  (D.copNormal "match-end")        copEndWithNormal
    , D.CopCalc  (D.copNormal "match-mid")        copContainNormal
    ]


-- ----------------------  begin-with / end-with

copBeginWithNormal, copEndWithNormal, copContainNormal :: (D.CContent c) => D.CopCalc c
copBeginWithNormal  = copMatchNormal O.csIsPrefix B.isPrefixOf
copEndWithNormal    = copMatchNormal O.csIsSuffix B.isSuffixOf
copContainNormal    = copMatchNormal O.csIsInfix  B.isInfixOf

copBeginWithInfix, copEndWithInfix, copContainInfix :: (D.CContent c) => D.CopCalc c
copBeginWithInfix   = copMatchInfix O.csIsPrefix B.isPrefixOf
copEndWithInfix     = copMatchInfix O.csIsSuffix B.isSuffixOf
copContainInfix     = copMatchInfix O.csIsInfix  B.isInfixOf

copMatchNormal :: (D.CContent c)
  => (S.Chars -> S.Chars -> Bool) -> ([c] -> [c] -> Bool) -> D.CopCalc c
copMatchNormal bf1 bf2 arg =
    do (part, whole) <- D.getRightArg2 arg
       copMatch bf1 bf2 (part, whole)

copMatchInfix :: (D.CContent c)
  => (S.Chars -> S.Chars -> Bool) -> ([c] -> [c] -> Bool) -> D.CopCalc c
copMatchInfix bf1 bf2 arg =
    do (whole, part) <- D.getRightArg2 arg
       copMatch bf1 bf2 (part, whole)

copMatch :: (D.CContent c)
  => (S.Chars -> S.Chars -> Bool) -> ([c] -> [c] -> Bool) -> (c, c) -> B.Ab c
copMatch bf1 bf2 (part, whole)
    | isText2 part whole = match bf1 D.gText
    | isList2 part whole = match bf2 D.gList
    | otherwise          = D.putFalse
    where match bf g = D.putBool $ bf (g part) (g whole)

isList2 :: (D.CList c) => c -> c -> Bool
isList2 x y = D.isList x && D.isList y

isText2 :: (D.CText c) => c -> c -> Bool
isText2 x y = D.isText x && D.isText y


-- ---------------------- Siv pattern

-- | [__/TEXT/ =? /PATTERN/__] Test /TEXT/ matches /PATTERN/.
--   [__/TEXT/ =! /PATTERN/__] Test /TEXT/ does not matches /PATTERN/.
copSiv :: (D.CContent c) => Bool -> D.CopCalc c
copSiv b [D.getText -> Right t, D.getText -> Right p] =
    do res <- P.sivMatch (O.tString p) (O.tString t)
       D.putBool $ res == b
copSiv _ cs = Msg.unexpArg cs ["text", "pattern text"]


-- ----------------------  text

-- char 70 => "F"
copChar :: (D.CContent c) => D.CopCalc c
copChar = op where
    op [Right c] | D.isDec c = D.putText [Ch.chr $ fromInteger $ T.decimalNum $ D.gDec c]
    op xs = Msg.badArg xs

-- code-list "abc" => [ 97 : 98 : 99 ]
copCodeList :: (D.CContent c) => D.CopCalc c
copCodeList = op where
    op [Right t] | D.isText t = Right $ D.contentMapTextList contOrd t
    op xs = Msg.badArg xs

    contOrd :: (D.CDec c) => Char -> c
    contOrd = D.pInt . Ch.ord

-- char-group "a!" => [ 'letter : 'punct ]
copCharGroup :: (D.CContent c) => D.CopCalc c
copCharGroup = op where
    op [Right t] | D.isText t = Right $ D.contentMapTextList contGroup t
    op xs = Msg.badArg xs

    contGroup :: (D.CText c) => Char -> c
    contGroup = D.pText . charGroup

-- char-group-1 "a!" => 'letter
copCharGroup1 :: (D.CContent c) => D.CopCalc c
copCharGroup1 [D.getText -> Right t]
    = case O.cut t of
        O.Jp c _ -> D.putText $ charGroup c
        _        -> Right D.empty
copCharGroup1 xs = Msg.badArg xs

charGroup :: Char -> String
charGroup = O.generalCategoryName . O.majorGeneralCategory


-- ----------------------  from-bool

-- from-bool "yes" "no" (+) => "yes"
copFromBool :: (D.CContent c) => D.CopCalc c
copFromBool = op where
    op [Right t, Right f, Right c]
        | D.isBool c = Right $ fromBool t f $ D.gBool c
        | otherwise  = Right c
    op xs = Msg.badArg xs

-- check (+) => "✓" (U+2713)
copCheck :: (D.CContent c) => D.CopCalc c
copCheck = op where
    op [Right c]
        | D.isBool c = Right $ from D.empty $ D.gBool c
        | otherwise  = Right c
    op [Right c, Right alt]
        | D.isBool c = Right $ from alt $ D.gBool c
        | otherwise  = Right c
    op xs = Msg.badArg xs

    from = fromBool (D.pText "✓")

-- | Choose true/false sign for Boolean value.
fromBool :: (D.CContent c) => c -> c -> Bool -> c
fromBool t _ True   = t
fromBool _ f False  = f


-- ----------------------  base-part / dir-part

-- dir-part "/" "aa/bb/cc.k" => "aa/bb"
copDirPart :: (D.CContent c) => D.CopCalc c
copDirPart = copDirBasePart fst

-- base-part "/" "aa/bb/cc.k" => "aa/bb"
copBasePart :: (D.CContent c) => D.CopCalc c
copBasePart = copDirBasePart snd

copDirBasePart :: (D.CContent c) => ((S.Chars, S.Chars) -> S.Chars) -> D.CopCalc c
copDirBasePart part [D.getChar -> Right sep, D.getText -> Right t]
    = D.putText $ (part . dirBasePart sep) t
copDirBasePart _ xs = Msg.badArg xs

dirBasePart :: (O.Textual t) => Char -> t -> (t, t)
dirBasePart sep t =
    case reverse $ O.tDivide (== sep) t of
      (base : dir) -> (O.tJoinWith (O.charT sep) $ reverse dir, base)
      []           -> (O.tEmpty, O.tEmpty)


-- ----------------------  words words-by

copWords :: (D.CContent c) => D.CopCalc c
copWords arg =
    do ws <- D.getRightArg1 arg
       if D.isText ws
          then D.putList (D.pText <$> (O.tWords $ D.gText ws))
          else Msg.badArg arg

copWordsBy :: (D.CContent c) => D.CopCalc c
copWordsBy arg =
    do (sep, ws) <- D.getRightArg2 arg
       case D.isText ws && D.isText sep of
         False -> Msg.badArg arg
         True  -> let isSep = (`O.csElem` D.gText sep)
                  in D.putList $ map D.pText $ O.tWordsBy isSep $ D.gText ws


-- ----------------------  unwords unwords-by

copUnwords :: (D.CContent c) => D.CopCalc c
copUnwords arg =
    do x <- D.getRightArg1 arg
       D.putText $ O.tUnwords $ wordList x

copUnwordsBy :: (D.CContent c) => D.CopCalc c
copUnwordsBy arg =
    do (sep, x) <- D.getRightArg2 arg
       case D.isText sep of
         True  -> let sep' = D.gText sep
                  in D.putText $ O.tJoinWith sep' $ wordList x
         False -> Msg.badArg arg

wordList :: (D.CContent c) => c -> [S.Chars]
wordList c
    | D.isList c  = concatMap wordList $ D.gList c
    | D.isText c  = [D.gText c]
    | D.isDec c   = [T.encodeDecimal $ D.gDec c]
    | otherwise   = []


-- ----------------------  trim

copTrimBegin :: (D.CContent c) => D.CopCalc c
copTrimBegin = copTrimBy O.trimBegin

copTrimEnd :: (D.CContent c) => D.CopCalc c
copTrimEnd = copTrimBy O.trimEnd

copTrimBoth :: (D.CContent c) => D.CopCalc c
copTrimBoth = copTrimBy O.trimBoth

copTrimBy :: (D.CContent c) => S.CharsMap -> D.CopCalc c
copTrimBy f arg =
    do text <- D.getRightArg1 arg
       case D.isText text of
         True  -> D.putText $ f (D.gText text)
         False -> Msg.badArg arg

copTrimTextBegin :: (D.CContent c) => D.CopCalc c
copTrimTextBegin = copTrimTextBy trimTextBegin

copTrimTextEnd :: (D.CContent c) => D.CopCalc c
copTrimTextEnd = copTrimTextBy trimTextEnd

copTrimTextBoth :: (D.CContent c) => D.CopCalc c
copTrimTextBoth = copTrimTextBy trimTextBoth

copTrimTextBy :: (D.CContent c) => O.Bin S.Chars -> D.CopCalc c
copTrimTextBy f arg =
    do (trim, text) <- D.getRightArg2 arg
       case D.isText trim && D.isText text of
         True  -> D.putText $ f (D.gText trim) (D.gText text)
         False -> Msg.badArg arg

trimTextBegin :: (O.Textual t) => O.Bin t
trimTextBegin t = snd . O.tWhile (`O.csElem` t)

trimTextEnd :: (O.Textual t) => O.Bin t
trimTextEnd t = O.csReverse . trimTextBegin t . O.csReverse

trimTextBoth :: (O.Textual t) => O.Bin t
trimTextBoth t = trimTextEnd t . trimTextBegin t


-- ----------------------  divide-text

type Divider = (Char -> Bool) -> S.Chars -> [S.Chars]

copDivideTextBy :: (D.CContent c) => Divider -> D.CopCalc c
copDivideTextBy f arg =
    do (divide, text) <- D.getRightArg2 arg
       case D.isText divide && D.isText text of
         True  -> D.putList $ D.pText <$> f (`O.csElem` D.gText divide) (D.gText text)
         False -> Msg.badArg arg

-- | Divide list about beginning of list.
--   This function returns two-element list: [match, right].
--
--   >>> divideBegin (== '1') "11-22-33-44-55"
--   ["11","-22-33-44-55"]
--
--   >>> divideBegin (== '-') "11-22-33-44-55"
--   ["","11-22-33-44-55"]

divideBegin :: Divider
divideBegin p x =
    let (match, right) = O.tWhile p x
    in [match, right]

-- | Divide list at matching first.
--   This function returns three-element list: [left, match, right].
--
--   >>> divideFirst (== '-') "11-22-33-44-55"
--   ["11","-","22-33-44-55"]
--
--   >>> divideFirst (== '-') "-"
--   ["","-",""]
--
--   >>> divideFirst (== '-') ""
--   ["","",""]

divideFirst :: Divider
divideFirst p x =
    let (left, y)      = O.tWhileNot p x
        (match, right) = O.tWhile p y
    in [left, match, right]

-- | Divide list at matching last.
--   This function returns three-element list: [left, match, right].
--
--   >>> divideLast (== '-') "11-22-33-44-55"
--   ["11-22-33-44","-","55"]

divideLast :: Divider
divideLast p x = reverse $ O.csReverse <$> (divideFirst p $ O.csReverse x)

-- | Divide list about end of list.
--   This function returns two-element list: [left, match].
--
--   >>> divideEnd (== '5') "11-22-33-44-55"
--   ["11-22-33-44-","55"]

divideEnd :: Divider
divideEnd p x = reverse $ O.csReverse <$> (divideBegin p $ O.csReverse x)

-- | Divide list at all matching positions.
--   This function returns odd-element list:
--   [left, match, mid, match, ..., match, right].
--   Matching sublists are in odd position.
--
--   >>> divideAll (== '-') "11-22-33-44-55"
--   ["11","-","22","-","33","-","44","-","55"]
--
--   >>> divideAll (== '-') "-"
--   ["","-",""]
--
--   >>> divideAll (== '-') ""
--   [""]

divideAll :: Divider
divideAll p x =
    case divideFirst p x of
      [left, e1, e2]   | O.tIsEmpty e1 && O.tIsEmpty e2 -> [left]
      [left, match, e] | O.tIsEmpty e -> [left, match, e]
      [left, match, right] -> left : match : divideAll p right
      _                    -> error "unexpected value from divideFirst"


-- ----------------------  general-symbol?

copGeneralSymbol :: (D.CContent c) => D.CopCalc c
copGeneralSymbol = copTestSymbol S.isGeneralSymbol

copPlainSymbol :: (D.CContent c) => D.CopCalc c
copPlainSymbol = copTestSymbol S.isPlainSymbol

copNumericSymbol :: (D.CContent c) => D.CopCalc c
copNumericSymbol = copTestSymbol S.isNumericSymbol

copShortSymbol :: (D.CContent c) => D.CopCalc c
copShortSymbol = copTestSymbol S.isShortSymbol

copTestSymbol :: (D.CContent c) => O.Test (S.Symbol S.Chars) -> D.CopCalc c
copTestSymbol test arg =
    do c <- D.getRightArg1 arg
       D.putBool $ case extractText c of
                     Nothing  -> False
                     Just s   -> case S.nextSymbol s of
                                   (rest, sym) | O.tIsEmpty rest -> test sym
                                   _ -> False

extractText :: (D.CContent c) => c -> Maybe S.Chars
extractText c
    | D.isCode c  = Just $ D.gCode c
    | D.isText c  = Just $ D.gText c
    | D.isTerm c  = Just $ O.stringT $ S.termNameContent $ D.gTerm c
    | otherwise   = Nothing


-- ----------------------  padding

-- | [pad-begin /CHAR/ /SIZE/ /TEXT/] Pad /CHAR/ to beginning of /TEXT/ upto /SIZE/.
--   [pad-end /CHAR/ /SIZE/ /TEXT/] Pad /CHAR/ to end of /TEXT/ upto /SIZE/.
copPad :: (D.CContent c) => (Char -> Int -> S.CharsMap) -> D.CopCalc c
copPad pad [D.getChar -> Right c, D.getIntegral -> Right n, D.getText -> Right t] =
    D.putText $ pad c n t
copPad _ cs = Msg.unexpArg cs ["padding character", "max size", "text"]


-- ----------------------  suffix & unsuffix

-- | [suffix /LIST/] Add unique suffixes to text elements of /LIST/.
copSuffix :: (D.CContent c) => D.CopCalc c
copSuffix = op where
    op [Right ls] | D.isList ls && (all D.isText $ D.gList ls)
           = D.putList (D.pText <$> B.uniqueNames '-' (D.gText <$> D.gList ls))
    op [c] = c
    op xs  = Msg.badArg xs

-- | [unsuffix /C/] Remove integer suffixes from /C/.
copUnsuffix :: (D.CContent c) => D.CopCalc c
copUnsuffix = op where
    op [Right c] = Right $ unsuf c
    op [c]       = c
    op xs        = Msg.badArg xs

    unsuf c | D.isText c  = D.pText $ B.unsuffix Ch.isDigit '-' $ D.gText c
            | D.isList c  = D.pList (unsuf <$> D.gList c)
            | D.isSet  c  = D.pSet  (unsuf <$> D.gSet c)
            | otherwise   = c

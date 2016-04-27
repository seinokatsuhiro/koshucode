{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Text
  ( copsText
  ) where

import qualified Data.Char                       as Char
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as D
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Cop.Message     as Msg

copsText :: (D.CContent c) => [D.Cop c]
copsText =
    [ D.CopCalc  (D.copInfix "*=")                copEndWithInfix
    , D.CopCalc  (D.copInfix "*=*")               copContainInfix
    , D.CopCalc  (D.copInfix "=*")                copBeginWithInfix

    , D.CopCalc  (D.copNormal "base-part")        copBasePart
    , D.CopCalc  (D.copNormal "char")             copChar
    , D.CopCalc  (D.copNormal "char-group")       copCharGroup
    , D.CopCalc  (D.copNormal "char-group-1")     copCharGroup1
    , D.CopCalc  (D.copNormal "code-list")        copCodeList
    , D.CopCalc  (D.copNormal "dir-part")         copDirPart
    , D.CopCalc  (D.copNormal "general-symbol?")  copGeneralSymbol
    , D.CopCalc  (D.copNormal "numeric-symbol?")  copNumericSymbol
    , D.CopCalc  (D.copNormal "plain-symbol?")    copPlainSymbol
    , D.CopCalc  (D.copNormal "short-symbol?")    copShortSymbol
    , D.CopCalc  (D.copNormal "unwords")          copUnwords
    , D.CopCalc  (D.copNormal "unwords-by")       copUnwordsBy
    , D.CopCalc  (D.copNormal "words")            copWords
    , D.CopCalc  (D.copNormal "words-by")         copWordsBy

    , D.CopCalc  (D.copNormal "match-beg")        copBeginWithNormal
    , D.CopCalc  (D.copNormal "match-end")        copEndWithNormal
    , D.CopCalc  (D.copNormal "match-mid")        copContainNormal
    ]

typeUnmatch :: D.CTypeOf c => [B.Ab c] -> B.Ab c
typeUnmatch _ = Msg.unmatchType ""


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
charGroup = B.generalCategoryName . B.majorGeneralCategory


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


-- ----------------------  general-symbol?

copGeneralSymbol :: forall c. (D.CContent c) => D.CopCalc c
copGeneralSymbol = copTestSymbol D.isGeneralSymbol

copPlainSymbol :: forall c. (D.CContent c) => D.CopCalc c
copPlainSymbol = copTestSymbol D.isPlainSymbol

copNumericSymbol :: forall c. (D.CContent c) => D.CopCalc c
copNumericSymbol = copTestSymbol D.isNumericSymbol

copShortSymbol :: forall c. (D.CContent c) => D.CopCalc c
copShortSymbol = copTestSymbol D.isShortSymbol

copTestSymbol :: forall c. (D.CContent c) => (D.Symbol -> Bool) -> D.CopCalc c
copTestSymbol test arg =
    do c <- D.getRightArg1 arg
       D.putBool $ case extractText c of
                     Nothing  -> False
                     Just s   -> case D.nextSymbol s of
                                   ("", sym) -> test sym
                                   _         -> False

extractText :: (D.CContent c) => c -> Maybe String
extractText c
    | D.isCode c  = Just $ D.gCode c
    | D.isText c  = Just $ D.gText c
    | D.isTerm c  = Just $ D.gTerm c
    | otherwise   = Nothing


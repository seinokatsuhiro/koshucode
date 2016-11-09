{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Term content parsers.

module Koshucode.Baala.Data.Content.Tree
  ( -- * Single content
    treeText,
    treesDigits,
    tokenClock,
    treesTime, stringTime,
    treesInterp,

    -- * Multiple contents
    treeFlatName,
    treesFlatNames,
    treesTerms,
    treesTerms1,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Type             as D
import qualified Koshucode.Baala.Data.Content.Message  as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  Text

-- | Get single text from token trees.
--
--   >>> S.tt "'aa 'bb" >>= treesToText True
--   Right "aabb"
--
--   >>> S.tt "aa bb" >>= treesToText False
--   Right "aabb"
--
treesTexts :: Bool -> [S.TTree] -> B.Ab [String]
treesTexts q = mapM $ treeText q

-- | Get text from token tree.
--
--   >>> S.tt1 "aa" >>= treeText False
--   Right "aa"
--
treeText :: Bool -> S.TTree -> B.Ab String
treeText q (L tok) = tokenString q tok
treeText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenString :: Bool -> S.Token -> B.Ab String
tokenString True  (S.TText _ q w) | q > S.TextRaw  = Right w
tokenString False (S.TTextRaw _ w)                 = Right w
tokenString _ _  =  Msg.nothing


-- ----------------------  Number

-- | Get digits from token trees.
--
--   >>> S.tt "-123 450.00" >>= treesDigits
--   Right "-123450.00"
--
treesDigits :: [S.TTree] -> B.Ab String
treesDigits = concatDigits B.<=< treesTexts False

concatDigits :: [String] -> B.Ab String
concatDigits = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789o" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.o") w = loop (w:ss) xs
    loop _ _ = Msg.nothing


-- ----------------------  Clock

-- | Get clock from token.
--
--   >>> tokenClock $ head $ S.toks "|12:00|"
--   Right |12:00|
--
tokenClock :: S.Token -> B.Ab D.Clock
tokenClock (S.TTextBar _ ('|' : w)) = textClock w
tokenClock _ = Msg.nothing

textClock :: String -> B.Ab D.Clock
textClock = sign where
    sign ('-' : cs)  = D.clockNeg `fmap` dayOrHour cs
    sign ('+' : cs)  = dayOrHour cs
    sign cs          = dayOrHour cs

    dayOrHour cs     = case getInt cs of
                         (h, ':'  : cs')  ->  minute 0 h cs'
                         (d, "'|")        ->  Right $ D.ClockD $ toInteger d
                         (h, "|")         ->  clock D.ClockDh 0 h 0 0
                         (d, '\'' : cs')  ->  hour (toInteger d) cs'
                         _                ->  Msg.nothing

    hour d cs        = case getInt cs of
                         (h, ':' : cs')   ->  minute d h cs'
                         (h, "|")         ->  clock D.ClockDh d h 0 0
                         _                ->  Msg.nothing

    minute d h cs    = case getInt cs of
                         (m, ':' : cs')   ->  second d h m cs'
                         (m, "|")         ->  clock D.ClockDhm d h m 0
                         _                ->  Msg.nothing

    second d h m cs  = case getInt cs of
                         (s, "|")         ->  clock D.ClockDhms d h m s
                         _                ->  Msg.nothing

    clock k d h m s
        | m >= 60    = Msg.nothing
        | s >= 60    = Msg.nothing
        | otherwise  = let (d', h') = h `divMod` 24
                       in Right $ k (d + toInteger d') $ D.secFromHms (h', m, s)

getInt :: String -> (Int, String)
getInt = loop 0 where
    loop n ""     = (n, "")
    loop n (c:cs) = case fromDigit c of
                      Just x  -> loop (10 * n + x) cs
                      Nothing -> (n, c:cs)

fromDigit :: Char -> Maybe Int
fromDigit '0'  =  Just 0
fromDigit '1'  =  Just 1
fromDigit '2'  =  Just 2
fromDigit '3'  =  Just 3
fromDigit '4'  =  Just 4
fromDigit '5'  =  Just 5
fromDigit '6'  =  Just 6
fromDigit '7'  =  Just 7
fromDigit '8'  =  Just 8
fromDigit '9'  =  Just 9
fromDigit _    =  Nothing


-- ----------------------  Time

-- | Get time from token trees.
--
--   >>> S.tt "2013-04-18 12:00" >>= treesTime
--   Right 2013-04-18 12:00
--
--   >>> S.tt "2013-04-18" >>= treesTime
--   Right 2013-04-18
--
--   >>> S.tt "2013-#16" >>= treesTime
--   Right 2013-#16
--
treesTime :: [S.TTree] -> B.Ab D.Time
treesTime = stringsTime B.<=< treesTexts False

-- | Get time from string.
--
--   >>> stringTime "2013-04-18 12:00"
--   Right 2013-04-18 12:00
--
stringTime :: String -> B.Ab D.Time
stringTime = S.tt B.>=> treesTime

stringsTime :: [String] -> B.Ab D.Time
stringsTime = year where

    year []             = Msg.nothing
    year (cs : xs)      = case getInt cs of
                            (y, '-'  : cs')  -> mwd (toInteger y) $ cs' : xs
                            _                -> Msg.nothing

    mwd _ []                      = Msg.nothing
    mwd y (('#' : '#': cs) : xs)  = day (D.dateFromYdAb y) $ cs : xs
    mwd y (('#' : cs) : xs)       = week y $ cs : xs
    mwd y xs                      = month y xs

    month _ []          = Msg.nothing
    month y (cs : xs)   = case getInt cs of
                            (m, '-'  : cs')  -> day (D.dateFromYmdAb y m) $ cs' : xs
                            (m, "")          -> D.timeFromYmAb y m
                            _                -> Msg.nothing

    week _ []           = Msg.nothing
    week y (cs : xs)    = case getInt cs of
                            (w, '-'  : cs')  -> day (D.dateFromYwdAb y w) $ cs' : xs
                            (w, "")          -> D.timeFromYwAb y w
                            _                -> Msg.nothing

    day _ []            = Msg.nothing
    day date (cs : xs)  = case getInt cs of
                            (d, "") | null xs    -> do d' <- date d
                                                       Right $ D.TimeYmd d'
                                    | otherwise  -> do d' <- date d
                                                       hour (D.timeFromDczAb d') $ concat xs
                            _                    -> Msg.nothing

    hour k cs           = case getInt cs of
                            (h, "")          -> k (D.clockFromDh 0 h) Nothing
                            (h, ':' : cs')   -> minute k h cs'
                            _                -> Msg.nothing

    minute k h cs       = case getInt cs of
                            (m, "")          -> k (D.clockFromDhm 0 h m) Nothing
                            (m, ':' : cs')   -> second k h m cs'
                            (m, cs')         -> zone1 k h m Nothing cs'

    second k h m cs     = case getInt cs of
                            (s, "")          -> k (D.clockFromDhms 0 h m s) Nothing
                            (s, cs')         -> zone1 k h m (Just s) cs'

    zone1 k h m s cs    = case cs of
                            '+' : cs'        -> zone2 k h m s   1  cs'
                            '-' : cs'        -> zone2 k h m s (-1) cs'
                            "UTC"            -> zone4 k h m s 0 0
                            _                -> Msg.nothing

    zone2 k h m s pm cs = case getInt cs of
                            (zh, ':' : cs')  -> zone3 k h m s (pm * zh) cs'
                            _                -> Msg.nothing

    zone3 k h m s zh cs = case getInt cs of
                            (zm, "")         -> zone4 k h m s zh zm
                            _                -> Msg.nothing

    zone4 k h m s zh zm = let c = D.clockFromHms (h - zh) (m - zm) s
                              z = D.secFromHms (zh, zm, 0)
                          in k c $ Just z


-- ----------------------  Interp

-- | Get interpretation from token trees.
--
--   >>> S.tt "term /a" >>= treesInterp
--   Right (Interp { interpWords = [InterpText "term", InterpTerm "a"],
--                   interpTerms = ["a"] })
--
treesInterp :: [S.TTree] -> B.Ab D.Interp
treesInterp = Right . D.interp B.<=< mapM treeInterpWord

treeInterpWord :: S.TTree -> B.Ab D.InterpWord
treeInterpWord (B.TreeB _ _ _) = Msg.nothing
treeInterpWord (B.TreeL x) =
    case x of
      S.TText _ _ w    -> Right $ D.InterpText w
      S.TTermN _ _ n   -> Right $ D.InterpTerm n
      S.TTerm _ _ [n]  -> Right $ D.InterpTerm n
      _                -> Msg.nothing


-- ----------------------  Term

-- | Read flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
--
--   >>> S.tt1 "/a" >>= treeFlatName
--   Right "a"
--
--   >>> S.tt1 "+/a" >>= treeFlatName
--   Right "a"
--
--   >>> S.tt1 "/a/x" >>= treeFlatName
--   Left ...
--
treeFlatName :: S.TTree -> B.Ab S.TermName
treeFlatName (L (S.TTermN _ _ n))  = Right n
--treeFlatName (L (S.TTerm _ _ [n])) = Right n
treeFlatName (L t)                 = Msg.reqFlatName t
treeFlatName _                     = Msg.reqTermName

-- | Read flat term names.
--
--   >>> S.tt "/a /b" >>= treesFlatNames
--   Right ["a","b"]
--
treesFlatNames :: [S.TTree] -> B.Ab [S.TermName]
treesFlatNames = mapM treeFlatName

-- | Read list of named token trees from token trees.
--
--   >>> S.tt "/a 'A3 /b 10" >>= treesTerms
--   Right [("a", [TreeL ...]),
--          ("b", [TreeL ...])]
--
treesTerms :: [S.TTree] -> B.Ab [S.NamedTrees]
treesTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeFlatName x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: S.TTreesTo ([S.TTree], [S.TTree])
    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

    isTermLeaf (L (S.TTermN _ _ _))             = True
    isTermLeaf (L (S.TTerm _ S.TermTypePath _)) = True
    isTermLeaf _                                = False

-- | Read list of named token trees from token trees.
--   This function wraps long branches into group.
treesTerms1 :: [S.TTree] -> B.Ab [S.NamedTree]
treesTerms1 xs = do xs' <- treesTerms xs
                    Right $ B.mapSndTo S.ttreeGroup xs'

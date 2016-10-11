{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Utilities for token trees.

module Koshucode.Baala.Data.Content.Tree
  ( treesToText,
    treeToText,
    treesToDigits,
    tokenClock,
    treesToTime,
    treeToFlatTerm,
    treesToTerms,
    treesToTerms1,
    treesToInterp,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Type             as D
import qualified Koshucode.Baala.Data.Content.Message  as Msg


-- ----------------------  Text

-- | Get single text from token trees.
--
--   >>> S.tt "'aa 'bb" >>= treesToText True
--   Right "aabb"
--
--   >>> S.tt "aa bb" >>= treesToText False
--   Right "aabb"
--
treesToText :: Bool -> S.TTreesToAb String
treesToText q xs = do ss <- treesToTexts q xs
                      Right $ concat ss

-- | Get text list from token trees.
treesToTexts :: Bool -> S.TTreesToAb [String]
treesToTexts q = mapM $ treeToText q

-- | Get text from token tree.
treeToText :: Bool -> S.TTreeToAb String
treeToText q (B.TreeL tok) = tokenToText q tok
treeToText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenToText :: Bool -> S.Token -> B.Ab String
tokenToText True  (S.TText _ q w) | q > S.TextRaw  = Right w
tokenToText False (S.TTextRaw _ w)                 = Right w
tokenToText _ _  =  Msg.nothing

-- | Get digits from token trees.
--
--   >>> S.tt "-123 450.00" >>= treesToDigits
--   Right "-123450.00"
--
treesToDigits :: S.TTreesToAb String
treesToDigits = concatDigits B.<=< treesToTexts False

concatDigits :: [String] -> B.Ab String
concatDigits = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789o" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.o") w = loop (w:ss) xs
    loop _ _ = Msg.nothing


-- ----------------------  Clock

-- | Get clock from token.
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
--   >>> S.tt "2013-04-18 12:00" >>= treesToTime
--   Right (TimeYmdc (Monthly 2013-04-18) (ClockDhm 0 43200))
--
--   >>> S.tt "2013-04-18" >>= treesToTime
--   Right (TimeYmd (Monthly 2013-04-18))
--
--   >>> S.tt "2013-#16" >>= treesToTime
--   Right (TimeYw 2013-04-15)
--
treesToTime :: S.TTreesToAb D.Time
treesToTime = concatTime B.<=< treesToTexts False

concatTime :: [String] -> B.Ab D.Time
concatTime = year where

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


-- ----------------------  Term

-- | Get flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
treeToFlatTerm :: S.TTreeToAb S.TermName
treeToFlatTerm (S.TermLeafName _ _ n)  = Right n
treeToFlatTerm (B.TreeL t)             = Msg.reqFlatName t
treeToFlatTerm _                       = Msg.reqTermName

-- | Get list of named token trees from token trees.
--
--   >>> S.tt "/a 'A3 /b 10" >>= treesToTerms
--   Right [("a", [TreeL ...]),
--          ("b", [TreeL ...])]
--
treesToTerms :: S.TTreesToAb [S.NamedTrees]
treesToTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeToFlatTerm x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: S.TTreesTo ([S.TTree], [S.TTree])
    cont xs@(x : _) | isTermLeaf x  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.consFst x $ cont xs

    isTermLeaf (S.TermLeafName _ _ _) = True
    isTermLeaf (S.TermLeafPath _ _)   = True
    isTermLeaf _                      = False

-- | Get list of named token trees from token trees.
--   This function wraps long branches into group.
treesToTerms1 :: S.TTreesToAb [S.NamedTree]
treesToTerms1 xs = do xs' <- treesToTerms xs
                      Right $ B.mapSndTo S.ttreeGroup xs'


-- ----------------------  Interp

-- | Get interpretation from token trees.
--
--   >>> S.tt "term /a" >>= treesToInterp
--   Right (Interp { interpWords = [InterpText "term", InterpTerm "a"],
--                   interpTerms = ["a"] })
--
treesToInterp :: S.TTreesToAb D.Interp
treesToInterp = Right . D.interp B.<=< mapM treeToInterpWord

treeToInterpWord :: S.TTreeToAb D.InterpWord
treeToInterpWord (B.TreeB _ _ _) = Msg.nothing
treeToInterpWord (B.TreeL x) =
    case x of
      S.TText _ _ w    -> Right $ D.InterpText w
      S.TTermN _ _ n   -> Right $ D.InterpTerm n
      S.TTerm _ _ [n]  -> Right $ D.InterpTerm n
      _                -> Msg.nothing

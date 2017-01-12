{-# OPTIONS_GHC -Wall #-}

-- | Decode numeric contents.

module Koshucode.Baala.Data.Decode.Numeric
  ( -- * Numeric
    treesDigits,
    tokenClock,
    treesTime, stringTime,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type                  as T
import qualified Koshucode.Baala.Data.Decode.Type      as D
import qualified Koshucode.Baala.Syntax.Pattern        as P
import qualified Koshucode.Baala.Data.Decode.Message   as Msg


-- ----------------------  Number

-- | Get digits from token trees.
--
--   >>> S.toTrees "-123 450.00" >>= treesDigits
--   Right "-123450.00"
--
treesDigits :: [S.Tree] -> B.Ab String
treesDigits = concatDigits O.#. D.treesTexts False

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
tokenClock :: S.Token -> B.Ab T.Clock
tokenClock (P.TBar ('|' : w)) = textClock w
tokenClock _ = Msg.nothing

textClock :: String -> B.Ab T.Clock
textClock = sign where
    sign ('-' : cs)  = T.clockNeg `fmap` dayOrHour cs
    sign ('+' : cs)  = dayOrHour cs
    sign cs          = dayOrHour cs

    dayOrHour cs     = case getInt cs of
                         (h, ':'  : cs')  ->  minute 0 h cs'
                         (d, "'|")        ->  Right $ T.ClockD $ toInteger d
                         (h, "|")         ->  clock T.ClockDh 0 h 0 0
                         (d, '\'' : cs')  ->  hour (toInteger d) cs'
                         _                ->  Msg.nothing

    hour d cs        = case getInt cs of
                         (h, ':' : cs')   ->  minute d h cs'
                         (h, "|")         ->  clock T.ClockDh d h 0 0
                         _                ->  Msg.nothing

    minute d h cs    = case getInt cs of
                         (m, ':' : cs')   ->  second d h m cs'
                         (m, "|")         ->  clock T.ClockDhm d h m 0
                         _                ->  Msg.nothing

    second d h m cs  = case getInt cs of
                         (s, "|")         ->  clock T.ClockDhms d h m s
                         _                ->  Msg.nothing

    clock k d h m s
        | m >= 60    = Msg.nothing
        | s >= 60    = Msg.nothing
        | otherwise  = let (d', h') = h `divMod` 24
                       in Right $ k (d + toInteger d') $ T.secFromHms (h', m, s)

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
--   >>> S.toTrees "2013-04-18 12:00" >>= treesTime
--   Right 2013-04-18 12:00
--
--   >>> S.toTrees "2013-04-18" >>= treesTime
--   Right 2013-04-18
--
--   >>> S.toTrees "2013-#16" >>= treesTime
--   Right 2013-#16
--
treesTime :: [S.Tree] -> B.Ab T.Time
treesTime = stringsTime O.#. D.treesTexts False

-- | Get time from string.
--
--   >>> stringTime "2013-04-18 12:00"
--   Right 2013-04-18 12:00
--
--   >>> stringTime "2013-04-18 12:00 +9:00" >>= (Right . T.timeCutZone)
--   Right 2013-04-18 03:00
--
stringTime :: String -> B.Ab T.Time
stringTime = S.toTrees B.>=> treesTime

stringsTime :: [String] -> B.Ab T.Time
stringsTime = year where

    year []             = Msg.nothing
    year (cs : xs)      = case getInt cs of
                            (y, '-'  : cs')  -> mwd (toInteger y) $ cs' : xs
                            _                -> Msg.nothing

    mwd _ []                      = Msg.nothing
    mwd y (('#' : '#': cs) : xs)  = day (T.dateFromYd y) $ cs : xs
    mwd y (('#' : cs) : xs)       = week y $ cs : xs
    mwd y xs                      = month y xs

    month _ []          = Msg.nothing
    month y (cs : xs)   = case getInt cs of
                            (m, '-'  : cs')  -> day (T.dateFromYmd y m) $ cs' : xs
                            (m, "")          -> T.timeFromYmAb y m
                            _                -> Msg.nothing

    week _ []           = Msg.nothing
    week y (cs : xs)    = case getInt cs of
                            (w, '-'  : cs')  -> day (T.dateFromYwd y w) $ cs' : xs
                            (w, "")          -> T.timeFromYwAb y w
                            _                -> Msg.nothing

    day _ []            = Msg.nothing
    day date (cs : xs)  = case getInt cs of
                            (d, "") | null xs    -> do d' <- date d
                                                       Right $ T.TimeYmd d'
                                    | otherwise  -> do d' <- date d
                                                       hour (T.timeFromDczAb d') $ concat xs
                            _                    -> Msg.nothing

    hour k cs           = case getInt cs of
                            (h, "")          -> k (T.clockFromDh 0 h) Nothing
                            (h, ':' : cs')   -> minute k h cs'
                            _                -> Msg.nothing

    minute k h cs       = case getInt cs of
                            (m, "")          -> k (T.clockFromDhm 0 h m) Nothing
                            (m, ':' : cs')   -> second k h m cs'
                            (m, cs')         -> zone1 k (T.ClockPartsMin 0 h m) cs'

    second k h m cs     = case getInt cs of
                            (s, "")          -> k (T.clockFromDhms 0 h m s) Nothing
                            (s, cs')         -> zone1 k (T.ClockPartsSec 0 h m s) cs'

    zone1 k clock cs    = case cs of
                            '+' : cs'        -> zone2 k clock   1  cs'
                            '-' : cs'        -> zone2 k clock (-1) cs'
                            "UTC"            -> zone4 k clock 0 0
                            _                -> Msg.nothing

    zone2 k clock pm cs = case getInt cs of
                            (zh, ':' : cs')  -> zone3 k clock (pm * zh) cs'
                            _                -> Msg.nothing

    zone3 k clock zh cs = case getInt cs of
                            (zm, "")         -> zone4 k clock zh zm
                            _                -> Msg.nothing

    zone4 k clock zh zm =
        let z = T.secFromHms (zh, zm, 0)
            c = case clock of
                  T.ClockPartsSec  d h m s -> T.ClockPartsSec d (h - zh) (m - zm) s
                  T.ClockPartsMin  d h m   -> T.ClockPartsMin d (h - zh) (m - zm)
                  T.ClockPartsHour d h     -> T.ClockPartsMin d (h - zh) (0 - zm)
                  T.ClockPartsDays d       -> T.ClockPartsMin d (0 - zh) (0 - zm)
        in k (T.toClock c) (Just z)


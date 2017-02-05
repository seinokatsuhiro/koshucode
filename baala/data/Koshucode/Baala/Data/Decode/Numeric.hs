{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode numeric contents.

module Koshucode.Baala.Data.Decode.Numeric
  ( -- * Numeric
    treesDigits,
    tokenClock,
    stringTime, treesTime,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type                  as T
import qualified Koshucode.Baala.Data.Decode.Type      as D
import qualified Koshucode.Baala.Syntax.Pattern        as P
import qualified Koshucode.Baala.Data.Decode.Message   as Msg


-- ----------------------  Number

-- | Decode digits from token trees.
--
--   >>> S.withTrees treesDigits "-123 450.00"
--   Right "-123450.00"
--
treesDigits :: (O.Textual t) => [S.TTree t] -> B.Ab t
treesDigits = concatDigits O.#. D.treesTexts False

concatDigits :: (O.Textual t) => [t] -> B.Ab t
concatDigits = first where
    first ((O.tCut -> O.Jp c cs) : xs)
        | c `elem` ("+-0123456789o" :: String) = loop [O.charT c] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ O.tJoinAll $ reverse ss
    loop ss (w : xs) | O.tAll (`elem` ("0123456789.o" :: String)) w = loop (w : ss) xs
    loop _ _ = Msg.nothing


-- ----------------------  Clock

-- | Decode clock from token.
--
--   >>> tokenClock (head $ S.toks "|12:00|" :: S.Token)
--   Right |12:00|
--
tokenClock :: (O.Textual t) => S.TToken t -> B.Ab T.Clock
tokenClock (P.TBar (O.tCut -> O.Jp '|' w)) = textClock w
tokenClock _ = Msg.nothing

textClock :: (O.Textual t) => t -> B.Ab T.Clock
textClock = sign where
    sign (O.tCut -> O.Jp '-' cs)  = T.clockNeg <$> dayOrHour cs
    sign (O.tCut -> O.Jp '+' cs)  = dayOrHour cs
    sign cs                       = dayOrHour cs

    dayOrHour cs     = case getInt cs of
                         (d, "'|")       -> Right $ T.ClockD $ toInteger d
                         (h, "|")        -> clock T.ClockDh 0 h 0 0
                         (n, O.tCut -> O.Jp c cs')
                             | c == ':'  -> minute 0 n cs'
                             | c == '\'' -> hour (toInteger n) cs'
                         _               -> Msg.nothing

    hour d cs        = case getInt cs of
                         (h, "|")         -> clock T.ClockDh d h 0 0
                         (h, O.tCut -> O.Jp ':' cs')
                                          -> minute d h cs'
                         _                -> Msg.nothing

    minute d h cs    = case getInt cs of
                         (m, "|")         -> clock T.ClockDhm d h m 0
                         (m, O.tCut -> O.Jp ':' cs')
                                          -> second d h m cs'
                         _                -> Msg.nothing

    second d h m cs  = case getInt cs of
                         (s, "|")         -> clock T.ClockDhms d h m s
                         _                -> Msg.nothing

    clock k d h m s
        | m >= 60    = Msg.nothing
        | s >= 60    = Msg.nothing
        | otherwise  = let (d', h') = h `divMod` 24
                       in Right $ k (d + toInteger d') $ T.secFromHms (h', m, s)

-- >>> getInt "12a"
-- (12,"a")
getInt :: (O.Textual t) => t -> (Int, t)
getInt = loop 0 where
    loop n (O.tCut -> O.Jp c cs) = case fromDigit c of
                                     Just x  -> loop (10 * n + x) cs
                                     Nothing -> (n, c O.<:> cs)
    loop n t = (n, t)

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

-- | Decode time from string.
--
--   >>> stringTime "2013-04-18 12:00"
--   Right 2013-04-18 12:00
--
--   >>> stringTime "2013-04-18 12:00 +9:00"
--   Right 2013-04-18 12:00 +09:00
--
stringTime :: String -> B.Ab T.Time
stringTime = treesTime O.#. (S.toTrees :: String -> B.Ab [S.Tree])

-- | Decode time from token trees.
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
treesTime :: (S.TextualTermName t) => [S.TTree t] -> B.Ab T.Time
treesTime = textsTime O.#. D.treesTexts False

textsTime :: (O.Textual t) => [t] -> B.Ab T.Time
textsTime = year where

    -- ----------------------  year, month, week, day

    year []             = Msg.nothing
    year (cs : xs)      = case getInt cs of
                            (y, O.tCut -> O.Jp '-' cs')
                                  -> mwd (toInteger y) $ cs' : xs
                            _     -> Msg.nothing

    mwd _ []                                    = Msg.nothing
    mwd y ((O.tCut2 -> O.Jp2 '#' '#' cs) : xs)  = day (T.yearlyDate y) $ cs : xs
    mwd y ((O.tCut  -> O.Jp '#' cs) : xs)       = week y $ cs : xs
    mwd y xs                                    = month y xs

    month _ []          = Msg.nothing
    month y (cs : xs)   = case getInt cs of
                            (m, O.tCut -> O.Jp '-' cs') -> day (T.monthlyDate y m) $ cs' : xs
                            (m, "")          -> T.ymTime y m
                            _                -> Msg.nothing

    week _ []           = Msg.nothing
    week y (cs : xs)    = case getInt cs of
                            (w, O.tCut -> O.Jp '-' cs') -> day (T.weeklyDate y w) $ cs' : xs
                            (w, "")          -> T.ywTime y w
                            _                -> Msg.nothing

    day _ []            = Msg.nothing
    day date (cs : xs)  = case getInt cs of
                            (d, "") | null xs    -> do d' <- date d
                                                       Right $ T.TimeYmd d'
                                    | otherwise  -> do d' <- date d
                                                       hour (dcz d') $ O.tJoinAll xs
                            _                    -> Msg.nothing

    dcz d c (Nothing) = Right $ T.dcTime d c
    dcz d c (Just z)  = Right $ T.dczTime d c z

    -- ----------------------  hour, minute, second

    hour k cs           = case getInt cs of
                            (h, O.tCut -> O.Jp ':' cs') -> minute k h cs'
                            (h, "")          -> k (T.clockFromDh 0 h) Nothing
                            _                -> Msg.nothing

    minute k h cs       = case getInt cs of
                            (m, O.tCut -> O.Jp ':' cs') -> second k h m cs'
                            (m, "")          -> k (T.clockFromDhm 0 h m) Nothing
                            (m, cs')         -> zone1 k (T.ClockPartsMin 0 h m) cs'

    second k h m cs     = case getInt cs of
                            (s, "")          -> k (T.clockFromDhms 0 h m s) Nothing
                            (s, cs')         -> zone1 k (T.ClockPartsSec 0 h m s) cs'

    -- ----------------------  time zone

    zone1 k clock cs    = case cs of
                            (O.tCut -> O.Jp '+' cs') -> zone2 k clock   1  cs'
                            (O.tCut -> O.Jp '-' cs') -> zone2 k clock (-1) cs'
                            "UTC"            -> zone4 k clock 0 0
                            _                -> Msg.nothing

    zone2 k clock pm cs = case getInt cs of
                            (zh, O.tCut -> O.Jp ':' cs')  -> zone3 k clock (pm * zh) cs'
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


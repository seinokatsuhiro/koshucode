{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Utilities for token trees.

module Koshucode.Baala.Core.Content.Tree
  ( treesToTokens,
    treesToTexts,
    treeToText,
    treesToDigits,
    tokenClock,
    treesToTime,
    treeToFlatTerm,
    treesToTerms,
    treesToTerms1,
    treesToInterp,
    -- $Function
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core.Message  as Msg

-- $Function
--
--  /Example/
--
--  >>> treesToTerms =<< B.tt "/a 'A3 /b 10"
--  Right [("/a", [TreeL ...]),
--         ("/b", [TreeL ...])]
--


-- ----------------------  Token

treesToTokens :: B.TTreesToAb [B.Token]
treesToTokens = mapM token where
    token (B.TreeL t) = Right t
    token _ = Msg.adlib "not token"


-- ----------------------  Text

treesToTexts :: Bool -> B.TTreesToAb [String]
treesToTexts q = loop [] where
    loop ss [] = Right $ reverse ss
    loop ss (B.TreeL x : xs) = do s <- tokenToText q x
                                  loop (s : ss) xs
    loop _ _ = Msg.nothing

treeToText :: Bool -> B.TTreeToAb String
treeToText q (B.TreeL tok) = tokenToText q tok
treeToText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenToText :: Bool -> B.Token -> B.Ab String
tokenToText True  (B.TText _ q w) | q > B.TextRaw  = Right w
tokenToText False (B.TTextRaw _ w)                 = Right w
tokenToText _ _  =  Msg.nothing

treesToDigits :: B.TTreesToAb String
treesToDigits = concatDigits B.<=< treesToTexts False

concatDigits :: [String] -> B.Ab String
concatDigits = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.") w = loop (w:ss) xs
    loop _ _ = Msg.nothing


-- ----------------------  Clock

tokenClock :: B.Token -> B.Ab B.Clock
tokenClock (B.TTextBar _ ('|' : w)) = textClock w
tokenClock _ = Msg.nothing

textClock :: String -> B.Ab B.Clock
textClock = sign where
    sign ('-' : cs)  = B.clockNeg `fmap` dayOrHour cs
    sign ('+' : cs)  = dayOrHour cs
    sign cs          = dayOrHour cs

    dayOrHour cs     = case getInt cs of
                         (h, ':'  : cs')  ->  minute 0 h cs'
                         (d, "'|")        ->  Right $ B.ClockD $ toInteger d
                         (h, "|")         ->  clock B.ClockDh 0 h 0 0
                         (d, '\'' : cs')  ->  hour (toInteger d) cs'
                         _                ->  Msg.nothing

    hour d cs        = case getInt cs of
                         (h, ':' : cs')   ->  minute d h cs'
                         (h, "|")         ->  clock B.ClockDh d h 0 0
                         _                ->  Msg.nothing

    minute d h cs    = case getInt cs of
                         (m, ':' : cs')   ->  second d h m cs'
                         (m, "|")         ->  clock B.ClockDhm d h m 0
                         _                ->  Msg.nothing

    second d h m cs  = case getInt cs of
                         (s, "|")         ->  clock B.ClockDhms d h m s
                         _                ->  Msg.nothing

    clock k d h m s
        | m >= 60    = Msg.nothing
        | s >= 60    = Msg.nothing
        | otherwise  = let (d', h') = h `divMod` 24
                       in Right $ k (d + toInteger d') $ B.secFromHms (h', m, s)

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

treesToTime :: B.TTreesToAb B.Time
treesToTime = concatTime B.<=< treesToTexts False

concatTime :: [String] -> B.Ab B.Time
concatTime = year where

    year []             = Msg.nothing
    year (cs : xs)      = case getInt cs of
                            (y, '-'  : cs')  -> mwd (toInteger y) $ cs' : xs
                            _                -> Msg.nothing

    mwd _ []                      = Msg.nothing
    mwd y (('#' : '#': cs) : xs)  = day (B.dateFromYdAb y) $ cs : xs
    mwd y (('#' : cs) : xs)       = week y $ cs : xs
    mwd y xs                      = month y xs

    month _ []          = Msg.nothing
    month y (cs : xs)   = case getInt cs of
                            (m, '-'  : cs')  -> day (B.dateFromYmdAb y m) $ cs' : xs
                            (m, "")          -> B.timeFromYmAb y m
                            _                -> Msg.nothing

    week _ []           = Msg.nothing
    week y (cs : xs)    = case getInt cs of
                            (w, '-'  : cs')  -> day (B.dateFromYwdAb y w) $ cs' : xs
                            (w, "")          -> B.timeFromYwAb y w
                            _                -> Msg.nothing

    day _ []            = Msg.nothing
    day date (cs : xs)  = case getInt cs of
                            (d, "") | null xs    -> do d' <- date d
                                                       Right $ B.TimeYmd d'
                                    | otherwise  -> do d' <- date d
                                                       hour (B.timeFromDczAb d') $ concat xs
                            _                    -> Msg.nothing

    hour k cs           = case getInt cs of
                            (h, "")          -> k (B.clockFromDh 0 h) Nothing
                            (h, ':' : cs')   -> minute k h cs'
                            _                -> Msg.nothing

    minute k h cs       = case getInt cs of
                            (m, "")          -> k (B.clockFromDhm 0 h m) Nothing
                            (m, ':' : cs')   -> second k h m cs'
                            (m, cs')         -> zone1 k h m Nothing cs'

    second k h m cs     = case getInt cs of
                            (s, "")          -> k (B.clockFromDhms 0 h m s) Nothing
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

    zone4 k h m s zh zm = let c = B.clockFromHms (h - zh) (m - zm) s
                              z = B.secFromHms (zh, zm, 0)
                          in k c $ Just z


-- ----------------------  Term

-- | Get flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
treeToFlatTerm :: B.TTreeToAb B.TermName
treeToFlatTerm (B.TermLeaf _ 0 [n])  = Right n
treeToFlatTerm (B.TreeL t)           = Msg.reqFlatName t
treeToFlatTerm _                     = Msg.reqTermName

-- | Convert token trees into a list of named token trees.
treesToTerms :: B.TTreesToAb [B.NamedTrees]
treesToTerms = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- treeToFlatTerm x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: B.TTreesTo ([B.TTree], [B.TTree])
    cont xs@(B.TermLeaf _ 0 _ : _)  = ([], xs)
    cont []                         = ([], [])
    cont (x : xs)                   = B.cons1 x $ cont xs

treesToTerms1 :: B.TTreesToAb [B.NamedTree]
treesToTerms1 xs = do xs' <- treesToTerms xs
                      Right $ B.mapSndTo B.wrapTrees xs'


-- ----------------------  Interp

treesToInterp :: B.TTreesToAb B.Interp
treesToInterp = Right . B.interp B.<=< mapM treeToInterpWord

treeToInterpWord :: B.TTreeToAb B.InterpWord
treeToInterpWord (B.TreeB _ _ _) = Msg.nothing
treeToInterpWord (B.TreeL x) =
    case x of
      B.TText _ _ w    ->  Right $ B.InterpText w
      B.TTerm _ _ [n]  ->  Right $ B.InterpTerm n
      _                ->  Msg.nothing

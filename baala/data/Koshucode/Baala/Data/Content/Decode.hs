{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode token trees to contents.

module Koshucode.Baala.Data.Content.Decode
  ( -- * Functions
    ContentCons, ContentCalc, 
    contentCons, treesJudge,

    -- * Assert type
    AssertType (..),
    assertAs,
    assertSymbol,

    -- * Document
  
    -- ** Simple data
    -- $SimpleData
  
    -- ** Compound data
    -- $CompoundData
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data.Type               as D
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Content.Tree       as D
import qualified Koshucode.Baala.Base.Message            as Msg
import qualified Koshucode.Baala.Data.Content.Message    as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  General content

pattern LName s    <- L (S.TTermN _ _ s)
pattern Text f s   <- S.TText _ f s

-- | Content constructor.
type ContentCons c = S.TTree -> B.Ab c

-- | Content calculator.
type ContentCalc c = S.TTree -> B.Ab c

-- | Convert token tree into internal form of content.
contentCons :: forall c. (D.CContent c) => ContentCalc c -> ContentCons c
contentCons calc tree = Msg.abLiteral tree $ cons tree where
    cons :: ContentCons c
    cons x@(L tok)
        = eithcon (eithcon (eithcon (token tok)
            D.putClock  $ D.tokenClock tok)
            D.putTime   $ D.treesTime   [x])
            decimal     $ D.treesDigits [x]
    cons g@(B b xs) = case b of
        S.BracketGroup   -> group g xs
        S.BracketList    -> D.putList   =<< consContents cons xs
        S.BracketSet     -> D.putSet    =<< consContents cons xs
        S.BracketTie     ->                 consAngle    cons xs
        S.BracketRel     -> D.putRel    =<< consRel      cons xs
        S.BracketType    -> D.putType   =<< consType          xs
        S.BracketInterp  -> D.putInterp =<< D.treesInterp     xs
        _                -> Msg.unkBracket
    cons _ = B.bug "contentCons"

    token :: S.Token -> B.Ab c
    token (Text n w)
        | n <= S.TextRaw     = keyword w
        | n == S.TextQ       = D.putCode w
        | otherwise          = D.putText w
    token (S.TTermN _ _ n)   = D.putTerm n
    token (S.TTerm _ _ [n])  = D.putTerm n
    token t                  = Msg.unkWord $ S.tokenContent t

    group g xs@(LText f _ : _)
        | f == S.TextRaw   = eithcon (eith g
                               D.putTime $ D.treesTime   xs)
                               decimal   $ D.treesDigits xs
    group _ []             = Right D.empty
    group g _              = calc g

    eithcon f      = either (const f)
    eith g         = either (const $ calc g)
    decimal        = D.putDec B.<=< D.decodeDecimal

    keyword :: String -> B.Ab c
    keyword "(+)"  = Right D.true
    keyword "(-)"  = Right D.false
    keyword "(/)"  = Right D.end
    keyword "0"    = Right D.false  -- obsolete
    keyword "1"    = Right D.true   -- obsolete
    keyword "dum"  = Right D.dum
    keyword "dee"  = Right D.dee
    keyword w      = Msg.unkWord w

-- List of contents
--
--   { 0 | 1 | 2 }
--   [ 0 | 1 | 2 ]
--     .........
--     :
--     consContents
--
consContents :: (D.CContent c) => ContentCons c -> [S.TTree] -> B.Ab [c]
consContents _   [] = Right []
consContents cons cs = lt `mapM` S.divideTreesByBar cs where
    lt []   = Right D.empty
    lt [x]  = cons x
    lt xs   = cons $ B.TreeB S.BracketGroup Nothing xs

-- Contents enclosed in angle brackets
--
--   << /a 1  /b 2  /c 3 >>   << words "a b c" >>
--      ................         .............
--      :                        :
--      consAngle                consAngle
--
consAngle :: (D.CContent c) => ContentCons c -> [S.TTree] -> B.Ab c
consAngle cons xs@(LName _ : _) = D.putTie =<< consTie cons xs
consAngle _ [] = D.putTie []
consAngle _ [LRaw "words", LQq ws] = D.putList $ map D.pText $ words ws
consAngle _ _ = Msg.adlib "unknown angle bracket"

-- Tie
--
--   {- /a 1  /b 2  /c 3 -}
--      ................
--      :
--      consTie
--
consTie :: (D.CContent c) => ContentCons c -> [S.TTree] -> B.Ab [S.Term c]
consTie cons = mapM p B.<=< D.treesTerms1 where
    p (name, tree) = Right . (name,) =<< cons tree

-- | Decode token trees into a judge.
--   Judges itself are not content type.
--   It can be only used in the top-level of resources.
treesJudge :: (D.CContent c) => ContentCalc c -> AssertType -> D.JudgeClass -> [S.TTree] -> B.Ab (D.Judge c)
treesJudge calc q p = Right . assertAs q p B.<=< consTie (contentCons calc)


-- ----------------------  Relation
--
--        .............  consRel  ..............
--        :                                    :
--     {= /a /b /c  [ 0 | 1 | 2 ]  [ 3 | 4 | 5 ] =}
--        ........  .............  .............
--        :         :              :
--        :         consRelTuple   consRelTuple
--        consTermNames
--
--
consRel :: (D.CContent c) => ContentCons c -> [S.TTree] -> B.Ab (D.Rel c)
consRel cons xs =
    do bo <- consRelTuple cons n `mapM` xs'
       Right $ D.Rel he $ B.unique bo
    where
      (ns, xs')  = consTermNames xs
      n          = length ns
      he         = D.headFrom ns

consTermNames :: [S.TTree] -> ([S.TermName], [S.TTree])
consTermNames = terms [] where
    terms ns (LName n : xs) = terms (n : ns) xs
    terms ns xs = (reverse ns, xs)

consRelTuple :: (D.CContent c) => ContentCons c -> Int -> S.TTree -> B.Ab [c]
consRelTuple cons n g@(B S.BracketList xs) =
    do cs <- consContents cons xs
       let n' = length cs
       B.when (n /= n') $ Msg.abLiteral g $ Msg.oddRelation n n'
       Right cs
consRelTuple _ _ g = Msg.abLiteral g $ Msg.reqRelTuple


-- ----------------------  Assert type

-- | Type of assertions.
data AssertType
    = AssertAffirm       -- ^ @|==@ /C/ @:@ /R/ generates affirmative judges.
    | AssertDeny         -- ^ @|=x@ /C/ @:@ /R/ generates denial judges.
    | AssertMultiDeny    -- ^ @|=xx@ /C/ @:@ /R/ generates multiple-denial judges.
    | AssertChange       -- ^ @|=c@ /C/ @:@ /R/ generates changement judges.
    | AssertMultiChange  -- ^ @|=cc@ /C/ @:@ /R/ generates multiple-changement judges.
    | AssertViolate      -- ^ @|=v@ /C/ @:@ /R/ generates violation judges.
      deriving (Show, Eq, Ord)

-- | Frege's stroke and various assertion lines.
assertSymbol :: AssertType -> String
assertSymbol AssertAffirm       = "|=="
assertSymbol AssertDeny         = "|=X"
assertSymbol AssertMultiDeny    = "|=XX"
assertSymbol AssertChange       = "|=C"
assertSymbol AssertMultiChange  = "|=CC"
assertSymbol AssertViolate      = "|=V"

-- | Create judgement corresponding to assertion type.
assertAs :: AssertType -> D.JudgeOf c
assertAs AssertAffirm        = D.JudgeAffirm
assertAs AssertDeny          = D.JudgeDeny
assertAs AssertMultiDeny     = D.JudgeMultiDeny
-- assertAs AssertChange        = D.JudgeChange
-- assertAs AssertMultiChange   = D.JudgeMultiChange
assertAs AssertViolate       = D.JudgeViolate


-- ----------------------  Type

-- | Literal reader for types.
consType :: [S.TTree] -> B.Ab D.Type
consType = gen where
    gen xs = case S.divideTreesByBar xs of
               [x] ->  single x
               xs2 ->  Right . D.TypeSum =<< mapM gen xs2

    single [B _ xs]          = gen xs
    single (LText f n : xs)
        | f == S.TextRaw     = dispatch n xs
        | otherwise          = Msg.quoteType n
    single []                = Right $ D.TypeSum []
    single _                 = Msg.unkType ""

    precision ws [LRaw w] | w `elem` ws = Right $ Just w
    precision _ []  = Right Nothing
    precision _ _   = Msg.unkType "precision"

    clock = ["sec", "min", "hour", "day"]
    time  = "month" : clock

    dispatch "any"     _    = Right D.TypeAny
    dispatch "empty"   _    = Right D.TypeEmpty
    dispatch "boolean" _    = Right D.TypeBool
    dispatch "text"    _    = Right D.TypeText
    dispatch "code"    _    = Right D.TypeCode
    dispatch "decimal" _    = Right D.TypeDec
    dispatch "clock"   xs   = Right . D.TypeClock  =<< precision clock xs
    dispatch "time"    xs   = Right . D.TypeTime   =<< precision time  xs
    dispatch "binary"  _    = Right D.TypeBin
    dispatch "term"    _    = Right D.TypeTerm
    dispatch "type"    _    = Right D.TypeType
    dispatch "interp"  _    = Right D.TypeInterp

    dispatch "tag"   xs     = case xs of
                                [tag, colon, typ]
                                    | D.treeText False colon == Right ":"
                                      -> do tag' <- D.treeText False tag
                                            typ' <- gen [typ]
                                            Right $ D.TypeTag tag' typ'
                                _   -> Msg.unkType "tag"
    dispatch "list"  xs     = Right . D.TypeList =<< gen xs
    dispatch "set"   xs     = Right . D.TypeSet  =<< gen xs
    dispatch "tuple" xs     = do ts <- mapM (gen. B.li1) xs
                                 Right $ D.TypeTuple ts
    dispatch "tie"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ D.TypeTie ts2
    dispatch "rel"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ D.TypeRel ts2
    dispatch n _            = Msg.unkType n


-- ------------------------------------------------------------------
-- $SimpleData
--
--  Prepere some definitions.
--
--  >>> :m +Koshucode.Baala.Op.Vanilla.Type
--  >>> let trees = B.ttrees . B.tokens
--  >>> let lit  = contentCons [] :: S.TTree -> B.Ab BaalaC
--  >>> let lits = consContents lit . trees
--
--  Boolean.
--
--    >>> lits "#true : #false"
--    Right [VBool True, VBool False]
--
--  Words.
--
--    >>> lits "'a : 'b #sp 'c"
--    Right [VText "a", VText "b c"]
--
--  Decimal.
--
--    >>> lits "12.0"
--    Right [VDec (Decimal (120, 10), 1, False)]
--
--  Empty as no ordinary value.
--
--    >>> lits "()"
--    Right [VEmpty]
--

-- ------------------------------------------------------------------
-- $CompoundData
--
--  Set.
--
--    >>> lits "{ 'b | 'a | 'a | 'c | 'a }"
--    Right [VSet [VText "b", VText "a", VText "c"]]
--
--  List.
--
--    >>> lits "[ 'a | '10 | 20 ]"
--    Right [VList [VText "a", VText "10", VDec (Decimal (20, 1), 0, False)]]
--
--  Tie.
--
--    >>> lits "{- /a 'x  /b { 'y | 'z } -}"
--    Right [VTie 
--      [ ("/a", VText "x")
--      , ("/b", VSet [VText "y", VText "z"])]]
--
--  Relation.
--
--    >>> lits "{= /a /x [ 'A1 | 20 ][ 'A3 | 40 ][ 'A4 | 60 ] =}"
--    Right [VRel (Rel
--      (Relhead [Term "/a", Term "/x"]),
--      [ [VText "A1", VDec (Decimal (20,1), 0, False)]
--      , [VText "A3", VDec (Decimal (40,1), 0, False)]
--      , [VText "A4", VDec (Decimal (60,1), 0, False)] ])]
--


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode contents from token trees.

module Koshucode.Baala.Data.Content.Decode
  ( -- * Functions
    DecodeContent, CalcContent, 
    stringContent,
    treeContent, treesJudge,

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
import qualified Koshucode.Baala.Data.Content.Numeric    as D
import qualified Koshucode.Baala.Data.Content.Term       as D
import qualified Koshucode.Baala.Data.Content.Tree       as D
import qualified Koshucode.Baala.Base.Message            as Msg
import qualified Koshucode.Baala.Data.Content.Message    as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  General content

pattern LName s    <- L (S.TTermN _ _ s)
pattern Text f s   <- S.TText _ f s

-- | Content decoder.
type DecodeContent c = S.TTree -> B.Ab c

-- | Content calculator.
type CalcContent c = S.TTree -> B.Ab c

-- | Decode content from string.
stringContent :: (D.CContent c) => String -> B.Ab c
stringContent = S.tt1 B.>=> treeContent undefined

-- | Decode content from token tree.
treeContent :: forall c. (D.CContent c) => CalcContent c -> DecodeContent c
treeContent calc tree = Msg.abLiteral tree $ cons tree where
    cons :: DecodeContent c
    cons x@(L tok)
        = eithcon (eithcon (eithcon (token tok)
            D.putClock  $ D.tokenClock tok)
            D.putTime   $ D.treesTime   [x])
            decimal     $ D.treesDigits [x]
    cons g@(B b xs) = case b of
        S.BracketGroup   -> group g xs
        S.BracketList    -> D.putList   =<< treesContents cons xs
        S.BracketSet     -> D.putSet    =<< treesContents cons xs
        S.BracketTie     ->                 treesTie      cons xs
        S.BracketRel     -> D.putRel    =<< treesRel      cons xs
        S.BracketType    -> D.putType   =<< D.treesType        xs
        S.BracketInterp  -> D.putInterp =<< D.treesInterp      xs
        _                -> Msg.unkBracket
    cons _ = B.bug "treeContent"

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
--     treesContents
--
treesContents :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab [c]
treesContents _   [] = Right []
treesContents cons cs = lt `mapM` S.divideTreesByBar cs where
    lt []   = Right D.empty
    lt [x]  = cons x
    lt xs   = cons $ B.TreeB S.BracketGroup Nothing xs

-- Contents enclosed in angle brackets
--
--   {- /a 1  /b 2  /c 3 -}   << words "a b c" >>
--      ................         .............
--      :                        :
--      treesTie                 treesTie
--
treesTie :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab c
treesTie cons xs@(LName _ : _) = D.putTie =<< treesTerms cons xs
treesTie _ [] = D.putTie []
treesTie _ [LRaw "words", LQq ws] = D.putList $ map D.pText $ words ws
treesTie _ _ = Msg.adlib "unknown angle bracket"

-- Terms
--
--   /a 1  /b 2  /c 3
--   ................
--   :
--   treesTerms
--
treesTerms :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab [S.Term c]
treesTerms cons = mapM p B.<=< D.treesTerms1 where
    p (name, tree) = Right . (name,) =<< cons tree

-- | Decode judge from token trees.
--   Judges itself are not content type.
--   It can be only used in the top-level of resources.
treesJudge ::
    (D.CContent c)
    => CalcContent c      -- ^ 
    -> AssertType         -- ^ Assertion type
    -> D.JudgeClass       -- ^ Judgement class
    -> [S.TTree]          -- ^ Trees of terms
    -> B.Ab (D.Judge c)   -- ^ Error or decoded judgement
treesJudge calc q p = Right . assertAs q p B.<=< treesTerms (treeContent calc)


-- ----------------------  Relation
--
--        .............  treesRel  .............
--        :                                    :
--     {= /a /b /c  [ 0 | 1 | 2 ]  [ 3 | 4 | 5 ] =}
--        ........  .............  .............
--        :         :              :
--        :         treeTuple      treeTuple
--        treesTermNames
--
--
treesRel :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab (D.Rel c)
treesRel cons xs =
    do bo <- treeTuple cons n `mapM` xs'
       Right $ D.Rel he $ B.unique bo
    where
      (ns, xs')  = treesTermNames xs
      n          = length ns
      he         = D.headFrom ns

treesTermNames :: [S.TTree] -> ([S.TermName], [S.TTree])
treesTermNames = terms [] where
    terms ns (LName n : xs) = terms (n : ns) xs
    terms ns xs = (reverse ns, xs)

treeTuple :: (D.CContent c) => DecodeContent c -> Int -> S.TTree -> B.Ab [c]
treeTuple cons n g@(B S.BracketList xs) =
    do cs <- treesContents cons xs
       let n' = length cs
       B.when (n /= n') $ Msg.abLiteral g $ Msg.oddRelation n n'
       Right cs
treeTuple _ _ g = Msg.abLiteral g $ Msg.reqRelTuple


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


-- ------------------------------------------------------------------
-- $SimpleData
--
--  Prepere some definitions.
--
--  >>> :m +Koshucode.Baala.Op.Vanilla.Type
--  >>> let trees = B.ttrees . B.tokens
--  >>> let lit  = treeContent [] :: S.TTree -> B.Ab BaalaC
--  >>> let lits = treesContents lit . trees
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


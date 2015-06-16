{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Make literal contents from token tree.

module Koshucode.Baala.Core.Content.Literal
  ( -- * Functions
    ContentCons, ContentCalc, 
    contentCons, treesToJudge,

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

import qualified Data.Generics                       as G
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Content.Class  as C
import qualified Koshucode.Baala.Core.Content.Tree   as C
import qualified Koshucode.Baala.Core.Message        as Msg



-- ----------------------  General content

-- | Content constructor.
type ContentCons c = B.TTreeToAb c

-- | Content calculator.
type ContentCalc c = B.TTreeToAb c

-- | Convert token tree into internal form of content.
contentCons :: forall c. (C.CContent c) => ContentCalc c -> ContentCons c
contentCons calc tree = Msg.abLiteral tree $ cons tree where
    cons :: ContentCons c
    cons x@(B.TreeL t)
        = eithcon (eithcon (eithcon (token t)
            C.putClock  $ C.tokenClock t)
            C.putTime   $ C.treesToTime   [x])
            decimal     $ C.treesToDigits [x]
    cons g@(B.TreeB b _ xs) = case b of
        B.BracketGroup   -> group g
        B.BracketList    -> C.putList   =<< consContents cons xs
        B.BracketSet     -> C.putSet    =<< consContents cons xs
        B.BracketAssn    ->                 consAngle    cons xs
        B.BracketRel     -> C.putRel    =<< consRel      cons xs
        B.BracketType    -> C.putType   =<< consType          xs
        B.BracketInterp  -> C.putInterp =<< C.treesToInterp   xs
        _                -> Msg.unkBracket

    token :: B.Token -> B.Ab c
    token (B.TText _ n w) | n <= B.TextRaw  = keyword w
    token (B.TText _ _ w)                   = C.putText w
    token (B.TTermN _ n)                    = C.putTerm n
    token (B.TTerm _ _ [n])                 = C.putTerm n
    token t                                 = Msg.unkWord $ B.tokenContent t

    group :: B.TTreeToAb c
    group g@(B.TreeB _ _ xs@(B.TextLeaf f _ _ : _))
        | f  > B.TextRaw    = eith g text $ C.treesToTexts True xs
        | f == B.TextRaw    = eithcon (eith g
                                C.putTime $ C.treesToTime   xs)
                                decimal   $ C.treesToDigits xs
    group (B.TreeB _ _ [])  = Right C.empty
    group g                 = calc g

    eithcon f    = either (const f)
    eith g       = either (const $ calc g)
    text         = C.putText . concat
    decimal      = C.putDec B.<=< B.litDecimal

    keyword :: (C.CEmpty c, C.CBool c) => String -> B.Ab c
    keyword "0"    = Right C.false
    keyword "1"    = Right C.true
    keyword "dum"  = Right C.dum
    keyword "dee"  = Right C.dee
    keyword w      = Msg.unkWord w

-- List of contents
--
--   { 0 | 1 | 2 }
--   [ 0 | 1 | 2 ]
--     .........
--     :
--     consContents

consContents :: (C.CContent c) => ContentCons c -> B.TTreesToAb [c]
consContents _   [] = Right []
consContents cons cs = lt `mapM` B.divideTreesByBar cs where
    lt []   = Right C.empty
    lt [x]  = cons x
    lt xs   = cons $ B.TreeB B.BracketGroup Nothing xs

-- Contents enclosed in angle brackets
--
--   << /a 1  /b 2  /c 3 >>   << words "a b c" >>
--      ................         .............
--      :                        :
--      consAngle                consAngle

consAngle :: (C.CContent c) => ContentCons c -> B.TTreesToAb c
consAngle cons xs@(B.TermLeafName _ _ : _) = C.putAssn =<< consAssn cons xs
consAngle cons xs@(B.TermLeafPath _ _ : _) = C.putAssn =<< consAssn cons xs
consAngle _ [] = C.putAssn []
consAngle _ [B.TextLeafRaw _ "words", B.TextLeafQQ _ ws] = C.putList $ map C.pText $ words ws
consAngle _ _ = Msg.adlib "unknown angle bracket"

-- Association
--
--   << /a 1  /b 2  /c 3 >>
--      ................
--      :
--      consAssn

consAssn :: (C.CContent c) => ContentCons c -> B.TTreesToAb [B.Named c]
consAssn cons = mapM p B.<=< C.treesToTerms1 where
    p (name, tree) = Right . (name,) =<< cons tree

-- | Convert token trees into a judge.
--   Judges itself are not content type.
--   It can be only used in the top-level of resources.
treesToJudge :: (C.CContent c) => ContentCalc c -> AssertType -> B.JudgePat -> B.TTreesToAb (B.Judge c)
treesToJudge calc q p = Right . assertAs q p B.<=< consAssn (contentCons calc)


-- ----------------------  Relation
--
--        .............  consRel  ..............
--        :                                    :
--     {| /a /b /c  [ 0 | 1 | 2 ]  [ 3 | 4 | 5 ] |}
--        ........  .............  .............
--        :         :              :
--        :         consRelTuple   consRelTuple
--        consTermNames
--

consRel :: (C.CContent c) => ContentCons c -> B.TTreesToAb (B.Rel c)
consRel cons xs =
    do bo <- consRelTuple cons n `mapM` xs'
       Right $ B.Rel he $ B.unique bo
    where
      (ns, xs')  = consTermNames xs
      n          = length ns
      he         = B.headFrom ns

consTermNames :: B.TTreesTo ([B.TermName], [B.TTree])
consTermNames = terms [] where
    terms ns (B.TermLeafName _ n   : xs) = terms (n : ns) xs
    terms ns (B.TermLeafPath _ [n] : xs) = terms (n : ns) xs
    terms ns xs = (reverse ns, xs)

consRelTuple :: (C.CContent c) => ContentCons c -> Int -> B.TTreeToAb [c]
consRelTuple cons n g@(B.TreeB B.BracketList _ xs) =
    do cs <- consContents cons xs
       let n' = length cs
       B.when (n /= n') $ Msg.abLiteral g $ Msg.oddRelation n n'
       Right cs
consRelTuple _ _ g = Msg.abLiteral g $ Msg.reqRelTuple


-- ----------------------  Assert type

data AssertType
    = AssertAffirm       -- ^ @|==@ /pattern/ @:@ /relmap/
    | AssertDeny         -- ^ @|=x@ /pattern/ @:@ /relmap/
    | AssertMultiDeny    -- ^ @|=xx@ /pattern/ @:@ /relmap/
    | AssertChange       -- ^ @|=c@ /pattern/ @:@ /relmap/
    | AssertMultiChange  -- ^ @|=cc@ /pattern/ @:@ /relmap/
    | AssertViolate      -- ^ @|=v@ /pattern/ @:@ /relmap/
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

assertSymbol :: AssertType -> String
assertSymbol AssertAffirm       = "|=="
assertSymbol AssertDeny         = "|=X"
assertSymbol AssertMultiDeny    = "|=XX"
assertSymbol AssertChange       = "|=C"
assertSymbol AssertMultiChange  = "|=CC"
assertSymbol AssertViolate      = "|=V"

assertAs :: AssertType -> B.JudgeOf c
assertAs AssertAffirm        = B.JudgeAffirm
assertAs AssertDeny          = B.JudgeDeny
assertAs AssertMultiDeny     = B.JudgeMultiDeny
-- assertAs AssertChange        = B.JudgeChange
-- assertAs AssertMultiChange   = B.JudgeMultiChange
assertAs AssertViolate       = B.JudgeViolate


-- ----------------------  Type

-- | Literal reader for types.
consType :: B.TTreesToAb B.Type
consType = gen where
    gen xs = case B.divideTreesByBar xs of
               [x] ->  single x
               xs2 ->  Right . B.TypeSum =<< mapM gen xs2

    single [B.TreeB _ _ xs]  = gen xs
    single (B.TextLeaf f _ n : xs)
        | f == B.TextRaw     = dispatch n xs
        | otherwise          = Msg.quoteType n
    single []                = Right $ B.TypeSum []
    single _                 = Msg.unkType ""

    precision ws [B.TextLeafRaw _ w] | w `elem` ws = Right $ Just w
    precision _ []  = Right Nothing
    precision _ _   = Msg.unkType "precision"

    clock = ["sec", "min", "hour", "day"]
    time  = "month" : clock

    dispatch "any"     _    = Right B.TypeAny
    dispatch "empty"   _    = Right B.TypeEmpty
    dispatch "boolean" _    = Right B.TypeBool
    dispatch "text"    _    = Right B.TypeText
    dispatch "code"    _    = Right B.TypeCode
    dispatch "decimal" _    = Right B.TypeDec
    dispatch "clock"   xs   = Right . B.TypeClock  =<< precision clock xs
    dispatch "time"    xs   = Right . B.TypeTime   =<< precision time  xs
    dispatch "binary"  _    = Right B.TypeBin
    dispatch "term"    _    = Right B.TypeTerm
    dispatch "type"    _    = Right B.TypeType
    dispatch "interp"  _    = Right B.TypeInterp

    dispatch "tag"   xs     = case xs of
                                [tag, colon, typ]
                                    | C.treeToText False colon == Right ":"
                                      -> do tag' <- C.treeToText False tag
                                            typ' <- gen [typ]
                                            Right $ B.TypeTag tag' typ'
                                _   -> Msg.unkType "tag"
    dispatch "list"  xs     = Right . B.TypeList =<< gen xs
    dispatch "set"   xs     = Right . B.TypeSet  =<< gen xs
    dispatch "tuple" xs     = do ts <- mapM (gen. B.li1) xs
                                 Right $ B.TypeTuple ts
    dispatch "assn"  xs     = do ts1 <- C.treesToTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ B.TypeAssn ts2
    dispatch "rel"   xs     = do ts1 <- C.treesToTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ B.TypeRel ts2
    dispatch n _            = Msg.unkType n


-- ------------------------------------------------------------------
-- $SimpleData
--
--  Prepere some definitions.
--
--  >>> :m +Koshucode.Baala.Op.Vanilla.Type
--  >>> let trees = B.ttrees . B.tokens
--  >>> let lit  = contentCons [] :: B.TTree -> B.Ab VContent
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
--    >>> lits "{ 'b : 'a : 'a : 'c : 'a }"
--    Right [VSet [VText "b", VText "a", VText "c"]]
--
--  List.
--
--    >>> lits "[ 'a : '10 : 20 ]"
--    Right [VList [VText "a", VText "10", VDec (Decimal (20, 1), 0, False)]]
--
--  Assn.
--
--    >>> lits "<| /a 'x  /b { 'y : 'z } |>"
--    Right [VAssn
--      [ ("/a", VText "x")
--      , ("/b", VSet [VText "y", VText "z"])]]
--
--  Relation.
--
--    >>> lits "{| /a : /x | 'A1 : 20 | 'A3 : 40 | 'A4 : 60 |}"
--    Right [VRel (Rel
--      (Relhead [Term "/a", Term "/x"]),
--      [ [VText "A1", VDec (Decimal (20,1), 0, False)]
--      , [VText "A3", VDec (Decimal (40,1), 0, False)]
--      , [VText "A4", VDec (Decimal (60,1), 0, False)] ])]
--


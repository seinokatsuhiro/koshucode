{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Make literal contents from token tree.

module Koshucode.Baala.Core.Content.Literal
(
  -- * Functions
  CalcContent,
  literal,
  getJudge,
  getTermedTrees,
  -- $Function

  -- * Document

  -- ** Types
  -- $Types

  -- ** Simple data
  -- $SimpleData

  -- ** Compound data
  -- $CompoundData
) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Core.Content.Class  as C
import qualified Koshucode.Baala.Core.Message        as Msg


-- ----------------------  General content

type CalcContent c = B.TTreeToAb c

-- | Convert 'B.TTree' into internal form of content.
literal :: forall c. (C.CContent c) => CalcContent c -> B.TTreeToAb c
literal calc tree = Msg.abLiteral tree $ lit tree where
    lit :: B.TTreeToAb c
    lit x@(B.TreeL t)
        = either (const $ token t) decimal $ getDecimalText [x]
    lit g@(B.TreeB b _ xs) = case b of
        B.BracketGroup   ->  group g
        B.BracketList    ->  C.putList   =<< literals lit xs
        B.BracketSet     ->  C.putSet    =<< literals lit xs
        B.BracketAssn    ->                  litAngle lit xs
        B.BracketRel     ->  C.putRel    =<< litRel   lit xs
        B.BracketType    ->  C.putType   =<< litType  xs
        B.BracketInterp  ->  C.putInterp =<< getInterp xs
        _                ->  Msg.unkBracket

    token :: B.Token -> B.Ab c
    token (B.TText _ n w) | n <= 0  =  getKeyword w
    token (B.TText _ _ w)           =  C.putText w
    token (B.TTerm _ _ [n])         =  C.putTerm n
    token t                         =  Msg.unkWord $ B.tokenContent t

    group :: B.TTreeToAb c
    group g@(B.TreeB _ _ xs@(B.TreeL (B.TText _ n _) : _))
        | n > 0   =  eith g text    $ getTexts True xs
        | n == 0  =  eith g decimal $ getDecimalText xs
    group (B.TreeB _ _ [])  = Right C.empty
    group g                 = calc g

    eith g   =  either (const $ calc g)
    text     =  C.putText . concat
    decimal  =  C.putDec B.<=< B.litDecimal

literals :: (C.CContent c) => B.TTreeToAb c -> B.TTreesToAb [c]
literals _   [] = Right []
literals lit cs = lt `mapM` B.divideTreesByColon cs where
    lt []  =  Right C.empty
    lt [x] =  lit x
    lt xs  =  lit $ B.TreeB B.BracketGroup Nothing xs

getKeyword :: (C.CEmpty c, C.CBool c) => String -> B.Ab c
getKeyword "0"  =  Right C.false
getKeyword "1"  =  Right C.true
getKeyword w    =  Msg.unkWord w

getDecimalText :: B.TTreesToAb String
getDecimalText = getDecimalText2 B.<=< getTexts False

getDecimalText2 :: [String] -> B.Ab String
getDecimalText2 = first where
    first ((c : cs) : xs) | c `elem` "+-0123456789" = loop [[c]] $ cs : xs
    first _ = Msg.nothing

    loop ss [] = Right $ concat $ reverse ss
    loop ss (w : xs) | all (`elem` "0123456789.") w = loop (w:ss) xs
    loop _ _ = Msg.nothing

getTexts :: Bool -> B.TTreesToAb [String]
getTexts quoted = loop [] where
    loop ss [] = Right $ reverse ss
    loop ss (B.TreeL x : xs) =
        do s <- getText quoted x
           loop (s : ss) xs
    loop _ _ = Msg.nothing

getText :: Bool -> B.Token -> B.Ab String
getText True  (B.TText _ q w) | q > 0   =  Right w
getText False (B.TText _ q w) | q == 0  =  Right w
getText _ _  =  Msg.nothing

getInterp :: B.TTreesToAb B.Interp
getInterp = Right . B.interp B.<=< mapM getInterpWord

getInterpWord :: B.TTreeToAb B.InterpWord
getInterpWord (B.TreeB _ _ _) = Msg.nothing
getInterpWord (B.TreeL x) =
    case x of
      B.TText _ _ w    ->  Right $ B.InterpText w
      B.TTerm _ _ [n]  ->  Right $ B.InterpTerm n
      _                ->  Msg.nothing


-- ----------------------  Particular content

-- $Function
--
--  /Example/
--
--  >>> getTermedTrees =<< B.tt "/a 'A3 /b 10"
--  Right [("/a", TreeB 1 [TreeL (TText 3 0 "'"), TreeL (TText 4 0 "A3")]),
--         ("/b", TreeL (TText 8 0 "10"))]
--

-- | Get flat term name from token tree.
--   If the token tree contains nested term name, this function failed.
getFlatName :: B.TTreeToAb String
getFlatName (B.TreeL (B.TTerm _ 0 [n])) = Right n
getFlatName (B.TreeL t)                 = Msg.reqFlatName t
getFlatName _                           = Msg.reqTermName

-- | Convert token trees into a list of named token trees.
getTermedTrees :: B.TTreesToAb [B.NamedTree]
getTermedTrees xs = do xs' <- getTermedTrees2 xs
                       Right $ B.mapSndTo B.wrapTrees xs'

getTermedTrees2 :: B.TTreesToAb [B.NamedTrees]
getTermedTrees2 = name where
    name [] = Right []
    name (x : xs) = do let (c, xs2) = cont xs
                       n    <- getFlatName x
                       xs2' <- name xs2
                       Right $ (n, c) : xs2'

    cont :: B.TTreesTo ([B.TTree], [B.TTree])
    cont xs@(B.TreeL (B.TTerm _ 0 _) : _) = ([], xs)
    cont [] = ([], [])
    cont (x : xs) = B.cons1 x $ cont xs

-- | Literal reader for angled group.
litAngle :: (C.CContent c) => B.TTreeToAb c -> B.TTreesToAb c
litAngle lit xs@(B.TreeL (B.TTerm _ 0 _) : _) = C.putAssn =<< litAssn lit xs
litAngle _ [] = C.putAssn []
litAngle _ [B.TreeL (B.TText _ 0 "words"), B.TreeL (B.TText _ 2 ws)] =
    C.putList $ map C.pText $ words ws
litAngle _ xs =
    do toks <- tokenList xs
       ws   <- wordList toks
       makeDate ws

tokenList :: B.TTreesToAb [B.Token]
tokenList = mapM token where
    token (B.TreeL t) = Right t
    token _ = Msg.adlib "not token"

wordList :: [B.Token] -> B.Ab [String]
wordList = mapM word where
    word (B.TText _ 0 w) = Right w
    word _ = Msg.adlib "not word"

makeDate :: (C.CList c, C.CDec c) => [String] -> B.Ab c
makeDate xs = date B.<|> time B.<|> unk where
    date = put3 $ parseDate s
    time = put3 $ parseTime s
    unk  = Msg.adlib "malformed date/time literal"
    s    = unwords xs

    put3 (Nothing)        = unk
    put3 (Just (a, b, c)) = C.putList [ C.pDecFromInt a
                                      , C.pDecFromInt b
                                      , C.pDecFromInt c ]

parseDate :: String -> Maybe (Int, Int, Int)
parseDate s = date '/' s B.<|> date '-' s where
    date = parseInt3 (9999, 12, 31)

parseTime :: String -> Maybe (Int, Int, Int)
parseTime = parseInt3 (23, 59, 59) ':'

parseInt3 :: (Int, Int, Int) -> Char -> String -> Maybe (Int, Int, Int)
parseInt3 m delim s =
    case B.divide delim s of
      [n1, n2, n3] -> int3 m (n1, n2, n3)
      _            -> Nothing

int3 :: (Int, Int, Int) -> (String, String, String) -> Maybe (Int, Int, Int)
int3 (ma, mb, mc) (a, b, c) =
    do a' <- B.readInt $ B.trimBoth a
       b' <- B.readInt $ B.trimBoth b
       c' <- B.readInt $ B.trimBoth c

       rangeCheck ma a'
       rangeCheck mb b'
       rangeCheck mc c'

       Just (a', b', c')
    where
      rangeCheck m x = B.guard $ x >= 0 && x <= m

-- | Literal reader for associations.
litAssn :: (C.CContent c) => B.TTreeToAb c -> B.TTreesToAb [B.Named c]
litAssn lit = mapM p B.<=< getTermedTrees where
    p (name, tree) = Right . (name,) =<< lit tree

-- | Literal reader for relations.
litRel :: (C.CContent c) => B.TTreeToAb c -> B.TTreesToAb (B.Rel c)
litRel lit cs =
    do let (h1 : b1) = B.divideTreesByBar cs
       h2 <- getFlatName `mapM` (concat $ B.divideTreesByColon h1)
       b2 <- literals lit `mapM` b1
       let b3 = B.unique b2
       if any (length h2 /=) $ map length b3
          then Msg.oddRelation
          else Right $ B.Rel (B.headFrom h2) b3

-- | Literal reader for types.
litType :: B.TTreesToAb B.Type
litType = gen where
    gen xs = case B.divideTreesByBar xs of
               [x] ->  single x
               xs2 ->  Right . B.TypeSum =<< mapM gen xs2

    single [B.TreeB _ _ xs]  =  gen xs
    single (B.TreeL (B.TText _ q n) : xs)
        | q == 0             =  dispatch xs n
        | otherwise          =  Msg.quoteType n
    single []                =  Right $ B.TypeSum []
    single _                 =  Msg.unkType ""

    dispatch _  "empty"    =  Right B.TypeEmpty
    dispatch _  "boolean"  =  Right B.TypeBool
    dispatch _  "text"     =  Right B.TypeText
    dispatch _  "decimal"  =  Right B.TypeDec
    dispatch _  "term"     =  Right B.TypeTerm
    dispatch _  "type"     =  Right B.TypeType
    dispatch _  "interp"   =  Right B.TypeInterp
    dispatch xs "rel"      =  do ts1 <- getTermedTrees2 xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ B.TypeRel ts2
    dispatch _ n           =  Msg.unkType n

-- | Convert token trees into a judge.
--   Judges itself are not content type.
--   It can be only used in the top-level of sections.
getJudge :: (C.CContent c) => CalcContent c -> Char -> B.JudgePat -> B.TTreesToAb (B.Judge c)
getJudge calc q p = Right . judgeHead q p B.<=< litAssn (literal calc)

judgeHead :: Char -> B.JudgeOf c
judgeHead 'O' = B.JudgeAffirm
judgeHead 'X' = B.JudgeDeny
judgeHead 'V' = B.JudgeViolate
judgeHead _   = B.bug "judgeHead"


-- ------------------------------------------------------------------
-- $Types
--
--  'literal' recognizes the following types.
--
--  [Boolean]   Boolean used for something is hold or unhold.
--              Textual forms: @\#true@, @\#fasle@.
--
--  [Empty]     Empty means that there are no values.
--              i.e., universal negation on the term holds.
--              Textual form is the non-quoted parens: @()@.
--
--  [Text]      Sequence of characters.
--              Textual forms is chars with apostrophe or
--              doubly-quoted line: @\'abc@, @\"abc def\"@.
--
--  [Decimal]   Decimal number.
--              Textual forms is sequence of digits:
--              @100@, @99.50@, @hex AF@.
--
--  [Set]       Set is an unordered collection of contents.
--              Duplication among contents is not significant.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in braces:
--              @{ \'a : \'b : \'c }@.
--
--  [List]      List is an ordered list of contents.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in square brackets:
--              @[ \'abc : \'def ]@.
--
--  [Assn]      Assn is an association of terms,
--              i.e., a list of named contents.
--              Textual form is a sequence of terms
--              with bar-angles: @\<| \/a 10 \/b 20 |\>@.
--
--  [Relation]  Relation is a set of same-type tuples,
--              Textual form is a sequence of tuples
--              enclosed in bar-braces.
--              The first tuple is a heading of relation,
--              and succeeding tuples are delimited by vertical bar:
--              @{| \/a : \/b | \'A1 : 20 | \'A3 : 40 |}@.
--

-- ------------------------------------------------------------------
-- $SimpleData
--
--  Prepere some definitions.
--
--  >>> :m +Koshucode.Baala.Op.Vanilla.Type
--  >>> let trees = B.tokenTrees . B.tokens
--  >>> let lit  = literal [] :: B.TTree -> B.Ab VContent
--  >>> let lits = literals lit . trees
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


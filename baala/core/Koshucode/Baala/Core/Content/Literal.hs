{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Make literal contents from token tree.

module Koshucode.Baala.Core.Content.Literal
(
  -- * Types
  LitTree,
  LitTrees,
  LitOperators,

  -- * Functions
  litOperators,
  litContent,
  litNamedTrees,
  litJudge,
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
import qualified Koshucode.Baala.Core.Message        as Message



-- ----------------------  Type

-- | Convert 'B.TokenTree' to content.
type LitTree c = B.TokenTree -> B.Ab c

-- | Convert list of 'B.TokenTree' to content.
type LitTrees c = [B.TokenTree] -> B.Ab c

type LitOperators c = [B.Named (LitTree c -> LitTrees c)]

-- | Convert token trees into a list of named token trees.
litNamedTrees :: LitTrees [B.Named B.TokenTree]
litNamedTrees = name where
    name [] = Right []
    name (x : xs) = let (c, xs2) = cont xs
                    in do n    <- litFlatname x
                          xs2' <- name xs2
                          Right $ (n, B.treeWrap c) : xs2'

    cont :: [B.TokenTree] -> ([B.TokenTree], [B.TokenTree])
    cont xs@(B.TreeL (B.TTerm _ _) : _) = ([], xs)
    cont [] = ([], [])
    cont (x : xs) = B.cons1 x $ cont xs


-- ----------------------  General content

litOperators :: (C.CContent c) => LitOperators c
litOperators = []

litContent :: (C.CContent c) => LitTree c
litContent = litContentBy []

-- | Convert 'B.TokenTree' into internal form of content.
litContentBy :: forall c. (C.CContent c) => LitOperators c -> LitTree c
litContentBy ops tree = B.abortableTree "literal" tree $ lit tree where
    lit :: LitTree c
    lit x@(B.TreeL tok)
        | isDecimal x = C.putDec =<< B.litDecimal =<< naked x
        | otherwise = case tok of
              B.TText _ (-1) w  ->  bracketKeyword w
              B.TText _ 0    w  ->  nakedKeyword   w
              B.TText _ _    w  ->  C.putText      w
              _                 ->  Message.unkWord $ B.tokenContent tok

    lit (B.TreeB n _ xs) = case n of
        1  ->  paren xs
        2  ->  C.putList =<< litContents lit xs
        3  ->  C.putSet  =<< litContents lit xs
        4  ->                litBracket  lit xs
        5  ->  C.putRel  =<< litRel      lit xs
        _  ->  Message.adlib "Unknown paren type"

    paren :: LitTrees c
    paren xs@(x : _)
        | isDecimal x = do xs2 <- mapM naked xs
                           dec <- B.litDecimal $ concat xs2
                           C.putDec dec

    -- tagged sequence
    paren (B.TreeL (B.TText _ 0 tag) : xs) =
        case lookup tag ops of
          Just f  -> f lit xs
          Nothing -> Message.unkCop tag
    
    paren [] = Message.emptyLiteral
    -- paren [] = Right C.empty
    paren xs = Message.unkWord $ treesContent xs  -- unknown sequence

    naked :: LitTree String
    naked (B.TreeL (B.TText _ 0 w)) = Right w
    naked x                         = Message.unkWord $ treeContent x

    treeContent  = concatMap B.tokenContent . B.untree
    treesContent = concatMap B.tokenContent . B.untrees

bracketKeyword :: (C.CEmpty c, C.CBool c) => String -> B.Ab c
bracketKeyword "0"    =  Right C.false     -- <0>
bracketKeyword "1"    =  Right C.true      -- <1>
bracketKeyword "nil"  =  Right C.empty     -- <nil>
bracketKeyword w      =  Message.unkWord w

nakedKeyword :: (C.CEmpty c) => String -> B.Ab c
nakedKeyword "()"     =  Right C.empty     -- ()
nakedKeyword w        =  Message.unkWord w

isDecimal :: B.TokenTree -> Bool
isDecimal (B.TreeL (B.TText _ 0 (c : _))) = isDecimalChar c
isDecimal _ = False

-- First letters of decimals
isDecimalChar :: Char -> Bool
isDecimalChar = (`elem` "0123456789+-.")


-- ----------------------  Particular content

-- $Function
--
--  /Example/
--
--  >>> litNamedTrees =<< B.tt "/a 'A3 /b 10"
--  Right [("/a", TreeB 1 [TreeL (TText 3 0 "'"), TreeL (TText 4 0 "A3")]),
--         ("/b", TreeL (TText 8 0 "10"))]
--

litBracket :: (C.CContent c) => LitTree c -> LitTrees c
litBracket lit xs@(B.TreeL (B.TTerm _ _) : _) = C.putAssn =<< litAssn lit xs
litBracket _ [] = C.putAssn []
litBracket _ [B.TreeL (B.TText _ 0 "words"), B.TreeL (B.TText _ 2 ws)] =
    C.putList $ map C.pText $ words ws
litBracket _ xs =
    do toks <- tokenList xs
       ws   <- wordList toks
       makeDate ws

tokenList :: [B.TokenTree] -> B.Ab [B.Token]
tokenList = mapM token where
    token (B.TreeL t) = Right t
    token _ = Message.adlib "not token"

wordList :: [B.Token] -> B.Ab [String]
wordList = mapM word where
    word (B.TText _ 0 w) = Right w
    word _ = Message.adlib "not word"

makeDate :: (C.CList c, C.CDec c) => [String] -> B.Ab c
makeDate xs = date B.<|> time B.<|> unk where
    date = put3 $ parseDate s
    time = put3 $ parseTime s
    unk  = Message.adlib "malformed date/time literal"
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

-- | Get single term name.
--   If 'TokenTree' contains nested term name, this function failed.
litFlatname :: LitTree String
litFlatname (B.TreeL (B.TTerm _ [n])) = Right n
litFlatname (B.TreeL t) = Message.reqFlatName t
litFlatname _ = Message.reqTermName

litContents :: (C.CContent c) => LitTree c -> LitTrees [c]
litContents _   [] = Right []
litContents lit cs = mapM lt $ B.divideTreesByColon cs where
    lt []  = Message.emptyLiteral
    lt [x] = lit x
    lt xs  = lit $ B.TreeB 1 Nothing xs

litAssn :: (C.CContent c) => LitTree c -> LitTrees [B.Named c]
litAssn lit = mapM p B.<=< litNamedTrees where
    p (n, c) = Right . (n,) =<< lit c

litRel :: (C.CContent c) => LitTree c -> LitTrees (B.Rel c)
litRel lit cs =
    do let (h1 : b1) = B.divideTreesByBar cs
       h2 <- mapM litFlatname $ concat $ B.divideTreesByColon h1
       b2 <- mapM (litContents lit) b1
       let b3 = B.unique b2
       if any (length h2 /=) $ map length b3
          then Message.oddRelation
          else Right $ B.Rel (B.headFrom h2) b3

-- | Convert token trees into a judge.
--   Judges itself are not content type.
--   It can be only used in the top-level of sections.
litJudge :: (C.CContent c) => Char -> B.JudgePat -> LitTrees (B.Judge c)
litJudge = litJudgeBy []

litJudgeBy :: (C.CContent c) => LitOperators c -> Char -> B.JudgePat -> LitTrees (B.Judge c)
litJudgeBy ops q p = Right . (judgeHead q) p B.<=< litAssn (litContentBy ops)

judgeHead :: Char -> B.JudgeOf c
judgeHead 'O' = B.JudgeAffirm
judgeHead 'X' = B.JudgeDeny
judgeHead 'V' = B.JudgeViolate
judgeHead _   = B.bug "judgeHead"


-- ------------------------------------------------------------------
-- $Types
--
--  'litContentBy' recognizes the following types.
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
--  >>> let lit  = litContentBy [] :: B.TokenTree -> B.Ab VContent
--  >>> let lits = litContents lit . trees
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


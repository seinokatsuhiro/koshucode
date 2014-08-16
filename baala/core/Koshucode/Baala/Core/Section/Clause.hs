{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Intermidiate structure between 'String' and 'Section'.

module Koshucode.Baala.Core.Section.Clause
( -- * Data type
  -- $Documentation
  ShortClause,
  Clause (..),
  ClauseBody (..),
  clauseTypeText,

  -- * Constructors
  consClause,
  consPreclause,
) where

import qualified Data.Generics                 as G
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core.Assert   as C



-- ----------------------  Data type

type ShortClause = B.Short [Clause]

data Clause =
    Clause { clauseSource :: B.TokenClause
           , clauseBody   :: ClauseBody
           } deriving (Show, G.Data, G.Typeable)

data ClauseBody
    = CSection    (Maybe String)                 -- ^ Section name
    | CImport     [B.Token] (Maybe Clause)       -- ^ Importing section name
    | CExport     String                         -- ^ Exporting relmap name
    | CShort      [B.ShortDef]                   -- ^ Short signs
    | CRelmap     String [B.Token]               -- ^ Source of relmap
    | CAssert     C.AssertType B.JudgePat [B.Token] [B.Token] -- ^ (Intermediate data)
    | CJudge      Char B.JudgePat [B.Token]      -- ^ Judge
    | CSlot       String [B.Token]               -- ^ Global slot
    | CComment                                   -- ^ Clause comment
    | CUnknown                                   -- ^ Unknown clause
    | CUnres      [B.Token]                      -- ^ Unresolved short sign
      deriving (Show, G.Data, G.Typeable)

instance B.CodePointer Clause where
    codePoints (Clause src _) = B.codePoints src

-- | Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@.
clauseTypeText :: Clause -> String
clauseTypeText (Clause _ body) =
    case body of
      CSection   _         ->  "section"
      CImport    _ _       ->  "import"
      CExport    _         ->  "export"
      CShort     _         ->  "short"
      CRelmap    _ _       ->  "relmap"
      CAssert    _ _ _ _   ->  "assert"
      CJudge     _ _ _     ->  "judge"
      CSlot      _ _       ->  "slot"
      CComment             ->  "comment"
      CUnknown             ->  "unknown"
      CUnres     _         ->  "unres"



-- ----------------------  Construction

-- | Convert token list into clause list.
--   Result clause list does not contain
--   'CRelmap' and 'CAssert'. Instead of them,
--   'TTokmap' and 'CAssert' are contained.
--   This function does not depend on 'C.ConsLexmap'.
--
--   >>> consPreclause . B.tokenize $ "a : source A /x /y"
--   [ TTokmap ( TokenClause
--                [TText 1 0 "a", TSpace 2 1, ..., TTerm 11 ["/y"]]
--                [CodeLine 1 "a : source A /x /y" [TText 1 0 "a", ...]] )
--            "a" [ TText 5 0 "source"
--                , TText 7 0 "A"
--                , TTerm 9 ["/x"]
--                , TTerm 11 ["/y"]
--                ]]

-- | First step of constructing 'Section'.
consClause :: [B.TokenLine] -> [ShortClause]
consClause = shortClause . consPreclause

consPreclause :: [B.TokenLine] -> [Clause]
consPreclause = concatMap consPreclause' . B.tokenClauses

consPreclause' :: B.TokenClause -> [Clause]
consPreclause' src = dispatch $ liaison $ B.clauseTokens src where

    liaison :: B.Map [B.Token]
    liaison [] = []
    liaison (B.TText p1 q1 w1 : B.TText _ q2 w2 : xs)
        | q1 > 0 && q2 > 0 = let tok = B.TText p1 (max q1 q2) (w1 ++ w2)
                             in liaison $ tok : xs
    liaison (x : xs) = x : liaison xs

    dispatch :: [B.Token] -> [Clause]
    dispatch (B.TText _ 0 "|" : B.TText _ 0 k : xs) =
        frege k xs  -- Frege's judgement stroke
    dispatch (B.TText _ 0 name : B.TText _ 0 colon : xs)
        | isDelim colon   =  rmap name xs
    dispatch (B.TText _ 0 k : xs)
        | k == "section"  =  sec xs
        | k == "import"   =  impt xs
        | k == "export"   =  expt xs
        | k == "short"    =  short xs
        | k == "****"     =  c1 CComment
    dispatch (B.TSlot _ 2 n : xs) = slot n xs
    dispatch []           =  []
    dispatch _            =  unk

    unk                   =  c1 CUnknown
    c0                    =  Clause src
    c1                    =  B.li1 . c0

    isDelim     =  (`elem` ["|", ":"])

    frege "--"  =  judge 'O'
    frege "-"   =  judge 'O'
    frege "-X"  =  judge 'X'
    frege "-x"  =  judge 'X'
    frege "-V"  =  judge 'V'
    frege "-v"  =  judge 'V'

    frege "=="  =  assert C.AssertAffirm
    frege "="   =  assert C.AssertAffirm
    frege "=X"  =  assert C.AssertDeny
    frege "=x"  =  assert C.AssertDeny
    frege "=V"  =  assert C.AssertViolate
    frege "=v"  =  assert C.AssertViolate

    frege _     =  const unk

    judge q (B.TText _ _ p : xs)  =  c1 $ CJudge q p xs
    judge _ _                     =  unk

    assert t (B.TText _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  ->  a expr opt
          Left  expr            ->  a expr []
        where a expr opt = c1 $ CAssert t p opt expr
    assert _ _             =  unk

    rmap n xs              =  c1 $ CRelmap n xs
    slot n xs              =  c1 $ CSlot   n xs

    sec [B.TText _ _ n]    =  c1 $ CSection (Just n)
    sec []                 =  c1 $ CSection Nothing
    sec _                  =  unk

    expt (B.TText _ _ n : B.TText _ _ ":" : xs)
                           =  c0 (CExport n) : rmap n xs
    expt [B.TText _ _ n]   =  c1 $ CExport n
    expt _                 =  unk

    impt xs                =  c1 $ CImport xs Nothing

    short xs               =  case wordPairs xs of
                                Nothing -> unk
                                Just sh -> c1 $ CShort sh

pairs :: [a] -> Maybe [(a, a)]
pairs (a:b:cs) = do cs' <- pairs cs
                    Just $ (a, b) : cs'
pairs [] = Just []
pairs _  = Nothing

wordPairs :: [B.Token] -> Maybe [(String, String)]
wordPairs toks =
    do p <- pairs toks
       mapM wordPair p
    where
      wordPair :: (B.Token, B.Token) -> Maybe (String, String)
      wordPair (B.TText _ _ a, B.TText _ _ b) = Just (a, b)
      wordPair _ = Nothing



-- ----------------------  Short-to-long conversion

shortClause :: [Clause] -> [ShortClause]
shortClause [] = []
shortClause ccs@(c : cs)
    | isCShort c = scope cs  $ shorts c
    | otherwise  = scope ccs []
    where
      pt = B.codePoints c

      scope :: [Clause] -> [B.ShortDef] -> [ShortClause]
      scope cs12 sh =
          let (cs1, cs2) = span (not . isCShort) cs12
              short      = B.Short pt sh $ shortToLong sh cs1
          in  short : shortClause cs2

      shorts :: Clause -> [B.ShortDef]
      shorts (Clause _ (CShort sh)) = sh
      shorts _                      = B.bug "shortClause"

      isCShort :: Clause -> Bool
      isCShort (Clause _ (CShort _)) = True
      isCShort _                     = False

shortToLong :: [B.ShortDef] -> B.Map [Clause]
shortToLong [] = id
shortToLong sh = map clause where
    clause :: B.Map Clause
    clause (Clause src bo) =
        Clause src $ case bo of
          CJudge  q p     xs  ->  body (CJudge  q p)     xs
          CAssert q p opt xs  ->  body (CAssert q p opt) xs
          CRelmap n       xs  ->  body (CRelmap n)       xs
          CSlot   n       xs  ->  body (CSlot   n)       xs
          _                   ->  bo

    body :: ([B.Token] -> ClauseBody) -> [B.Token] -> ClauseBody
    body k xs =
        let ls = map long xs
            ss = filter B.isShortToken ls
        in if null ss
           then k ls
           else CUnres ss

    long :: B.Map B.Token
    long token@(B.TShort n a b) =
        case lookup a sh of
          Just l  -> B.TText n 2 $ l ++ b
          Nothing -> token
    long token = token



-- ----------------------
-- $Documentation
--
--  There are eight types of 'Clause'.
--  Textual representation of 'Section' is a list of clauses.
--  'consClause' constructs clause list from section text.
--
--  [@short@ prefix full ...]
--    Clause for declarations of short signs
--
--  [@|--@ pattern \/name content ...]
--    Affirmative judgement clause
--
--  [@|-X@ pattern \/name content ...]
--    Denial judgement clause
--
--  [name @:@ relmap]
--    Relmap clause
--
--  [@|==@ pattern @:@ relmap]
--    Affirmative assertion clause
--
--  [@|=X@ pattern @:@ relmap]
--    Denial assertion clause
--
--  [@|=V@ pattern @:@ relmap]
--    Violated assertion clause
--
--  [@****@ blah blah blah ...]
--    Comment clause


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
  consPreclause,
  consClause,
) where

import qualified Data.Generics                 as G
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core.Relmap   as C
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
    | CShort      [(B.Named String)]             -- ^ Short signs
    | CTokmap     String [B.TokenTree]           -- ^ Source of relmap
    | TTokmap     String [B.Token]               -- ^ (Intermediate data)
    | CAssert     C.AssertType B.JudgePattern C.AssertOption [B.TokenTree] -- ^ Assertions of relmap
    | TAssert     C.AssertType B.JudgePattern C.AssertOption [B.Token]     -- ^ (Intermediate data)
    | CJudge      Bool B.JudgePattern [B.Token]  -- ^ Judge
    | CSlot       String [B.TokenTree]           -- ^ Global slot
    | TSlot       String [B.Token]               -- ^ Global slot
    | CComment                                   -- ^ Clause comment
    | CUnknown                                   -- ^ Unknown clause
    | CUnres      [B.Token]                      -- ^ Unresolved short sign
      deriving (Show, G.Data, G.Typeable)

-- | Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@.
clauseTypeText :: Clause -> String
clauseTypeText (Clause _ body) =
    case body of
      CSection   _         ->  "Section"
      CImport    _ _       ->  "Import"
      CExport    _         ->  "Export"
      CShort     _         ->  "Short"
      CTokmap    _ _       ->  "Tokmap"
      TTokmap    _ _       ->  "Tokmap"
      CAssert    _ _ _ _   ->  "Assert"
      TAssert    _ _ _ _   ->  "Assert"
      CJudge     _ _ _     ->  "Judge"
      CSlot      _ _       ->  "Slot"
      TSlot      _ _       ->  "Slot"
      CComment             ->  "Comment"
      CUnknown             ->  "Unknown"
      CUnres     _         ->  "Unres"



-- ----------------------  Preconstruction

-- | Convert token list into clause list.
--   Result clause list does not contain
--   'CTokmap' and 'CAssert'. Instead of them,
--   'TTokmap' and 'TAssert' are contained.
--   This function does not depend on 'C.ConsLexmap'.
--
--   >>> consPreclause . B.tokenize $ "a : source A /x /y"
--   [ TTokmap ( TokenClause
--                [TWord 1 0 "a", TSpace 2 1, ..., TTerm 11 ["/y"]]
--                [CodeLine 1 "a : source A /x /y" [TWord 1 0 "a", ...]] )
--            "a" [ TWord 5 0 "source"
--                , TWord 7 0 "A"
--                , TTerm 9 ["/x"]
--                , TTerm 11 ["/y"]
--                ]]

consPreclause :: [B.TokenLine] -> [Clause]
consPreclause = concatMap consPreclause' . B.tokenClauses

consPreclause' :: B.TokenClause -> [Clause]
consPreclause' src = dispatch $ B.clauseTokens src where

    dispatch :: [B.Token] -> [Clause]
    dispatch (B.TWord _ 0 "|" : B.TWord _ 0 k : xs) =
        frege k xs  -- Frege's judgement stroke
    dispatch (B.TWord _ 0 name : B.TWord _ 0 colon : xs)
        | isDelim colon   =  rmap name xs
    dispatch (B.TWord _ 0 k : xs)
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
    c1                    =  B.singleton . c0

    isDelim     =  (`elem` ["|", ":"])

    frege "--"  =  judge True
    frege "-"   =  judge True
    frege "-X"  =  judge False
    frege "-x"  =  judge False

    frege "=="  =  assert C.AssertAffirm
    frege "="   =  assert C.AssertAffirm
    frege "=X"  =  assert C.AssertDeny
    frege "=x"  =  assert C.AssertDeny
    frege "=V"  =  assert C.AssertViolate
    frege "=v"  =  assert C.AssertViolate

    frege _     =  const unk

    judge q (B.TWord _ _ p : xs)  =  c1 $ CJudge q p xs
    judge _ _                     =  unk

    assert t (B.TWord _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  ->  a expr opt
          Left  expr            ->  a expr []
        where a expr opt =
                  let opt' = C.rod $ B.tokenTrees opt
                  in c1 $ TAssert t p opt' expr
    assert _ _             =  unk

    rmap n xs              =  c1 $ TTokmap n xs
    slot n xs              =  c1 $ TSlot   n xs

    sec [B.TWord _ _ n]    =  c1 $ CSection (Just n)
    sec []                 =  c1 $ CSection Nothing
    sec _                  =  unk

    expt (B.TWord _ _ n : B.TWord _ _ ":" : xs)
                           =  c0 (CExport n) : rmap n xs
    expt [B.TWord _ _ n]   =  c1 $ CExport n
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
      wordPair (B.TWord _ _ a, B.TWord _ _ b) = Just (a, b)
      wordPair _ = Nothing


-- ----------------------  Lexmap construction

-- | Construct 'Clause' list from 'B.Token' list.
--   This is a first step of constructing 'Section'.
consClause :: [B.TokenLine] -> [ShortClause]
consClause = (map $ fmap clauseTree) . shortSections . consPreclause

clauseTree :: B.Map [Clause]
clauseTree = map clause where
    clause :: B.Map Clause
    clause (Clause src bo) = Clause src $ tree bo

    tree :: B.Map ClauseBody
    tree (TSlot   n ts)     = CSlot   n     $ B.tokenTrees ts
    tree (TTokmap n ts)     = CTokmap n     $ B.tokenTrees ts
    tree (TAssert q p o ts) = CAssert q p o $ B.tokenTrees ts
    tree bo = bo



-- ----------------------  Short-to-long conversion

shortSections :: [Clause] -> [ShortClause]
shortSections [] = []
shortSections xxs@(x : xs)
    | isCShort x = f xs $ shorts x
    | otherwise  = f xxs []
    where f cl sh = case span (not . isCShort) cl of
                       (xs1, xs2) -> B.Short sh (shortToLong sh xs1)
                                     : shortSections xs2

isCShort :: Clause -> Bool
isCShort (Clause _ (CShort _)) = True
isCShort _ = False

shorts :: Clause -> [B.Named String]
shorts (Clause _ (CShort s)) = s
shorts _ = []

shortToLong :: [B.Named String] -> B.Map [Clause]
shortToLong [] = id
shortToLong sh = map clause where
    clause :: B.Map Clause
    clause cl@(Clause src b) =
        case b of
          CJudge  q p xs     -> Clause src $ body xs $ CJudge  q p
          TAssert q p opt xs -> Clause src $ body xs $ TAssert q p opt
          TTokmap n xs       -> Clause src $ body xs $ TTokmap n
          _                  -> cl

    body :: [B.Token] -> ([B.Token] -> ClauseBody) -> ClauseBody
    body xs k =
        let ls = map long xs
            ss = filter B.isShortToken ls
        in if null ss
           then k ls
           else CUnres ss

    long :: B.Map B.Token
    long token@(B.TShort n a b) =
        case lookup a sh of
          Just l  -> B.TWord n 2 $ l ++ b
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


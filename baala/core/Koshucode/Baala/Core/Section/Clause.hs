{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Intermidiate structure between 'String' and 'Section'.

module Koshucode.Baala.Core.Section.Clause
( -- * Datatype
  -- $Documentation
  Clause (..),
  ClauseBody (..),
  clauseTypeText,

  -- * Constructors
  consPreclause,
  consClause,
) where

import qualified Data.Generics as G
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Relmap  as C
import qualified Koshucode.Baala.Core.Assert  as C



-- ----------------------  Data type

data Clause =
    Clause { clauseSource :: B.TokenClause
           , clauseBody   :: ClauseBody
           } deriving (Show, G.Data, G.Typeable)

data ClauseBody
    = CSection    (Maybe String)                 -- ^ Section name
    | CImport     [B.Token] (Maybe Clause)       -- ^ Importing section name
    | CExport     String                         -- ^ Exporting relmap name
    | CShort      [(B.Named String)]             -- ^ Short signs
    | CRelmapUse  String C.Rod C.Lexmap          -- ^ Lexmap and its name
    | TRelmapDef  String [B.TokenTree]
    | TRelmap     String [B.Token]               -- ^ Not include Lexmap
    | CAssert     C.AssertType B.JudgePattern C.AssertOption C.Lexmap   -- ^ Assertions of relmaps
    | TAssert     C.AssertType B.JudgePattern C.AssertOption [B.Token]  -- ^ Not include Lexmap
    | CJudge      Bool B.JudgePattern [B.Token]  -- ^ Judge
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
      CRelmapUse _ _ _     ->  "RelmapUse"
      TRelmapDef _ _       ->  "RelmapDef"
      TRelmap    _ _       ->  "Relmap"
      CAssert    _ _ _ _   ->  "Assert"
      TAssert    _ _ _ _   ->  "Assert"
      CJudge     _ _ _     ->  "Judge"
      CComment             ->  "Comment"
      CUnknown             ->  "Unknown"
      CUnres     _         ->  "Unres"



-- ----------------------  Preconstruction

-- | Convert token list into clause list.
--   Result clause list does not contain
--   'CRelmapUse' and 'CAssert'. Instead of them,
--   'TRelmap' and 'TAssert' are contained.
--   This function does not depend on 'C.ConsLexmap'.
--
--   >>> consPreclause . B.tokenize $ "a : source A /x /y"
--   [ TRelmap ( TokenClause
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
        frege k xs  -- frege's judgement stroke
    dispatch (B.TWord _ 0 name : B.TWord _ 0 colon : xs)
        | isDelim colon   =  rel name xs
    dispatch (B.TWord _ 0 k : xs)
        | k == "section"  =  sec xs
        | k == "import"   =  impt xs
        | k == "export"   =  expt xs
        | k == "short"    =  short xs
        | k == "****"     =  c1 CComment
    dispatch []           =  []
    dispatch _            =  unk

    unk                   =  c1 CUnknown
    c0                    =  Clause src
    c1                    =  B.singleton . c0

    isDelim     =  (`elem` ["|", ":"])

    frege "--"  =  jud True
    frege "-"   =  jud True
    frege "-X"  =  jud False
    frege "-x"  =  jud False

    frege "=="  =  ass C.AssertAffirm
    frege "="   =  ass C.AssertAffirm
    frege "=X"  =  ass C.AssertDeny
    frege "=x"  =  ass C.AssertDeny
    frege "=V"  =  ass C.AssertViolate
    frege "=v"  =  ass C.AssertViolate

    frege _     =  const unk

    jud q (B.TWord _ _ p : xs) = c1 $ CJudge q p xs
    jud _ _     =  unk

    ass t (B.TWord _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  ->  a expr opt
          Left  expr            ->  a expr []
        where a expr opt =
                  let opt' = C.ropOperandAssoc $ B.tokenTrees opt
                  in c1 $ TAssert t p opt' expr
    ass _ _               =  unk

    rel n expr            =  c1 $ TRelmap n expr

    sec [B.TWord _ _ n]    =  c1 $ CSection (Just n)
    sec []                 =  c1 $ CSection Nothing
    sec _                  =  unk

    expt (B.TWord _ _ n : B.TWord _ _ ":" : xs) = c0 (CExport n) : rel n xs
    expt [B.TWord _ _ n]   =  c1 $ CExport n
    expt _                 =  unk

    impt xs                =  c1 $ CImport xs Nothing

    short xs               = case wordPairs xs of
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
consClause
    :: C.ConsLexmap             -- ^ Relmap lex constructor
    -> [B.TokenLine]            -- ^ Source tokens
    -> B.Ab [B.Short [Clause]]  -- ^ Result clauses
consClause cons = clauseLexClause cons . shortSections . consPreclause

clauseLexClause :: C.ConsLexmap -> B.AbMap [B.Short [Clause]]
clauseLexClause cons = sequence . map B.shortM . f where
    f :: [B.Short [Clause]] -> [B.Short (B.Ab [Clause])]
    f = map $ fmap (clauseLexUse cons B.<=< clauseLexDef cons)

clauseLexDef :: C.ConsLexmap -> B.AbMap [Clause]
clauseLexDef cons = mapM clause where
    clause :: B.AbMap Clause
    clause (Clause src bd) = Right . Clause src =<< def bd

    def :: B.AbMap ClauseBody
    def (TRelmap n ts)       -- = Right $ TRelmapDef n $ B.tokenTrees ts
                             = Right . CRelmapUse n [] =<< cons (B.tokenTrees ts)
    def (TAssert q p opt ts) = Right . CAssert q p opt =<< cons (B.tokenTrees ts)
    def bd = Right bd

clauseLexUse :: C.ConsLexmap -> B.AbMap [Clause]
clauseLexUse cons cs =
    do cs2 <- mapM use ass
       Right $ concat cs2 ++ cs
    where
      def = map rel cs
      ass = B.catMaybes $ map maybeLexmap cs
      use lexmap = lexmapUse cons lexmap def

      rel (Clause src (TRelmapDef n trees)) = (n, (src, trees))
      rel (Clause src _) = ("", (src, []))

      maybeLexmap (Clause _ (CAssert _ _ _ lexmap)) = Just lexmap
      maybeLexmap _ = Nothing

-- clauseLexUse :: C.ConsLexmap -> B.AbMap [Clause]
-- clauseLexUse lx = mapM clause where
--     clause :: B.AbMap Clause
--     clause (Clause src bd)    = Right . Clause src      =<< body bd

--     body :: B.AbMap ClauseBody
--     body (TRelmapDef n trees) = Right . CRelmapUse n [] =<< lx trees
--     body (TAssert q p opt ts) = Right . CAssert q p opt =<< lx (B.tokenTrees ts)
--     body bd                   = Right bd


-- ----------------------  Short-to-long conversion

shortSections :: [Clause] -> [B.Short [Clause]]
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
          TRelmap n xs       -> Clause src $ body xs $ TRelmap n
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


-- ----------------------  Substitution

substTree :: C.Rod -> B.Map B.TokenTree
substTree assoc = loop where
    loop (B.TreeB p q sub) = B.TreeB p q $ map loop sub
    loop tk@(B.TreeL (B.TWord _ 0 ('#' : word))) =
        case lookup word assoc of
          Nothing    -> tk
          Just trees -> B.TreeB 1 Nothing trees
    loop tk = tk

substTrees :: C.Rod -> B.Map [B.TokenTree]
substTrees assoc = (substTree assoc `map`)

lexmapUse :: C.ConsLexmap -> C.Lexmap
    -> [(String, (B.TokenClause, [B.TokenTree]))]
    -> B.Ab [Clause]
lexmapUse cons lexmap def = loop lexmap where
    loop lx = let op = C.lexOpText lx
                  od = C.lexOperand lx
                  subuse = loops $ C.lexSubmap lx
              in case lookup op def of
                   Nothing    -> subuse
                   Just (src, trees) ->
                       do lx2 <- cons $ substTrees od trees
                          use2 <- loop lx2
                          use3 <- subuse
                          Right $ Clause src (CRelmapUse op od lx2) : use2 ++ use3

    loops :: [C.Lexmap] -> B.Ab [Clause]
    loops [] = Right []
    loops (lx : lxs) = do lx'  <- loop lx
                          lxs' <- loops lxs
                          Right $ lx' ++ lxs'


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


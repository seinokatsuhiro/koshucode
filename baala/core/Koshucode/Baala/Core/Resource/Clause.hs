{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Intermidiate structure between 'String' and 'Resource'.

module Koshucode.Baala.Core.Resource.Clause
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
import qualified Koshucode.Baala.Core.Content  as C



-- ----------------------  Data type

type ShortClause = B.Short [Clause]

data Clause =
    Clause { clauseSource  :: B.TokenClause
           , clauseSecNo   :: Int
           , clauseBody    :: ClauseBody
           } deriving (Show, G.Data, G.Typeable)

data ClauseBody
    = CSection  String                                      -- ^ Section heading
    | CImport   [B.Token] (Maybe Clause)                    -- ^ Importing resource name
    | CExport   String                                      -- ^ Exporting name
    | CShort    [B.ShortDef]                                -- ^ Short signs
    | CRelmap   String [B.Token]                            -- ^ Source of relmap
    | CAssert   C.AssertType B.JudgePat [B.Token] [B.Token] -- ^ Assertion
    | CJudge    C.AssertType B.JudgePat [B.Token]           -- ^ Judge
    | CSlot     String [B.Token]                            -- ^ Global slot
    | CUnknown                                              -- ^ Unknown clause
    | CUnres    [B.Token]                                   -- ^ Unresolved short sign
      deriving (Show, G.Data, G.Typeable)

instance B.CodePtr Clause where
    codePts (Clause src _ _) = B.codePts src

-- | Name of clause type. e.g., @\"relmap\"@, @\"assert\"@.
clauseTypeText :: Clause -> String
clauseTypeText (Clause _ _ body) =
    case body of
      CSection   _         -> "section"
      CImport    _ _       -> "import"
      CExport    _         -> "export"
      CShort     _         -> "short"
      CRelmap    _ _       -> "relmap"
      CAssert    _ _ _ _   -> "assert"
      CJudge     _ _ _     -> "judge"
      CSlot      _ _       -> "slot"
      CUnknown             -> "unknown"
      CUnres     _         -> "unres"



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

-- | First step of constructing 'Resource'.
consClause :: [B.TokenLine] -> [ShortClause]
consClause = shortClause . consPreclause

consPreclause :: [B.TokenLine] -> [Clause]
consPreclause = loop 0 . B.tokenClauses where
    loop _ [] = []
    loop n (x:xs) = let (n', cs) = consPreclause' n x
                    in cs ++ loop n' xs

consPreclause' :: Int -> B.TokenClause -> (Int, [Clause])
consPreclause' no src = dispatch $ liaison $ B.clauseTokens src where

    liaison :: B.Map [B.Token]
    liaison [] = []
    liaison (B.TText _ B.TextQ "" : B.TTerm p2 0 w2 : xs)
                       = let tok = B.TTerm p2 1 w2
                         in liaison $ tok : xs
    liaison (x : xs)   = x : liaison xs

    dispatch :: [B.Token] -> (Int, [Clause])
    dispatch (B.TText _ B.TextBar ('|' : k) : xs) =
        same $ frege k xs   -- Frege's judgement stroke
    dispatch (B.TText _ B.TextRaw name : B.TText _ B.TextRaw colon : xs)
        | isDelim colon             = same $ rmap name xs
    dispatch (B.TText _ B.TextRaw k : xs)
        | k == "import"             = same $ impt xs
        | k == "export"             = same $ expt xs
        | k == "short"              = same $ short xs
        | k == "****"               = same []
        | k == "=="                 = up   $ sec xs
    dispatch (B.TSlot _ 2 n : xs)   = same $ slot n xs
    dispatch []                     = same []
    dispatch _                      = same unk

    same cs      = (no,     cs)
    up   cs      = (no + 1, cs)

    unk          = c1 CUnknown
    c0           = Clause src no
    c1           = B.li1 . c0

    isDelim      = (`elem` ["|", ":"])

    frege "--"   = judge C.AssertAffirm
    frege "-"    = judge C.AssertAffirm
    frege "-X"   = judge C.AssertDeny
    frege "-x"   = judge C.AssertDeny
    frege "-V"   = judge C.AssertViolate
    frege "-v"   = judge C.AssertViolate

    frege "=="   = assert C.AssertAffirm
    frege "="    = assert C.AssertAffirm
    frege "=X"   = assert C.AssertDeny
    frege "=x"   = assert C.AssertDeny
    frege "=V"   = assert C.AssertViolate
    frege "=v"   = assert C.AssertViolate

    frege _      = const unk

    judge q (B.TText _ _ p : xs)  = c1 $ CJudge q p xs
    judge _ _                     = unk

    assert t (B.TText _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  -> a expr opt
          Left  expr            -> a expr []
        where a expr opt        = c1 $ CAssert t p opt expr
    assert _ _                  = unk

    rmap n xs                   = c1 $ CRelmap n xs
    slot n xs                   = c1 $ CSlot   n xs

    sec [B.TText _ _ n]         = c1 $ CSection n
    sec _                       = unk

    expt (B.TText _ _ n : B.TText _ _ ":" : xs)
                                = c0 (CExport n) : rmap n xs
    expt [B.TText _ _ n]        = c1 $ CExport n
    expt _                      = unk

    impt xs                     = c1 $ CImport xs Nothing

    short xs                    = case wordPairs xs of
                                    Nothing  -> unk
                                    Just sh  -> c1 $ CShort sh

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
      pt = B.codePts c

      scope :: [Clause] -> [B.ShortDef] -> [ShortClause]
      scope cs12 sh =
          let (cs1, cs2) = span (not . isCShort) cs12
              short      = B.Short pt sh $ shortToLong sh cs1
          in  short : shortClause cs2

      shorts :: Clause -> [B.ShortDef]
      shorts (Clause _  _ (CShort sh))  = sh
      shorts _                          = B.bug "shortClause"

      isCShort :: Clause -> Bool
      isCShort (Clause _ _ (CShort _))  = True
      isCShort _                        = False

shortToLong :: [B.ShortDef] -> B.Map [Clause]
shortToLong [] = id
shortToLong sh = map clause where
    clause :: B.Map Clause
    clause (Clause src sec bo) =
        Clause src sec $ case bo of
          CJudge  q p     xs   -> body (CJudge  q p)     xs
          CAssert q p opt xs   -> body (CAssert q p opt) xs
          CRelmap n       xs   -> body (CRelmap n)       xs
          CSlot   n       xs   -> body (CSlot   n)       xs
          _                    -> bo

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
          Just l  -> B.TText n B.TextQQ $ l ++ b
          Nothing -> token
    long token = token



-- ----------------------
-- $Documentation
--
--  There are eight types of 'Clause'.
--  Textual representation of 'Resource' is a list of clauses.
--  'consClause' constructs clause list from resource text.
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


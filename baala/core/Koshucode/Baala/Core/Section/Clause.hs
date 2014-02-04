{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'Section'. -}

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

data Clause =
    Clause { clauseSource :: B.TokenClause
           , clauseBody   :: ClauseBody
           } deriving (Show, G.Data, G.Typeable)

data ClauseBody
    = CSection  (Maybe String)            -- ^ Section name
    | CImport   [B.Token] (Maybe Clause)  -- ^ Importing section name
    | CExport   String                    -- ^ Exporting relmap name
    | CShort    (B.Named String)          -- ^ Short signs
    | CRelmap   String C.HalfRelmap       -- ^ Relmap and its name
    | TRelmap   String [B.Token]          -- ^ Not include HalfRelmap
    | CAssert   C.AssertType B.JudgePattern C.AssertOption C.HalfRelmap  -- ^ Assertions of relmaps
    | TAssert   C.AssertType B.JudgePattern C.AssertOption [B.Token]     -- ^ Not include HalfRelmap
    | CJudge    Bool B.JudgePattern [B.Token]     -- ^ Judge
    | CComment               -- ^ Clause comment
    | CUnknown               -- ^ Unknown clause
    | CUnres    [B.Token]    -- ^ Unresolved short sign
      deriving (Show, G.Data, G.Typeable)

{-| Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@. -}
clauseTypeText :: Clause -> String
clauseTypeText (Clause _ body) =
    case body of
      CSection _         ->  "Section"
      CImport  _ _       ->  "Import"
      CExport  _         ->  "Export"
      CShort   _         ->  "Short"
      CRelmap  _ _       ->  "Relmap"
      TRelmap  _ _       ->  "Relmap"
      CAssert  _ _ _ _   ->  "Assert"
      TAssert  _ _ _ _   ->  "Assert"
      CJudge   _ _ _     ->  "Judge"
      CComment           ->  "Comment"
      CUnknown           ->  "Unknown"
      CUnres   _         ->  "Unres"



-- ----------------------  Preconstruction

{-| Convert token list into clause list.
    Result clause list does not contain
    'CRelmap' and 'CAssert'. Instead of them,
    'TRelmap' and 'TAssert' are contained.
    This function does not depend on 'C.RelmapConsHalf'.

    >>> consPreclause . B.tokenize $ "a : source A /x /y"
    [ TRelmap ( TokenClause
                 [TWord 1 0 "a", TSpace 2 1, ..., TTerm 11 ["/y"]]
                 [CodeLine 1 "a : source A /x /y" [TWord 1 0 "a", ...]] )
             "a" [ TWord 5 0 "source"
                 , TWord 7 0 "A"
                 , TTerm 9 ["/x"]
                 , TTerm 11 ["/y"]
                 ]]
    -}
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

    sec [B.TWord _ _ n]   =  c1 $ CSection (Just n)
    sec []                =  c1 $ CSection Nothing
    sec _                 =  unk

    expt (B.TWord _ _ n : B.TWord _ _ ":" : xs) = c0 (CExport n) : rel n xs
    expt [B.TWord _ _ n]   =  c1 $ CExport n
    expt _                 =  unk

    impt xs                =  c1 $ CImport xs Nothing

    short [B.TWord _ _ a, B.TWord _ _ b] = c1 $ CShort (a, b)
    short _               =  unk



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'B.Token' list.
    This is a first step of constructing 'Section'. -}
consClause
    :: C.RelmapConsHalf    -- ^ Relmap half constructor
    -> [B.TokenLine]       -- ^ Source tokens
    -> B.Ab [Clause]  -- ^ Result clauses
consClause half = clauseHalf half . consPreclause

clauseHalf :: C.RelmapConsHalf -> [Clause] -> B.Ab [Clause]
clauseHalf half xs = mapM f xs2 where
    f (Clause src body)        = Right . Clause src =<< g body

    g (TRelmap n ts)       = Right . CRelmap n       =<< h ts
    g (TAssert q p opt ts) = Right . CAssert q p opt =<< h ts
    g body                 = Right body

    h ts = half (B.tokenTrees ts)

    xs2 = concatMap resolve xs
    resolve = resolveClause $ concatMap short xs
    short (Clause _ (CShort p)) = [p]
    short _ = []

resolveClause :: [B.Named String] -> Clause -> [Clause]
resolveClause shorts = f where
    f (Clause src (CJudge  q p xs))     = resolve src xs $ CJudge  q p
    f (Clause src (TAssert q p opt xs)) = resolve src xs $ TAssert q p opt
    f (Clause src (TRelmap n xs))       = resolve src xs $ TRelmap n
    f clause                            = [clause]

    resolve src xs k = let xs2 = map (resolveShort shorts) xs
                       in Clause src (k xs2) : unres src xs2
    unres src xs2    = case filter isTShort xs2 of
                         []  -> []
                         xs3 -> [Clause src (CUnres xs3)]

resolveShort :: [B.Named String] -> B.Map B.Token
resolveShort shorts = f where
    f token@(B.TShort n a b) =
        case lookup a shorts of
          Just l  -> B.TWord n 2 $ l ++ b
          Nothing -> token
    f token = token

isTShort :: B.Token -> Bool
isTShort (B.TShort _ _ _) = True
isTShort _                = False



-- ----------------------
{- $Documentation

   There are seven types of 'Clause'.
   Textual representation of 'Section' is a list of clauses.
   'consClause' constructs clause list from section text.

   [@|--@ pattern \/name content ...]
     Affirmative judgement clause

   [@|-X@ pattern \/name content ...]
     Denial judgement clause

   [name @:@ relmap]
     Relmap clause

   [@|==@ pattern @:@ relmap]
     Affirmative assertion clause

   [@|=X@ pattern @:@ relmap]
     Denial assertion clause

   [@|=V@ pattern @:@ relmap]
     Violated assertion clause

   [@****@ blah blah blah ...]
     Comment clause

-}


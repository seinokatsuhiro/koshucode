{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'C.Section'. -}

module Koshucode.Baala.Core.Section.Clause
( -- * Datatype
  -- $Documentation
  Clause (..),
  clauseTypeText,
  clauseSource,

  -- * Constructors
  consPreclause,
  consClause,
  consSection,
) where

import Prelude hiding (exp, mod)
import qualified Data.Generics as G

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content          as C
import qualified Koshucode.Baala.Core.Relmap           as C
import qualified Koshucode.Baala.Core.Assert           as C
import qualified Koshucode.Baala.Core.Section.Clausify as C
import qualified Koshucode.Baala.Core.Section.Section  as C

data Clause
    = CSection  C.ClauseSource (Maybe String)            -- ^ Section name
    | CImport   C.ClauseSource [B.Token] (Maybe Clause)  -- ^ Importing section name
    | CExport   C.ClauseSource String                    -- ^ Exporting relmap name
    | CShort    C.ClauseSource (B.Named String)          -- ^ Short signs
    | CRelmap   C.ClauseSource String C.HalfRelmap       -- ^ Relmap and its name
    | TRelmap   C.ClauseSource String [B.Token]          -- ^ Not include HalfRelmap
    | CAssert   C.ClauseSource C.AssertType B.JudgePattern C.AssertOption C.HalfRelmap  -- ^ Assertions of relmaps
    | TAssert   C.ClauseSource C.AssertType B.JudgePattern C.AssertOption [B.Token]     -- ^ Not include HalfRelmap
    | CJudge    C.ClauseSource Bool B.JudgePattern [B.Token]     -- ^ Judge
    | CComment  C.ClauseSource           -- ^ Clause comment
    | CUnknown  C.ClauseSource           -- ^ Unknown clause
    | CUnres    C.ClauseSource [B.Token] -- ^ Unresolved short sign
      deriving (Show, G.Data, G.Typeable)

{-| Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@. -}
clauseTypeText :: Clause -> String
clauseTypeText c =
    case c of
      CSection _ _          ->  "Section"
      CImport  _ _ _        ->  "Import"
      CExport  _ _          ->  "Export"
      CShort   _ _          ->  "Short"
      CRelmap  _ _ _        ->  "Relmap"
      TRelmap  _ _ _        ->  "Relmap"
      CAssert  _ _ _ _ _    ->  "Assert"
      TAssert  _ _ _ _ _    ->  "Assert"
      CJudge   _ _ _ _      ->  "Judge"
      CComment _            ->  "Comment"
      CUnknown _            ->  "Unknown"
      CUnres   _ _          ->  "Unres"

{-| Source code information of clause. -}
clauseSource :: Clause -> C.ClauseSource
clauseSource c =
    case c of
      CSection src _        ->  src
      CImport  src _ _      ->  src
      CExport  src _        ->  src
      CShort   src _        ->  src
      CRelmap  src _ _      ->  src
      TRelmap  src _ _      ->  src
      CAssert  src _ _ _ _  ->  src
      TAssert  src _ _ _ _  ->  src
      CJudge   src _ _ _    ->  src
      CComment src          ->  src
      CUnknown src          ->  src
      CUnres   src _        ->  src

isCImport :: Clause -> Bool
isCImport (CImport _ _ _)     = True
isCImport _                   = False

isCExport :: Clause -> Bool
isCExport (CExport _ _)       = True
isCExport _                   = False

isCShort :: Clause -> Bool
isCShort (CShort _ _)         = True
isCShort _                    = False

isTShort :: B.Token -> Bool
isTShort (B.TShort _ _ _)     = True
isTShort _                    = False

isCRelmap :: Clause -> Bool
isCRelmap (CRelmap _ _ _)     = True
isCRelmap _                   = False

isCAssert :: Clause -> Bool
isCAssert (CAssert _ _ _ _ _) = True
isCAssert _                   = False

isCJudge :: Clause -> Bool
isCJudge (CJudge _ _ _ _)     = True
isCJudge _                    = False

isCUnknown :: Clause -> Bool
isCUnknown (CUnknown _)       = True
isCUnknown _                  = False

isCUnres :: Clause -> Bool
isCUnres (CUnres _ _)         = True
isCUnres _                    = False


-- ----------------------  Preconstruction

{-| Convert token list into clause list.
    Result clause list does not contain
    'CRelmap' and 'CAssert'. Instead of them,
    'TRelmap' and 'TAssert' are contained.
    This function does not depend on 'C.RelmapHalfCons'.

    >>> consPreclause . B.tokenize $ "a : source A /x /y"
    [TRelmap (ClauseSource
                 [TWord 1 0 "a", TSpace 2 1, ..., TTerm 11 ["/y"]]
                 [CodeLine 1 "a : source A /x /y" [TWord 1 0 "a", ...]])
             "a" [ TreeL (TWord 5 0 "source")
                 , TreeL (TWord 7 0 "A")
                 , TreeL (TTerm 9  ["/x"])
                 , TreeL (TTerm 11 ["/y"]) ]]
    -}
consPreclause :: [B.TokenLine] -> [Clause]
consPreclause = concatMap consPreclause' . C.clausify

consPreclause' :: C.ClauseSource -> [Clause]
consPreclause' src@(C.ClauseSource toks _) = clause $ B.sweepToken toks where
    clause :: [B.Token] -> [Clause]
    clause (B.TWord _ 0 "|" : B.TWord _ 0 k : xs) =
        frege k xs  -- frege's judgement stroke
    clause (B.TWord _ 0 n   : B.TWord _ 0 k : xs)
        | isDelim k       =  rel n xs
    clause (B.TWord _ 0 k : xs)
        | k == "section"  =  sec xs
        | k == "import"   =  imp xs
        | k == "export"   =  exp xs
        | k == "short"    =  short xs
        | k == "****"     =  [CComment src]
    clause []             =  []
    clause _              =  unk

    unk                   =  [CUnknown src]

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

    jud q (B.TWord _ _ p : xs) = [CJudge src q p xs]
    jud _ _     =  unk

    ass t (B.TWord _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  ->  a expr opt
          Left  expr            ->  a expr []
        where a expr opt =
                  let opt' = C.sortOperand $ B.tokenTrees opt
                  in [TAssert src t p opt' expr]
    ass _ _               =  unk

    rel n expr            =  [TRelmap src n expr]

    sec [B.TWord _ _ n]   =  [CSection src $ Just n]
    sec []                =  [CSection src Nothing]
    sec _                 =  unk

    exp (B.TWord _ _ n : B.TWord _ _ ":" : xs) = CExport src n : rel n xs
    exp [B.TWord _ _ n]   =  [CExport src n]
    exp _                 =  unk

    imp _                 =  [CImport src toks Nothing]

    short [B.TWord _ _ a, B.TWord _ _ b] = [CShort src (a, b)]
    short _               =  unk



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'B.Token' list.
    This is a first step of constructing 'C.Section'. -}
consClause
    :: C.RelmapHalfCons    -- ^ Relmap half constructor
    -> [B.TokenLine]       -- ^ Source tokens
    -> B.AbortOr [Clause]  -- ^ Result clauses
consClause half = clauseHalf half . consPreclause

clauseHalf :: C.RelmapHalfCons -> [Clause] -> B.AbortOr [Clause]
clauseHalf half xs = mapM f xs2 where
    f (TRelmap src n ts)       = Right . CRelmap src n       =<< h src ts
    f (TAssert src q p opt ts) = Right . CAssert src q p opt =<< h src ts
    f clause                   = Right clause

    h src ts = let ls = C.clauseLines src
               in case half ls (B.tokenTrees ts) of
                    Right r -> Right r
                    Left  a -> Left (a, ts, ls)

    xs2 = concatMap resolve xs
    resolve = resolveClause $ concatMap short xs
    short (CShort _ p) = [p]
    short _ = []

resolveClause :: [B.Named String] -> Clause -> [Clause]
resolveClause shorts = f where
    f (CJudge  src q p xs)     = resolve src xs $ CJudge  src q p
    f (TAssert src q p opt xs) = resolve src xs $ TAssert src q p opt
    f (TRelmap src n xs)       = resolve src xs $ TRelmap src n
    f clause                   = [clause]

    resolve src xs k = let xs2 = map (resolveShort shorts) xs
                       in k xs2 : unres src xs2
    unres src xs2    = case filter isTShort xs2 of
                         []  -> []
                         xs3 -> [CUnres src xs3]

resolveShort :: [B.Named String] -> B.Map B.Token
resolveShort shorts = f where
    f token@(B.TShort n a b) =
        case lookup a shorts of
          Just l  -> B.TWord n 2 $ l ++ b
          Nothing -> token
    f token = token



-- ----------------------  Full construction

{-| Second step of constructing 'C.Section'. -}
consSection
    :: (C.CContent c)
    => C.RelmapFullCons c      -- ^ Relmap full constructor
    -> String                  -- ^ Resource name
    -> [Clause]                -- ^ Output of 'consClause'
    -> B.AbortOr (C.Section c) -- ^ Result section
consSection full resource xs =
    do _        <-  mapMFor unk    isCUnknown
       _        <-  mapMFor unres  isCUnres
       imports  <-  mapMFor imp    isCImport
       judges   <-  mapMFor judge  isCJudge 
       relmaps  <-  mapMFor relmap isCRelmap
       asserts  <-  mapMFor assert isCAssert
       Right $ C.emptySection
           { C.sectionName      =  section xs
           , C.sectionImport    =  imports
           , C.sectionExport    =  mapFor exp   isCExport
           , C.sectionShort     =  mapFor short isCShort
           , C.sectionAssert    =  asserts
           , C.sectionRelmap    =  relmaps
           , C.sectionJudge     =  judges
           , C.sectionResource  =  resource }
    where
      mapFor  f p = map  f $ filter p xs
      mapMFor f p = mapM f $ filter p xs
      consSec = consSection full ""

      -- todo: multiple section name
      section (CSection _ n : _) = n
      section (_ : xs2) = section xs2
      section [] = Nothing

      imp (CImport _ _ (Nothing)) = Right C.emptySection
      imp (CImport _ _ (Just e))  = consSec [e]
      imp _                       = B.bug

      exp (CExport _ n)   = n
      exp _               = B.bug

      short (CShort _ p)  = p
      short _             = B.bug

      judge (CJudge src q p xs2) =
          case C.litJudge q p (B.tokenTrees xs2) of
            Right j -> Right j
            Left  a -> abort a [] src
      judge _ = B.bug

      relmap (CRelmap src n r) =
          case full r of
            Right r'     -> Right (n, r')
            Left (a, ts) -> abort a ts src
      relmap _ = B.bug

      assert (CAssert src t p opt r) =
          let ls = C.clauseLines src
          in case full r of
               Right r'     -> Right $ C.Assert t p opt r' ls
               Left (a, ts) -> abort a ts src
      assert _ = B.bug

      unk (CUnknown src) = abort B.AbortUnkClause [] src
      unk _ = B.bug

      unres (CUnres src ts) = abort B.AbortUnresToken ts src
      unres _ = B.bug

      abort a ts src = Left (a, ts, C.clauseLines src)



-- ----------------------
{- $Documentation

   There are six types of 'Clause'.
   Textual representation of 'C.Section' is a list of clauses.
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

   [@****@ blah blah blah ...]
     Comment clause

-}


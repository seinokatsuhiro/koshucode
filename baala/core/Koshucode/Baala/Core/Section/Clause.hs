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

import Data.Generics
import Prelude hiding (exp, mod)

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content          as C
import qualified Koshucode.Baala.Core.Relmap           as C
import qualified Koshucode.Baala.Core.Section.Clausify as C
import qualified Koshucode.Baala.Core.Section.Section  as C

data Clause
    = CSection C.ClauseSource (Maybe String)            -- ^ Section name
    | CImport  C.ClauseSource [B.Token] (Maybe Clause)  -- ^ Importing section name
    | CExport  C.ClauseSource String                    -- ^ Exporting relmap name
    | CRelmap  C.ClauseSource String C.HalfRelmap       -- ^ Relmap and its name
    | TRelmap  C.ClauseSource String [B.TokenTree]      -- ^ Not include HalfRelmap
    | CAssert  C.ClauseSource Bool String C.AssertOption C.HalfRelmap  -- ^ Assertions of relmaps
    | TAssert  C.ClauseSource Bool String C.AssertOption [B.TokenTree] -- ^ Not include HalfRelmap
    | CJudge   C.ClauseSource Bool String [B.Token]     -- ^ Judge
    | CComment C.ClauseSource       -- ^ Caluse comment
    | CUnknown C.ClauseSource       -- ^ Unknown clause
      deriving (Show, Data, Typeable)

{-| Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@. -}
clauseTypeText :: Clause -> String
clauseTypeText c =
    case c of
      CSection _ _          ->  "Section"
      CImport  _ _ _        ->  "Import"
      CExport  _ _          ->  "Export"
      CRelmap  _ _ _        ->  "Relmap"
      TRelmap  _ _ _        ->  "Relmap"
      CAssert  _ _ _ _ _    ->  "Assert"
      TAssert  _ _ _ _ _    ->  "Assert"
      CJudge   _ _ _ _      ->  "Judge"
      CComment _            ->  "Comment"
      CUnknown _            ->  "Unknown"

{-| Source code information of clause. -}
clauseSource :: Clause -> C.ClauseSource
clauseSource c =
    case c of
      CSection src _        ->  src
      CImport  src _ _      ->  src
      CExport  src _        ->  src
      CRelmap  src _ _      ->  src
      TRelmap  src _ _      ->  src
      CAssert  src _ _ _ _  ->  src
      TAssert  src _ _ _ _  ->  src
      CJudge   src _ _ _    ->  src
      CComment src          ->  src
      CUnknown src          ->  src

isCImport :: Clause -> Bool
isCImport (CImport _ _ _)     = True
isCImport _                   = False

isCExport :: Clause -> Bool
isCExport (CExport _ _)       = True
isCExport _                   = False

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
    clause (B.TWord _ 0 "|" : B.TWord _ 0 k : xs) = frege k xs
    clause (B.TWord _ 0 n   : B.TWord _ 0 k : xs)
        | isDelim k       =  rel n xs
    clause (B.TWord _ 0 k : xs)
        | k == "section"  =  sec xs
        | k == "import"   =  imp xs
        | k == "export"   =  exp xs
        | k == "****"     =  [CComment src]
    clause []             =  []
    clause _              =  unk

    unk                   =  [CUnknown src]

    isDelim     =  (`elem` ["|", ":"])

    frege "--"  =  jud True
    frege "-"   =  jud True
    frege "-X"  =  jud False
    frege "-x"  =  jud False

    frege "=="  =  ass True
    frege "="   =  ass True
    frege "=X"  =  ass False
    frege "=x"  =  ass False

    frege _     =  const unk

    jud q (B.TWord _ _ p : xs) = [CJudge src q p xs]
    jud _ _     =  unk

    ass q (B.TWord _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (opt, _, expr)  ->  a expr opt
          Left  expr            ->  a expr []
        where a expr opt =
                  let opt'  = C.sortOperand $ B.tokenTrees opt
                      expr' = B.tokenTrees expr
                  in [TAssert src q p opt' expr']
    ass _ _               =  unk

    rel n expr            =  let expr' = B.tokenTrees expr
                             in [TRelmap src n expr']

    sec [B.TWord _ _ n]   =  [CSection src $ Just n]
    sec []                =  [CSection src Nothing]
    sec _                 =  unk

    exp (B.TWord _ _ n : B.TWord _ _ ":" : xs) = CExport src n : rel n xs
    exp [B.TWord _ _ n]   =  [CExport src n]
    exp _                 =  unk

    imp _                 =  [CImport src toks Nothing]



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'B.Token' list.
    This is a first step of constructing 'C.Section'. -}
consClause
    :: C.RelmapHalfCons  -- ^ Relmap half constructor
    -> [B.TokenLine]     -- ^ Source tokens
    -> [Clause]          -- ^ Result clauses
consClause half = clauseHalf half . consPreclause

clauseHalf :: C.RelmapHalfCons -> B.Map [Clause]
clauseHalf half = map f where
    f (TRelmap src n ts)       = CRelmap src n       $ h src ts
    f (TAssert src q p opt ts) = CAssert src q p opt $ h src ts
    f x = x
    h src = half (C.clauseLines src)



-- ----------------------  Full construction

{-| Second step of constructing 'C.Section'. -}
consSection
    :: (C.CContent c)
    => C.RelmapFullCons c      -- ^ Relmap full constructor
    -> String                  -- ^ Resource name
    -> [Clause]                -- ^ Output of 'consClause'
    -> B.AbortOr (C.Section c) -- ^ Result section
consSection whole resource xs =
    do _        <-  mapMFor unk isCUnknown
       imports  <-  mapMFor imp isCImport
       judges   <-  mapMFor jud isCJudge 
       relmaps  <-  mapMFor rel isCRelmap
       asserts  <-  mapMFor ass isCAssert
       Right $ C.emptySection
           { C.sectionName      =  sec xs
           , C.sectionImport    =  imports
           , C.sectionExport    =  mapFor exp isCExport
           , C.sectionAssert    =  asserts
           , C.sectionRelmap    =  relmaps
           , C.sectionJudge     =  judges
           , C.sectionResource  =  resource }
    where
      mapFor  f p = map  f $ filter p xs
      mapMFor f p = mapM f $ filter p xs
      consSec = consSection whole ""

      -- todo: multiple section name
      sec (CSection _ n : _) = n
      sec (_ : xs2) = sec xs2
      sec [] = Nothing

      imp (CImport _ _ (Nothing)) = Right C.emptySection
      imp (CImport _ _ (Just e))  = consSec [e]
      imp _ = B.bug

      exp (CExport _ n) = n
      exp _ = B.bug

      jud (CJudge src q p xs2) =
          case C.litJudge q p (B.tokenTrees xs2) of
            Right j -> Right j
            Left  a -> Left (a, [], C.clauseLines src)
      jud _ = B.bug

      rel (CRelmap src n r) =
          case whole r of
            Right r'     -> Right (n, r')
            Left (a, ts) -> Left (a, ts, C.clauseLines src)
      rel _ = B.bug

      ass (CAssert src q p opt r) =
          let ls = C.clauseLines src
          in case whole r of
               Right r'     -> Right $ C.Assert q p opt r' ls
               Left (a, ts) -> Left (a, ts, C.clauseLines src)
      ass _ = B.bug

      unk (CUnknown src) =
          Left (B.AbortUnkClause, [], C.clauseLines src)
      unk _ = B.bug



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


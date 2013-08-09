{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'Section'. -}

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
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap
import Koshucode.Baala.Core.Section.Clausify
import Koshucode.Baala.Core.Section.Section

data Clause
    = CSection ClauseSource (Maybe String)            -- ^ Section name
    | CImport  ClauseSource [B.Token] (Maybe Clause)  -- ^ Importing section name
    | CExport  ClauseSource String                    -- ^ Exporting relmap name
    | CRelmap  ClauseSource String HalfRelmap         -- ^ Relmap and its name
    | TRelmap  ClauseSource String [B.TokenTree]      -- ^ Not include HalfRelmap
    | CAssert  ClauseSource Bool String HalfRelmap    -- ^ Assertions of relmaps
    | TAssert  ClauseSource Bool String [B.TokenTree] -- ^ Not include HalfRelmap
    | CJudge   ClauseSource Bool String [B.Token]     -- ^ Judge
    | CComment ClauseSource       -- ^ Caluse comment
    | CUnknown ClauseSource       -- ^ Unknown clause
      deriving (Show, Data, Typeable)

{-| Name of clause type. e.g., @\"Relmap\"@, @\"Assert\"@. -}
clauseTypeText :: Clause -> String
clauseTypeText c =
    case c of
      CSection _ _         ->  "Section"
      CImport  _ _ _       ->  "Import"
      CExport  _ _         ->  "Export"
      CRelmap  _ _ _       ->  "Relmap"
      TRelmap  _ _ _       ->  "Relmap"
      CAssert  _ _ _ _     ->  "Assert"
      TAssert  _ _ _ _     ->  "Assert"
      CJudge   _ _ _ _     ->  "Judge"
      CComment _           ->  "Comment"
      CUnknown _           ->  "Unknown"

{-| Source code information of clause. -}
clauseSource :: Clause -> ClauseSource
clauseSource c =
    case c of
      CSection src _       ->  src
      CImport  src _ _     ->  src
      CExport  src _       ->  src
      CRelmap  src _ _     ->  src
      TRelmap  src _ _     ->  src
      CAssert  src _ _ _   ->  src
      TAssert  src _ _ _   ->  src
      CJudge   src _ _ _   ->  src
      CComment src         ->  src
      CUnknown src         ->  src

isCImport :: Clause -> Bool
isCImport (CImport _ _ _)   = True
isCImport _                 = False

isCExport :: Clause -> Bool
isCExport (CExport _ _)     = True
isCExport _                 = False

isCRelmap :: Clause -> Bool
isCRelmap (CRelmap _ _ _)   = True
isCRelmap _                 = False

isCAssert :: Clause -> Bool
isCAssert (CAssert _ _ _ _) = True
isCAssert _                 = False

isCJudge :: Clause -> Bool
isCJudge (CJudge _ _ _ _)   = True
isCJudge _                  = False

isCUnknown :: Clause -> Bool
isCUnknown (CUnknown _)     = True
isCUnknown _                = False



-- ----------------------  Preconstruction

{-| Convert token list into clause list.
    Result clause list does not contain
    'CRelmap' and 'CAssert'. Instead of them,
    'TRelmap' and 'TAssert' are contained.
    This function does not depend on 'RelmapHalfCons'.

    >>> consPreclause $ tokens "a : source A /x /y"
    [TRelmap [CodeLine 1 "a : source A /x /y"]
             "a" [TreeL (Word 0 "source"),
                  TreeL (Word 0 "A"),
                  TreeL (TermN ["/x"]),
                  TreeL (TermN ["/y"])]]
    -}
consPreclause :: [B.CodeLine] -> [Clause]
consPreclause = concatMap consPreclause' . clausify

consPreclause' :: ClauseSource -> [Clause]
consPreclause' src@(ClauseSource toks _) = cl toks' where
    toks' = B.sweepToken toks

    cl :: [B.Token] -> [Clause]
    cl (B.TWord _ 0 n : B.TWord _ 0 ":" : xs) = rel n xs
    cl (B.TWord _ 0 k : xs)
        | k == "section"  = mod xs
        | k == "import"   = imp xs
        | k == "export"   = exp xs
        | k == "affirm"   = ass True  xs
        | k == "deny"     = ass False xs
        | k == "|--"      = jud True  xs
        | k == "|-"       = jud True  xs
        | k == "|-X"      = jud False xs
        | k == "|-x"      = jud False xs
        | k == "****"     = [CComment src]
    cl []                 = []
    cl _                  = unk

    unk                   = [CUnknown src]

    mod [B.TWord _ _ n]   = [CSection src $ Just n]
    mod []                = [CSection src Nothing]
    mod _                 = unk

    exp [B.TWord _ _ n]   = [CExport src n]
    exp (B.TWord _ _ n : B.TWord _ _ ":" : xs) = CExport src n : rel n xs
    exp _                 = unk

    imp _                 = [CImport src toks Nothing]

    rel n xs              = [TRelmap src n $ B.tokenTrees xs]

    jud q (B.TWord _ _ s : xs) = [CJudge src q s xs]
    jud _ _ = unk

    ass q (B.TWord _ _ s : xs) = [TAssert src q s $ B.tokenTrees xs]
    ass _ _ = unk



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'Token' list.
    This is a first step of constructing 'Section'. -}
consClause
    :: RelmapHalfCons  -- ^ Relmap half constructor
    -> [B.CodeLine]    -- ^ Source tokens
    -> [Clause]        -- ^ Result clauses
consClause half = clauseHalf half . consPreclause

clauseHalf :: RelmapHalfCons -> [Clause] -> [Clause]
clauseHalf half = map f where
    f (TRelmap src n ts)   = CRelmap src n   $ half (clauseLines src) ts
    f (TAssert src q s ts) = CAssert src q s $ half (clauseLines src) ts
    f x = x




-- ----------------------  Full construction

{-| Second step of constructing 'Section'. -}
consSection
    :: (CContent c)
    => RelmapFullCons c    -- ^ Relmap full constructor
    -> String              -- ^ Resource name
    -> [Clause]            -- ^ Output of 'consClause'
    -> B.AbortOr (Section c) -- ^ Result section
consSection whole res xs =
    do _        <-  mapMFor unk isCUnknown
       imports  <-  mapMFor imp isCImport
       judges   <-  mapMFor jud isCJudge 
       relmaps  <-  mapMFor rel isCRelmap
       asserts  <-  mapMFor ass isCAssert
       Right $ emptySection
           { sectionName      =  sec xs
           , sectionImport    =  imports
           , sectionExport    =  mapFor exp isCExport
           , sectionAssert    =  asserts
           , sectionRelmap    =  relmaps
           , sectionJudge     =  judges
           , sectionResource  =  res }
    where
      mapFor  f p = map  f $ filter p xs
      mapMFor f p = mapM f $ filter p xs
      consSec = consSection whole ""

      -- todo: multiple section name
      sec (CSection _ n : _) = n
      sec (_ : xs2) = sec xs2
      sec [] = Nothing

      imp (CImport _ _ (Nothing)) = Right emptySection
      imp (CImport _ _ (Just e))  = consSec [e]
      imp _ = B.bug

      exp (CExport _ n) = n
      exp _ = B.bug

      jud (CJudge src q p xs2) =
          case litJudge q p (B.tokenTrees xs2) of
            Right j -> Right j
            Left  a -> Left (a, clauseLines src, [])
      jud _ = B.bug

      rel (CRelmap _ n r) = Right . (n,) =<< whole r
      rel _ = B.bug

      ass (CAssert _ q p r) = Right . Assert q p =<< whole r
      ass _ = B.bug

      unk (CUnknown src) = Left (B.AbortUnknownClause, clauseLines src, [])
      unk _ = B.bug



-- ----------------------
{- $Documentation

   There are six types of 'Clause'.
   Textual representation of 'Section' is a list of clauses.
   'consClause' constructs clause list from section text.

   [name @:@ relmap]
     Relmap clause

   [@affirm@ pattern relmap]
     Affirmative assertion clause

   [@deny@ pattern relmap]
     Denial assertion clause

   [@|--@ pattern \/name content ...]
     Affirmative judgement clause

   [@|-X@ pattern \/name content ...]
     Denial judgement clause

   [@****@ blah blah blah ...]
     Comment clause

-}


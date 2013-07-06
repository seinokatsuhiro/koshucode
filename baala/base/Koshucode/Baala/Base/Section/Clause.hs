{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'Section'. -}

module Koshucode.Baala.Base.Section.Clause
( -- * Datatype
  -- $Documentation
  Clause (..)
, clauseTypeText
, clauseSource

  -- * Constructors
, consPreclause
, consClause
, consSection
) where

import Data.Generics
import Prelude hiding (exp, mod)

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Relmap
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Section.Clausify
import Koshucode.Baala.Base.Section.ConsJudge
import Koshucode.Baala.Base.Section.Section

-- Synthesis process
--
--   make half section :: [Token] -> [HalfSection]
--   make half relmap  :: [Token] -> [HalfRelmap]
--   make full section :: [HalfSection] -> Section v
--   make full relmap  :: HalfRelmap    -> Relmap v

data Clause
    = CSection ClauseSource (Maybe String)          -- ^ Section name
    | CImport  ClauseSource [Token] (Maybe Clause)  -- ^ Importing section name
    | CExport  ClauseSource String                  -- ^ Exporting relmap name
    | CRelmap  ClauseSource String HalfRelmap       -- ^ Relmap and its name
    | TRelmap  ClauseSource String [TokenTree]      -- ^ Not include HalfRelmap
    | CAssert  ClauseSource Bool String HalfRelmap  -- ^ Assertions of relmaps
    | TAssert  ClauseSource Bool String [TokenTree] -- ^ Not include HalfRelmap
    | CJudge   ClauseSource Bool String [Token]     -- ^ Judge
    | CComment ClauseSource       -- ^ Caluse comment
    | CUnknown ClauseSource       -- ^ Unknown clause
      deriving (Show, Data, Typeable)

clauseTypeText :: Clause -> String
clauseTypeText c =
    case c of
      CSection _ _      -> "Section"
      CImport  _ _ _    -> "Import"
      CExport  _ _      -> "Export"
      CRelmap  _ _ _    -> "Relmap"
      TRelmap  _ _ _    -> "Relmap"
      CAssert  _ _ _ _  -> "Assert"
      TAssert  _ _ _ _  -> "Assert"
      CJudge   _ _ _ _  -> "Judge"
      CComment _        -> "Comment"
      CUnknown _        -> "Unknown"

clauseSource :: Clause -> ClauseSource
clauseSource c =
    case c of
      CSection s _      -> s
      CImport  s _ _    -> s
      CExport  s _      -> s
      CRelmap  s _ _    -> s
      TRelmap  s _ _    -> s
      CAssert  s _ _ _  -> s
      TAssert  s _ _ _  -> s
      CJudge   s _ _ _  -> s
      CComment s        -> s
      CUnknown s        -> s



-- ----------------------  Preconstruction

{-| Convert token list into clause list.
    Result clause list does not contain
    'CRelmap' and 'CAssert'. Instead of them,
    'TRelmap' and 'TAssert' are contained.
    This function does not depend on 'RelmapHalfCons'.

    >>> consPreclause $ tokens "a : source A /x /y"
    [TRelmap [SourceLine 1 "a : source A /x /y"]
             "a" [TreeL (Word 0 "source"),
                  TreeL (Word 0 "A"),
                  TreeL (TermN ["/x"]),
                  TreeL (TermN ["/y"])]]
    -}
consPreclause :: [SourceLine] -> [Clause]
consPreclause = concatMap consPreclause' . clausify

consPreclause' :: ClauseSource -> [Clause]
consPreclause' src@(ClauseSource toks _) = cl toks' where
    toks' = sweepToken toks

    cl :: [Token] -> [Clause]
    cl (TWord _ 0 n : TWord _ 0 ":" : xs) = rel n xs
    cl (TWord _ 0 k : xs)
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

    mod [TWord _ _ n]        = [CSection src $ Just n]
    mod []                = [CSection src Nothing]
    mod _                 = unk

    exp [TWord _ _ n]        = [CExport src n]
    exp (TWord _ _ n : TWord _ _ ":" : xs) = CExport src n : rel n xs
    exp _                 = unk

    imp _                 = [CImport src toks Nothing]

    rel n xs              = [TRelmap src n $ tokenTrees xs]

    jud q (TWord _ _ s : xs) = [CJudge src q s xs]
    jud _ _ = unk

    ass q (TWord _ _ s : xs) = [TAssert src q s $ tokenTrees xs]
    ass _ _ = unk

-- e1 = mapM_ print . consPreclause . tokens
-- e2 = e1 "section 'http://example.com/'"
-- e3 = e1 "import 'http://example.com/'"
-- e4 = e1 "export aa"
-- e5 = e1 "|-- A /x 0 /y 0"
-- e6 = e1 "a : source A /x /y"
-- e7 = e1 "a : @a"



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'Token' list.
    This is a first step of constructing 'Section'. -}
consClause
    :: RelmapHalfCons  -- ^ Relmap half constructor
    -> [SourceLine]    -- ^ Source tokens
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
    :: (Value v)
    => RelmapFullCons v    -- ^ Relmap full constructor
    -> [Clause]            -- ^ Output of 'consClause'
    -> AbortOr (Section v) -- ^ Result section
consSection whole xs = do
  _       <- unk xs
  imports <- sequence $ imp xs
  judges  <- sequence $ jud xs
  relmaps <- rel xs
  asserts <- ass xs
  Right $ emptySection {
              sectionName   = mod xs
            , sectionImport = imports
            , sectionExport = exp xs
            , sectionAssert = asserts
            , sectionRelmap = relmaps
            , sectionJudge  = judges
            }
    where
      consSec = consSection whole

      mod (CSection _ n : _) = n
      mod (_ : xs2) = mod xs2
      mod [] = Nothing

      imp (CImport _ _ (Nothing) : xs2) = Right emptySection : imp xs2
      imp (CImport _ _ (Just e)  : xs2) = consSec [e] : imp xs2
      imp xs2 = skip imp xs2

      exp (CExport _ n : xs2) = n : exp xs2
      exp xs2 = skip exp xs2

      jud (CJudge src q s xs2 : xs3) =
          consJudge src q s (tokenTrees xs2) : jud xs3
      jud xs2 = skip jud xs2

      rel (CRelmap _ n r : xs2) =
          do m  <- whole r
             ms <- rel xs2
             Right $ (n, m) : ms
      rel (_ : xs2) = rel xs2
      rel [] = Right []

      ass (CAssert _ q s r : xs2) =
          do a  <- whole r
             as <- ass xs2
             Right $ (Assert q s a) : as
      ass (_ : xs2) = ass xs2
      ass [] = Right []

      unk (CUnknown src : _) = Left $ AbortUnknownClause $ clauseLines src
      unk (_ : xs2) = unk xs2
      unk [] = Right []

skip :: ([a] -> [b]) -> [a] -> [b]
skip loop (_ : xs) = loop xs
skip _ [] = []



-- ----------------------
-- $Documentation
--
-- There are six types of 'Clause'.
-- Textual representation of 'Section' is a list of clauses.
-- 'consClause' constructs clause list from section text.
--
-- [name @:@ relmap]
--   Relmap clause
--
-- [@affirm@ relsign relmap]
--   Affirmative assertion clause
--
-- [@deny@ relsign relmap]
--   Denial assertion clause
--
-- [@|--@ relsign \/name content ...]
--   Affirmative judgement clause
--
-- [@|-X@ relsign \/name content ...]
--   Denial judgement clause
--
-- [@****@ ...]
--   Comment clause


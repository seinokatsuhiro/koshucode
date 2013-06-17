{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'Section'. -}

module Koshucode.Baala.Base.Struct.Half.Clause
( -- * Documentation
  -- $Documentation
  Clause (..)
, consClause
, consSection
) where

import Data.Generics
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude as Prelude
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.HalfRelmap
import Koshucode.Baala.Base.Struct.Full.Section
import Koshucode.Baala.Base.Struct.Half.RelmapCons
import Koshucode.Baala.Base.Syntax
import Prelude hiding (exp, mod)

-- Synthesis process
--
--   make half section :: [Token] -> [HalfSection]
--   make half relmap  :: [Token] -> [HalfRelmap]
--   make full section :: [HalfSection] -> Section v
--   make full relmap  :: HalfRelmap    -> Relmap v

data Clause
    = CSection [SourceLine] (Maybe String)          -- ^ Section name
    | CImport  [SourceLine] [Token] (Maybe Clause)  -- ^ Importing section name
    | CExport  [SourceLine] String                  -- ^ Exporting relmap name
    | CRelmap  [SourceLine] String HalfRelmap       -- ^ Relmap and its name
    | TRelmap  [SourceLine] String [TokenTree]      -- ^ Not include HalfRelmap
    | CAssert  [SourceLine] Bool String HalfRelmap  -- ^ Assertions of relmaps
    | TAssert  [SourceLine] Bool String [TokenTree] -- ^ Not include HalfRelmap
    | CJudge   [SourceLine] Bool String [Token]     -- ^ Here data
    | CUnknown [SourceLine]       -- ^ Unknown clause
      deriving (Show, Data, Typeable)



-- ----------------------  Half construction

{-| Construct 'Clause' list from 'Token' list.
    This is a first step of constructing 'Section'. -}
consClause
    :: RelmapHalfCons  -- ^ Relmap half constructor
    -> [Token]         -- ^ Source tokens
    -> [Clause]        -- ^ Result clauses
consClause half = clauseHalf half . concatMap clause . clausify

clauseHalf :: RelmapHalfCons -> [Clause] -> [Clause]
clauseHalf half = map f where
    f (TRelmap src n ts)   = CRelmap src n   $ half src ts
    f (TAssert src q s ts) = CAssert src q s $ half src ts
    f x = x

clause :: [Token] -> [Clause]
clause toks = cl toks' where
    toks' = sweepToken toks
    src   = sourceLines toks

    cl :: [Token] -> [Clause]
    cl (Word 0 n : Word 0 ":" : xs) = rel n xs
    cl (Word 0 k : xs)
        | k == "section"  = mod xs
        | k == "import"   = imp xs
        | k == "export"   = exp xs
        | k == "affirm"   = ass True  xs
        | k == "deny"     = ass False xs
        | k == "|--"      = jud True  xs
        | k == "|-"       = jud True  xs
        | k == "|-X"      = jud False xs
        | k == "|-x"      = jud False xs
    cl []                 = []
    cl _                  = unk

    unk                   = [CUnknown src]

    mod [Word _ n]        = [CSection src $ Just n]
    mod []                = [CSection src Nothing]
    mod _                 = unk

    exp [Word _ n]        = [CExport src n]
    exp (Word _ n : Word _ ":" : xs) = CExport src n : rel n xs
    exp _                 = unk

    imp _                 = [CImport src toks Nothing]

    rel n xs              = [TRelmap src n $ tokenTrees xs]

    jud q (Word _ s : xs) = [CJudge src q s xs]
    jud _ _ = unk

    ass q (Word _ s : xs) = [TAssert src q s $ tokenTrees xs]
    ass _ _ = unk

-- e1 = mapM_ print . clause . tokens
-- e2 = e1 "section 'http://example.com/'"
-- e3 = e1 "import 'http://example.com/'"
-- e4 = e1 "export aa"
-- e5 = e1 "|-- A /x 0 /y 0"
-- e6 = e1 "a : source A /x /y"
-- e7 = e1 "a : @a"



-- ----------------------  Full construction

{-| Second step of constructing 'Section'. -}
consSection
    :: (StringValue v)
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

      jud (CJudge _ q s xs2 : xs3) = judge q s xs2 : jud xs3
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

      unk (CUnknown src : _) = Left $ AbortUnknownClause src
      unk (_ : xs2) = unk xs2
      unk [] = Right []

skip :: ([a] -> [b]) -> [a] -> [b]
skip loop (_ : xs) = loop xs
skip _ [] = []

judge :: (StringValue v) => Bool -> Relsign -> [Token] -> AbortOr (Judge v)
judge q s xs = do
  xs' <- terms xs
  Right $ Judge q s xs'

-- Collect term name and content
terms :: (StringValue v) => [Token] -> AbortOr [(String, v)]
terms (TermN [n] : Word _ w : xs) = do
  xs' <- terms xs
  Right $ (n, stringValue w) : xs'
terms [] = Right []
terms (TermN ns : _) = Left $ AbortMalformedTerms [] (show ns) -- no content
terms (Word _ c : _) = Left $ AbortMalformedTerms [] (show c) -- no name
terms (x : _)        = Left $ AbortMalformedTerms [] (show x) -- ???



-- ----------------------
-- $Documentation
--
-- There are five types of 'Clause'.
-- Textual representation of 'Section' is a list of clauses.
-- 'consClause' constructs clause list from section text.
--
-- [name @:@ relmap]
--   Relmap clause
--
-- [@affirm@ relsign relmap]
--   Assertion clause
--
-- [@deny@ relsign relmap]
--   Assertion clause
--
-- [@|--@ relsign \/name content ...]
--   Judgement clause
--
-- [@|-X@ relsign \/name content ...]
--   Judgement clause


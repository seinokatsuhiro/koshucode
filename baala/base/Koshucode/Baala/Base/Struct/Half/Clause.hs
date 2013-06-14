{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Intermidiate structure between 'String' and 'Section'. -}

module Koshucode.Baala.Base.Struct.Half.Clause
( Clause
, consClause
, consFullModule
) where
import Data.Generics
import qualified Data.Maybe as Maybe
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude as Prelude
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Module
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.HalfRelmap
import Koshucode.Baala.Base.Syntax
import Prelude hiding (exp, mod)

-- Synthesis process
--
--   make half section :: [Token] -> [HalfSection]
--   make half relmap  :: [Token] -> [HalfRelmap]
--   make full section :: [HalfSection] -> Section v
--   make full relmap  :: HalfRelmap    -> Relmap v

data Clause
    = CModule  [SourceLine] (Maybe String)         -- ^ Module name
    | CImport  [SourceLine] [Token] (Maybe Clause) -- ^ Importing module name
    | CExport  [SourceLine] String                 -- ^ Exporting relmap name
    | CRelmap  [SourceLine] String HalfRelmap      -- ^ Relmap and its name
    | CAssert  [SourceLine] Bool String HalfRelmap -- ^ Assertions of relmaps
    | CJudge   [SourceLine] Bool String [Token]    -- ^ Here data
    | CUnknown [SourceLine]       -- ^ Unknown clause
      deriving (Show, Data, Typeable)



-- ----------------------  Half construction

sourceLines :: [Token] -> [SourceLine]
sourceLines xs = Maybe.mapMaybe sourceLine xs

sourceLine :: Token -> Maybe SourceLine
sourceLine (Line src) = Just src
sourceLine _ = Nothing

zeroLine :: SourceLine
zeroLine = SourceLine 0 ""

{-| First step of constructing 'Section'. -}
consClause
    :: RelmapHalfCons  -- ^ Relmap half constructor
    -> [Token]         -- ^ Source tokens
    -> [Clause]        -- ^ Result clauses
consClause relmap = concatMap (classify relmap) . gather clause

{-| Split into first clause and rest tokens -}
clause :: [Token] -> ([Token], [Token])
clause = loop zero where
    zero = Line zeroLine
    loop _  (ln@(Line _) : xs)   = loop ln xs
    loop ln ((Comment _) : xs)   = loop ln xs
    loop ln (Space i : xs) = clauseBody i ln xs -- initial indent is 'i' spaces
    loop ln xs             = clauseBody 0 ln xs -- no indent

clauseBody :: Int -> Token -> [Token] -> ([Token], [Token])
clauseBody i ln xs = ln `cons1` mid xs where
    -- middle of line
    mid xxs@(Line _ : _)    = beg xxs   -- next line
    mid (x : xs2)           = x `cons1` mid xs2
    mid xxs                 = ([], xxs)

    -- beginning of line
    beg (x1@(Line _) : x2@(Space n) : xs2)
        | n > i = x1 `cons1` (x2 `cons1` mid xs2) -- indented line
    beg xxs     = ([], xxs)                   -- non indented line

-- e1 = gather clause . tokens
-- e2 = e1 "a\nb\nc\n\n"
-- e3 = e1 "a\n b\nc\n"
-- e4 = e1 " a\n b\nc\n"
-- e5 = e1 " a\n  b\nc\n"
-- e6 = e1 " a\nb\nc\n"
-- e7 = e1 "\na\nb\n"
-- e8 = e1 "a\n\n b\nc\n"
-- e9 = e1 "a\n  \n b\nc\n"

classify :: RelmapHalfCons -> [Token] -> [Clause]
classify half toks = cl toks' where
    toks' = sweepToken toks
    src   = sourceLines toks

    halfRel :: [Token] -> HalfRelmap
    halfRel = consHalfRelmap half src . tokenTrees

    cl :: [Token] -> [Clause]
    cl (Word 0 n : Word 0 ":" : xs) = rel n xs
    cl (Word 0 k : xs)
        | k == "module"   = mod xs
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

    mod [Word _ n]        = [CModule src $ Just n]
    mod []                = [CModule src Nothing]
    mod _                 = unk

    exp [Word _ n]        = [CExport src n]
    exp (Word _ n : Word _ ":" : xs) = CExport src n : rel n xs
    exp _                 = unk

    imp _                 = [CImport src toks Nothing]

    rel n xs              = [CRelmap src n $ halfRel xs]

    jud q (Word _ s : xs) = [CJudge src q s xs]
    jud _ _ = unk

    ass q (Word _ s : xs) = [CAssert src q s $ halfRel xs]
    ass _ _ = unk

-- e1 = consClause $ makeSynth1 []
-- e2 = e1 "module 'http://example.com/'"
-- e3 = e1 "import 'http://example.com/'"
-- e4 = e1 "export aa"
-- e5 = e1 "|-- A /x 0 /y 0"
-- e6 = e1 "a : source A /x /y"
-- e7 = e1 "a : @a"



-- ----------------------  Full construction

{-| Second step of module construction. -}
consFullModule
    :: (StringValue v)
    => RelmapWholeCons v   -- ^ Relmap full constructor
    -> [Clause]            -- ^ Half modules (Output of 'consClause')
    -> AbortOr (Module v)  -- ^ Result full module
consFullModule whole xs = do
  _       <- unk xs
  imports <- sequence $ imp xs
  judges  <- sequence $ jud xs
  relmaps <- rel xs
  asserts <- ass xs
  Right $ emptyModule { moduleName   = mod xs
                      , moduleImport = imports
                      , moduleExport = exp xs
                      , moduleAssert = asserts
                      , moduleRelmap = relmaps
                      , moduleJudge  = judges
                      }
    where
      consRel = whole
      consMod = consFullModule whole

      mod (CModule _ n : _) = n
      mod (_ : xs2) = mod xs2
      mod [] = Nothing

      imp (CImport _ _ (Nothing) : xs2) = Right emptyModule : imp xs2
      imp (CImport _ _ (Just e)  : xs2) = consMod [e] : imp xs2
      imp xs2 = skip imp xs2

      exp (CExport _ n : xs2) = n : exp xs2
      exp xs2 = skip exp xs2

      jud (CJudge _ q s xs2 : xs3) = judge q s xs2 : jud xs3
      jud xs2 = skip jud xs2

      rel (CRelmap _ n r : xs2) =
          do m  <- consRel r
             ms <- rel xs2
             Right $ (n, m) : ms
      rel (_ : xs2) = rel xs2
      rel [] = Right []

      ass (CAssert _ q s r : xs2) =
          do a  <- consRel r
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


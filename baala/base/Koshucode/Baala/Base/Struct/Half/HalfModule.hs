{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Struct.Half.HalfModule
( HalfModule
, consHalfModule
, consFullModule
) where
import Data.Generics
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

-- todo: add source information
data HalfModule
    = HModule  (Maybe String)             -- ^ Module name
    | HImport  [Token] (Maybe HalfModule) -- ^ Importing module name
    | HExport  String                     -- ^ Exporting relmap name
    | HRelmap  String HalfRelmap          -- ^ Relmap and its name
    | HAssert  Bool String HalfRelmap     -- ^ Assertions of relmaps
    | HJudge   Bool String [Token]        -- ^ Here data
    | HUnknown [Token]                    -- ^ Unknown clause
      deriving (Show, Data, Typeable)



-- ----------------------  Half construction

-- | First step of module construction.
consHalfModule
    :: RelmapHalfCons  -- ^ Relmap half constructor
    -> [Token]         -- ^ Source tokens
    -> [HalfModule]    -- ^ Result half modules
consHalfModule relmap = concatMap (classify relmap) . gather clause

-- | Split into first clause and rest tokens
clause :: [Token] -> ([Token], [Token])
clause (Newline : xs) = clause xs        -- skipt empty line
clause (Space i : xs) = clauseBody i xs  -- initial indent is 'i' spaces
clause xs             = clauseBody 0 xs  -- no indent

clauseBody :: Int -> [Token] -> ([Token], [Token])
clauseBody i = mid where
    -- middle of line
    mid (x : xs) | x == Newline    = x `cons1` beg xs
                 | otherwise       = x `cons1` mid xs
    mid xxs                        = ([], xxs)

    -- beginning of line
    beg (x@Newline   : xs)         = x `cons1` beg xs  -- skip empty line
    beg (x@(Space n) : xs) | n > i = x `cons1` mid xs  -- indented line
    beg xxs                        = ([], xxs)     -- non indented line

-- e1 = clause . tokens
-- e2 = e1 "a\nb\nc\n"
-- e3 = e1 "a\n b\nc\n"
-- e4 = e1 " a\n b\nc\n"
-- e5 = e1 " a\n  b\nc\n"
-- e6 = e1 " a\nb\nc\n"
-- e7 = e1 "\na\nb\n"
-- e8 = e1 "a\n\n b\nc\n"
-- e9 = e1 "a\n  \n b\nc\n"

classify :: RelmapHalfCons -> [Token] -> [HalfModule]
classify half toks = halfMod toks' where
    toks' = sweepToken toks

    halfRel :: [Token] -> HalfRelmap
    halfRel = consHalfRelmap half . tokenTrees

    halfMod :: [Token] -> [HalfModule]
    halfMod (Word 0 n : Word 0 ":" : xs) = rel n xs
    halfMod (Word 0 k : xs)
        | k == "module"   = mod xs
        | k == "import"   = imp xs
        | k == "export"   = exp xs
        | k == "affirm"   = ass True  xs
        | k == "deny"     = ass False xs
        | k == "|--"      = jud True  xs
        | k == "|-"       = jud True  xs
        | k == "|-X"      = jud False xs
        | k == "|-x"      = jud False xs
    halfMod _             = unk

    unk                   = [HUnknown toks]

    mod [Word _ n]        = [HModule $ Just n]
    mod []                = [HModule Nothing]
    mod _                 = unk

    exp [Word _ n]        = [HExport n]
    exp (Word _ n : Word _ ":" : xs) = HExport n : rel n xs
    exp _                 = unk

    imp _                 = [HImport toks Nothing]

    rel n xs              = [HRelmap n $ halfRel xs]

    jud q (Word _ s : xs) = [HJudge q s xs]
    jud _ _ = unk

    ass q (Word _ s : xs) = [HAssert q s $ halfRel xs]
    ass _ _ = unk

-- e1 = consHalfModule $ makeSynth1 []
-- e2 = e1 "module 'http://example.com/'"
-- e3 = e1 "import 'http://example.com/'"
-- e4 = e1 "export aa"
-- e5 = e1 "|-- A /x 0 /y 0"
-- e6 = e1 "a : source A /x /y"
-- e7 = e1 "a : @a"



-- ----------------------  Full construction

-- | Second step of module construction.
consFullModule
    :: (StringValue v)
    => RelmapWholeCons v   -- ^ Relmap full constructor
    -> [HalfModule]       -- ^ Half modules (Output of 'consHalfModule')
    -> AbortOr (Module v) -- ^ Result full module
consFullModule whole xs = do
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

      mod (HModule n : _) = n
      mod (_:xs2) = mod xs2
      mod [] = Nothing

      imp (HImport _ (Nothing) : xs2) = Right emptyModule : imp xs2
      imp (HImport _ (Just e)  : xs2) = consMod [e] : imp xs2
      imp xs2 = skip imp xs2

      exp (HExport n : xs2) = n : exp xs2
      exp xs2 = skip exp xs2

      jud (HJudge q s xs2 : xs3) = judge q s xs2 : jud xs3
      jud xs2 = skip jud xs2

      rel (HRelmap n r : xs2) =
          do m  <- consRel r
             ms <- rel xs2
             Right $ (n, m) : ms
      rel (_ : xs2) = rel xs2
      rel [] = Right []

      ass (HAssert q s r : xs2) =
          do a  <- consRel r
             as <- ass xs2
             Right $ (Assert q s a) : as
      ass (_ : xs2) = ass xs2
      ass [] = Right []

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
terms (TermN ns : _) = Left $ AbortMalformedTerms (show ns) -- no content
terms (Word _ c : _) = Left $ AbortMalformedTerms (show c) -- no name
terms (x : _)        = Left $ AbortMalformedTerms (show x) -- ???


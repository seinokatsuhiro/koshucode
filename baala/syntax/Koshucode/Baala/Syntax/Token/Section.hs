{-# OPTIONS_GHC -Wall #-}

-- | Code section.

module Koshucode.Baala.Syntax.Token.Section
  ( -- * Section
    TokenRoll, TokenRollMap, ChangeSection,
    section,

    -- * Simple sections
    sectionEnd, sectionNote, sectionLicense,

    -- * Splitter
    Scan, ScanW,
    scan, scanW,
    scanSpace,
    scanSymbol,
    symbolToken,
    isSymbol,
    isSpace,
  ) where

import qualified Data.Map                               as Map
import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------

-- | Code roll for token.
type TokenRoll = B.CodeRoll S.Token

-- | Read single token.
type TokenRollMap = B.Map TokenRoll

type ChangeSection = String -> Maybe TokenRollMap

-- Line begins with the equal sign is treated as section delimter.
section :: ChangeSection -> (S.InputText -> TokenRoll) -> TokenRollMap
section change f
        r@B.CodeRoll { B.codeMap    = prev
                     , B.codeInput  = cs0
                     , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = B.codeChange (selectSection change prev) r
    st _ cs         = f cs

selectSection :: ChangeSection -> TokenRollMap -> TokenRollMap
selectSection change prev
              r@B.CodeRoll { B.codeInputPt  = cp
                           , B.codeInput    = cs0
                           , B.codeWords    = ws
                           } = sec cs0 where
    v      = scan r
    vw     = scanW r
    out    = reverse $ S.sweepToken $ B.codeOutput r
    --toPrev = B.codeChange prev

    sec ""               = dispatch out  -- end of line
    sec ('*' : '*' : _)  = dispatch out  -- end of effective text
    sec ccs@(c:cs)
        | isSpace c      = v  $ scanSpace  cp cs
        | isSymbol c     = vw $ scanSymbol cp ws ccs
        | otherwise      = sectionUnexp [] r

    dispatch :: [S.Token] -> TokenRoll
    dispatch [S.TTextSect _] = B.codeChange prev r
    dispatch ts@[S.TTextSect _, S.TTextRaw _ name] =
        case change name of
          Just ch -> ch r
          Nothing -> sectionUnexp ts r
    dispatch ts    = sectionUnexp ts r

sectionUnexp :: [S.Token] -> TokenRollMap
sectionUnexp ts r@B.CodeRoll { B.codeInput = cs } = B.codeUpdate "" tok r where
    tok  = S.unknownToken cp cs $ Msg.unexpSect help
    cp | null ts    = B.codeInputPt r
       | otherwise  = B.codePt $ head ts
    help = [ "=== rel      for relational calculation"
           , "=== note     for commentary section"
           , "=== license  for license section"
           , "=== end      for ending of input" ]


-- --------------------------------------------  Simple sections

-- | Tokenizer for end section.
sectionEnd :: TokenRollMap
sectionEnd r@B.CodeRoll { B.codeInput = cs } = comment cs r

-- | Tokenizer for note section.
sectionNote :: ChangeSection -> TokenRollMap
sectionNote change r = section change (`comment` r) r

comment :: S.InputText -> TokenRollMap
comment "" r = r
comment cs r = B.codeUpdate "" tok r where
    tok  = S.TComment cp cs
    cp   = B.codeInputPt r

-- | Tokenizer for license section.
sectionLicense :: ChangeSection -> TokenRollMap
sectionLicense change r = section change license r where
    license "" = r
    license cs = B.codeUpdate "" tok r where
        tok  = S.TText cp S.TextLicense cs
        cp   = B.codeInputPt r


-- --------------------------------------------  Scanner

-- | Split next token.
type Scan  = B.CodePt -> S.InputText -> (S.InputText, S.Token)

-- | Split next token with word table.
type ScanW = B.CodePt -> B.WordTable -> S.InputText -> (B.WordTable, S.InputText, S.Token)

-- | Update token scanner.
scan :: TokenRoll -> (S.InputText, S.Token) -> TokenRoll
scan r (cs, tok) = B.codeUpdate cs tok r

-- | Update token scanner with word table.
scanW :: TokenRoll -> (B.WordTable, S.InputText, S.Token) -> TokenRoll
scanW r (wtab, cs, tok) = B.codeUpdateWords wtab cs tok r

-- | Split space token.
scanSpace :: Scan
scanSpace cp cs =
    let (cs', n) = S.nextSpace cs
    in (cs', S.TSpace cp $ n + 1)

-- | Split symbolic token.
scanSymbol :: ScanW
scanSymbol cp wtab cs =
    let (cs', sym) = S.nextSymbol cs
    in case sym of
         S.SymbolShort pre w  -> (wtab, cs', S.TShort cp pre w)
         S.SymbolCommon    w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolGeneral   w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolPlain     w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolNumeric   w  -> symbolToken S.TTextRaw w cp wtab cs'
         S.SymbolUnknown   w  -> (wtab, [], S.unknownToken cp cs $ Msg.forbiddenInput w)

-- | Create symbolic token.
symbolToken :: (B.CodePt -> String -> S.Token) -> String -> ScanW
symbolToken k w cp wtab cs =
    case Map.lookup w wtab of
         Just w' -> (wtab, cs, k cp w')
         Nothing -> let wtab' = Map.insert w w wtab
                    in (wtab', cs, k cp w)

-- | Test character is symbolic.
isSymbol :: B.Pred Char
isSymbol = S.isSymbolChar

-- | Test character is space.
isSpace :: B.Pred Char
isSpace = Ch.isSpace


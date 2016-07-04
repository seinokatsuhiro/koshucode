{-# OPTIONS_GHC -Wall #-}

-- | Code section.

module Koshucode.Baala.Syntax.Token.Section
  ( -- * Section
    ChangeSection,
    section,

    -- * Simple scanner
    Scanner,
    scanEnd, scanNote, scanLicense,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Nipper    as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------

-- | Select scanner based on section name.
type ChangeSection = String -> Maybe S.TokenScanMap

-- Line begins with the equal sign is treated as section delimter.
section :: ChangeSection -> (S.InputText -> S.TokenScan) -> S.TokenScanMap
section change f
        sc@B.CodeScan { B.codeMap    = prev
                      , B.codeInput  = cs0
                      , B.codeOutput = out } = body out cs0 where
    body [] ('=' : _) = B.codeChange (selectSection change prev) sc
    body _ cs         = f cs

selectSection :: ChangeSection -> S.TokenScanMap -> S.TokenScanMap
selectSection change prev
              sc@B.CodeScan { B.codeInputPt  = cp
                            , B.codeInput    = cs0
                            , B.codeWords    = ws
                            } = sec cs0 where
    nip    = S.nipUpdate  sc
    nipw   = S.nipUpdateW sc
    out    = reverse $ S.sweepToken $ B.codeOutput sc
    --toPrev = B.codeChange prev

    sec ""               = dispatch out  -- end of line
    sec ('*' : '*' : _)  = dispatch out  -- end of effective text
    sec ccs@(c:cs)
        | S.isSpace c    = nip  $ S.nipSpace  cp cs
        | S.isSymbol c   = nipw $ S.nipSymbol cp ws ccs
        | otherwise      = sectionUnexp [] sc

    dispatch :: [S.Token] -> S.TokenScan
    dispatch [S.TTextSect _] = B.codeChange prev sc
    dispatch ts@[S.TTextSect _, S.TTextRaw _ name] =
        case change name of
          Just ch -> ch sc
          Nothing -> sectionUnexp ts sc
    dispatch ts    = sectionUnexp ts sc

sectionUnexp :: [S.Token] -> S.TokenScanMap
sectionUnexp ts sc@B.CodeScan { B.codeInput = cs } = B.codeUpdate "" tok sc where
    tok  = S.unknownToken cp cs $ Msg.unexpSect help
    cp | null ts    = B.codeInputPt sc
       | otherwise  = B.codePt $ head ts
    help = [ "=== rel      for relational calculation"
           , "=== note     for commentary section"
           , "=== license  for license section"
           , "=== end      for ending of input" ]


-- --------------------------------------------  Simple sections

-- | Token scanner.
type Scanner = ChangeSection -> S.TokenScanMap

-- | Scan tokens in @end@ section.
scanEnd :: Scanner
scanEnd _ sc@B.CodeScan { B.codeInput = cs } = comment cs sc

-- | Scan tokens in @note@ section.
scanNote :: Scanner
scanNote change sc = section change (`comment` sc) sc

comment :: S.InputText -> S.TokenScanMap
comment "" sc = sc
comment cs sc = B.codeUpdate "" tok sc where
    tok = S.TComment (B.codePt sc) cs

-- | Scan tokens in @license@ section.
scanLicense :: Scanner
scanLicense change sc = section change license sc where
    license "" = sc
    license cs = B.codeUpdate "" tok sc where
        tok = S.TText (B.codePt sc) S.TextLicense cs


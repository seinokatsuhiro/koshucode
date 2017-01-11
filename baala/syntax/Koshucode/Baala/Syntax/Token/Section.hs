{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Code section.

module Koshucode.Baala.Syntax.Token.Section
  ( -- * Section
    ChangeSection,
    section,

    -- * Simple scanner
    Scanner,
    scanEnd,
    scanNote,
    scanLicense,
    scanLine,
    --scanLineInClause,
    scanTextAssert,
  ) where

import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Clip      as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as P
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------

-- | Select scanner based on section name.
type ChangeSection = String -> Maybe S.TokenScanMap

-- | Line begins with triple equal signs is treated as section delimter.
section :: ChangeSection -> (S.InputText -> S.TokenScan) -> S.TokenScanMap
section change f
        sc@B.CodeScan { B.codeMap    = prev
                      , B.codeInput  = cs0
                      , B.codeOutput = out } = body out cs0 where
    body [] (O.tCut -> Just ('=', _)) = B.codeChange (selectSection change prev) sc
    body _ cs = f cs

selectSection :: ChangeSection -> S.TokenScanMap -> S.TokenScanMap
selectSection change prev
              sc@B.CodeScan { B.codeInputPt  = cp
                            , B.codeInput    = cs0
                            , B.codeWords    = ws
                            } = sec cs0 where
    clip   = S.clipUpdate  sc
    clipw  = S.clipUpdateC sc
    out    = reverse $ S.sweepToken $ B.codeOutput sc
    --toPrev = B.codeChange prev

    sec (O.tCut2 -> Just ('*', Just( '*', _)))
                         = dispatch out  -- end of effective text
    sec ccs@(O.tCut -> Just (c, cs))
        | S.isSpace c    = clip  $ S.clipSpace  cp cs
        | S.isSymbol c   = clipw $ S.clipSymbol cp ws ccs
        | otherwise      = sectionUnexp [] sc
    sec _                = dispatch out  -- end of line

    dispatch :: [S.Token] -> S.TokenScan
    dispatch [P.TSection] = B.codeChange prev sc
    dispatch ts@[P.TSection, P.TRaw name] =
        case change name of
          Just ch -> ch sc
          Nothing -> sectionUnexp ts sc
    dispatch ts    = sectionUnexp ts sc

sectionUnexp :: [S.Token] -> S.TokenScanMap
sectionUnexp ts sc@B.CodeScan { B.codeInput = cs } = B.codeUpdate "" tok sc where
    tok  = S.unknownToken cp cs $ Msg.unexpSect help
    cp | null ts    = B.codeInputPt sc
       | otherwise  = B.getCP $ head ts
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
comment cs sc
    | O.tIsEmpty cs = sc
    | otherwise     = B.codeUpdate "" tok sc
    where tok = S.TComment (B.getCP sc) cs

-- | Scan tokens in @license@ section.
scanLicense :: Scanner
scanLicense = scanLine S.TextLicense

-- | Scan entire line as text.
scanLine :: S.TextForm -> Scanner
scanLine form change sc = section change text sc where
    text cs | O.tIsEmpty cs = sc
            | otherwise     = let tok = S.TText (B.getCP sc) form cs
                              in B.codeUpdate "" tok sc

scanLineInClause :: S.TextForm -> Scanner
scanLineInClause form change sc = section change text sc where
    text cs@(O.tCut -> Just (c, _))
        | S.isSpace c = S.clipUpdate sc $ S.clipSpace (B.getCP sc) cs
        | B.isBol sc  = B.codeScanRestore sc
        | otherwise   = let tok = S.TText (B.getCP sc) form cs
                        in B.codeUpdate "" tok sc
    text _ = sc

-- | Section for @koshu-text-assert@ command.
scanTextAssert :: Scanner
scanTextAssert change sc = section change text sc where
    cp = B.getCP sc
    raw = S.TText cp S.TextRaw

    text ccs@(O.tCut -> Just (c, cs))
        | S.isSpace c  = S.clipUpdate  sc $ S.clipSpace cp ccs
        | c == '|'     = S.clipUpdate  sc $ S.clipBar cp cs
        | c == ':'     = B.codeChange (scanLineInClause S.TextRaw change)
                           $ B.codeScanSave $ S.clipUpdate sc (cs, raw [c])
        | otherwise    = case S.clipSymbol cp (B.codeWords sc) ccs of
                           (ws', _, P.TRaw "") ->
                               S.clipUpdateC sc (ws', cs, raw [c])
                           sym -> S.clipUpdateC sc sym
    text _ = sc


{-# OPTIONS_GHC -Wall #-}

-- | Code section.

module Koshucode.Baala.Syntax.Token.Section
  ( -- * Section
    ChangeSection,
    section,

    -- * Simple sections
    sectionEnd, sectionNote, sectionLicense,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Nipper    as S
import qualified Koshucode.Baala.Syntax.Token.Pattern   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Syntax.Token.Message   as Msg


-- --------------------------------------------

type ChangeSection = String -> Maybe S.TokenRollMap

-- Line begins with the equal sign is treated as section delimter.
section :: ChangeSection -> (S.InputText -> S.TokenRoll) -> S.TokenRollMap
section change f
        r@B.CodeRoll { B.codeMap    = prev
                     , B.codeInput  = cs0
                     , B.codeOutput = out } = st out cs0 where
    st [] ('=' : _) = B.codeChange (selectSection change prev) r
    st _ cs         = f cs

selectSection :: ChangeSection -> S.TokenRollMap -> S.TokenRollMap
selectSection change prev
              r@B.CodeRoll { B.codeInputPt  = cp
                           , B.codeInput    = cs0
                           , B.codeWords    = ws
                           } = sec cs0 where
    v      = S.scan r
    vw     = S.scanW r
    out    = reverse $ S.sweepToken $ B.codeOutput r
    --toPrev = B.codeChange prev

    sec ""               = dispatch out  -- end of line
    sec ('*' : '*' : _)  = dispatch out  -- end of effective text
    sec ccs@(c:cs)
        | S.isSpace c    = v  $ S.nipSpace  cp cs
        | S.isSymbol c   = vw $ S.nipSymbol cp ws ccs
        | otherwise      = sectionUnexp [] r

    dispatch :: [S.Token] -> S.TokenRoll
    dispatch [S.TTextSect _] = B.codeChange prev r
    dispatch ts@[S.TTextSect _, S.TTextRaw _ name] =
        case change name of
          Just ch -> ch r
          Nothing -> sectionUnexp ts r
    dispatch ts    = sectionUnexp ts r

sectionUnexp :: [S.Token] -> S.TokenRollMap
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
sectionEnd :: S.TokenRollMap
sectionEnd r@B.CodeRoll { B.codeInput = cs } = comment cs r

-- | Tokenizer for note section.
sectionNote :: ChangeSection -> S.TokenRollMap
sectionNote change r = section change (`comment` r) r

comment :: S.InputText -> S.TokenRollMap
comment "" r = r
comment cs r = B.codeUpdate "" tok r where
    tok  = S.TComment cp cs
    cp   = B.codeInputPt r

-- | Tokenizer for license section.
sectionLicense :: ChangeSection -> S.TokenRollMap
sectionLicense change r = section change license r where
    license "" = r
    license cs = B.codeUpdate "" tok r where
        tok  = S.TText cp S.TextLicense cs
        cp   = B.codeInputPt r


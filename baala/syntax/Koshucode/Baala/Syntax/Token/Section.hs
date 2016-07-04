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
        sc@B.CodeScan { B.codeMap    = prev
                      , B.codeInput  = cs0
                      , B.codeOutput = out } = body out cs0 where
    body [] ('=' : _) = B.codeChange (selectSection change prev) sc
    body _ cs         = f cs

selectSection :: ChangeSection -> S.TokenRollMap -> S.TokenRollMap
selectSection change prev
              sc@B.CodeScan { B.codeInputPt  = cp
                            , B.codeInput    = cs0
                            , B.codeWords    = ws
                            } = sec cs0 where
    v      = S.scan  sc
    vw     = S.scanW sc
    out    = reverse $ S.sweepToken $ B.codeOutput sc
    --toPrev = B.codeChange prev

    sec ""               = dispatch out  -- end of line
    sec ('*' : '*' : _)  = dispatch out  -- end of effective text
    sec ccs@(c:cs)
        | S.isSpace c    = v  $ S.nipSpace  cp cs
        | S.isSymbol c   = vw $ S.nipSymbol cp ws ccs
        | otherwise      = sectionUnexp [] sc

    dispatch :: [S.Token] -> S.TokenRoll
    dispatch [S.TTextSect _] = B.codeChange prev sc
    dispatch ts@[S.TTextSect _, S.TTextRaw _ name] =
        case change name of
          Just ch -> ch sc
          Nothing -> sectionUnexp ts sc
    dispatch ts    = sectionUnexp ts sc

sectionUnexp :: [S.Token] -> S.TokenRollMap
sectionUnexp ts sc@B.CodeScan { B.codeInput = cs } = B.codeUpdate "" tok sc where
    tok  = S.unknownToken cp cs $ Msg.unexpSect help
    cp | null ts    = B.codeInputPt sc
       | otherwise  = B.codePt $ head ts
    help = [ "=== rel      for relational calculation"
           , "=== note     for commentary section"
           , "=== license  for license section"
           , "=== end      for ending of input" ]


-- --------------------------------------------  Simple sections

-- | Tokenizer for end section.
sectionEnd :: S.TokenRollMap
sectionEnd sc@B.CodeScan { B.codeInput = cs } = comment cs sc

-- | Tokenizer for note section.
sectionNote :: ChangeSection -> S.TokenRollMap
sectionNote change sc = section change (`comment` sc) sc

comment :: S.InputText -> S.TokenRollMap
comment "" sc = sc
comment cs sc = B.codeUpdate "" tok sc where
    tok  = S.TComment cp cs
    cp   = B.codeInputPt sc

-- | Tokenizer for license section.
sectionLicense :: ChangeSection -> S.TokenRollMap
sectionLicense change sc = section change license sc where
    license "" = sc
    license cs = B.codeUpdate "" tok sc where
        tok  = S.TText cp S.TextLicense cs
        cp   = B.codeInputPt sc


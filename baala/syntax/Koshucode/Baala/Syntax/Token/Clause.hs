
-- | Token clause.

module Koshucode.Baala.Syntax.Token.Clause
  ( -- * Token clause
    TokenClause,
    tokenClauses,

    -- * Token line
    TokenLine,
    toks,
    tokenLines, tokenLinesBz,
    tokenLinesBzTextAssert,
    isShortPrefix,
  
    -- * Examples
    -- $Examples
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token.Nipper    as S
import qualified Koshucode.Baala.Syntax.Token.Rel       as S
import qualified Koshucode.Baala.Syntax.Token.Section   as S
import qualified Koshucode.Baala.Syntax.Token.Token     as S
import qualified Koshucode.Baala.Syntax.Token.Utility   as S
import qualified Koshucode.Baala.Base.Message           as Msg


-- --------------------------------------------  Token clause

-- | Code clause of tokens.
type TokenClause = B.CodeClause S.Token

-- | Convert token lines into token clauses
tokenClauses :: [TokenLine] -> [TokenClause]
tokenClauses = map clause . split where
    clause ls = B.CodeClause ls $ list ls

    list :: [TokenLine] -> [S.Token]
    list = concatMap $ S.sweepToken . B.lineTokens

    split :: [TokenLine] -> [[TokenLine]]
    split = B.gather B.splitClause . map indent . B.omit blank

    blank :: TokenLine -> Bool
    blank = all S.isBlankToken . B.lineTokens

    indent :: TokenLine -> (B.IndentSize, TokenLine)
    indent = B.lineIndentPair tokenIndent

tokenIndent :: S.Token -> Int
tokenIndent (S.TSpace _ n) = n
tokenIndent _ = 0


-- --------------------------------------------  Token line

-- | Token list on a line.
type TokenLine = B.CodeLine S.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.NIOPoint -> S.InputText -> [S.Token]
tokens nio cs = concatMap B.lineTokens $ tokenLines nio cs

-- | Abbreviated tokenizer.
toks :: S.InputText -> [S.Token]
toks s = tokens (B.nioFrom $ B.stringBz s) s

-- | Tokenize text.
tokenLines :: B.NIOPoint -> S.InputText -> [TokenLine]
tokenLines = tokenLinesWith S.scanRel

-- | Tokenize lazy bytestring.
tokenLinesBz :: B.NIOPoint -> B.Bz -> [TokenLine]
tokenLinesBz = tokenLinesBzWith S.scanRel

-- | Tokenize lazy bytestring.
tokenLinesBzTextAssert :: B.NIOPoint -> B.Bz -> [TokenLine]
tokenLinesBzTextAssert = tokenLinesBzWith S.scanTextAssert

tokenLinesWith :: S.Scanner -> B.NIOPoint -> S.InputText -> [TokenLine]
tokenLinesWith scan = B.codeScanUp $ scan changeSection

tokenLinesBzWith :: S.Scanner -> B.NIOPoint -> B.Bz -> [TokenLine]
tokenLinesBzWith scan = B.codeScanUpBz $ scan changeSection

changeSection :: S.ChangeSection
changeSection name =
    case name of
      "rel"      -> just S.scanRel
      "note"     -> just S.scanNote
      "end"      -> just S.scanEnd
      "license"  -> just S.scanLicense
      "local"    -> unsupp "local section"
      "attr"     -> unsupp "attr section"
      "text"     -> unsupp "text section"
      "doc"      -> unsupp "doc section"
      "data"     -> unsupp "data section"
      _          -> Nothing
    where
      just scan   = Just $ B.codeChange $ scan changeSection
      unsupp n    = Just $ sectionUnsupported n

sectionUnsupported :: String -> S.TokenScanMap
sectionUnsupported msg r@B.CodeScan { B.codeInput = cs } = B.codeUpdate "" tok r where
    tok  = S.unknownToken cp cs $ Msg.unsupported msg
    cp   = B.codeInputPt r

-- | Test string is short prefix.
isShortPrefix :: O.Test String
isShortPrefix  = all Ch.isAlpha


-- ------------------------------------------------------------------
-- $Examples
--
--  Words and quotations.
--
--  >>> toks $ unlines ["aa", "'bb'", "\"cc\""]
--  [ TText <I0-L1-C0> TextRaw "aa"
--  , TText <I0-L2-C0> TextQ "bb"
--  , TText <I0-L2-C3> TextQ ""
--  , TText <I0-L3-C0> TextQQ "cc" ]
--
--  Judgement.
--
--  >>> toks "|-- R  /a A0 /b 31"
--  [ TText <I0-L1-C0> TextBar "|--", TSpace <I0-L1-C3> 1
--  , TText <I0-L1-C4> TextRaw "R", TSpace <I0-L1-C5> 2
--  , TTermN <I0-L1-C7> EQ "a", TSpace <I0-L1-C9> 1
--  , TText <I0-L1-C10> TextRaw "A0", TSpace <I0-L1-C12> 1
--  , TTermN <I0-L1-C13> EQ "b", TSpace <I0-L1-C15> 1
--  , TText <I0-L1-C16> TextRaw "31" ]
--
--  Brackets.
--
--  >>> toks "aa (bb x y (z))"
--  [ TText <I0-L1-C0> TextRaw "aa"
--  , TSpace <I0-L1-C2> 1
--  , TOpen <I0-L1-C3> "("
--    , TText <I0-L1-C4> TextRaw "bb", TSpace <I0-L1-C6> 1
--    , TText <I0-L1-C7> TextRaw "x", TSpace <I0-L1-C8> 1
--    , TText <I0-L1-C9> TextRaw "y", TSpace <I0-L1-C10> 1
--    , TOpen <I0-L1-C11> "("
--      , TText <I0-L1-C12> TextRaw "z"
--    , TClose <I0-L1-C13> ")"
--  , TClose <I0-L1-C14> ")" ]
--
--  A comment.
--
--  >>> toks $ unlines ["abc ** this is a comment", "def",""]
--  [ TText <I0-L1-C0> TextRaw "abc", TSpace <I0-L1-C3> 1
--  , TComment <I0-L1-C4> " this is a comment"
--  , TText <I0-L2-C0> TextRaw "def" ]
--

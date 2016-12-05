
-- | Token line and clause.

module Koshucode.Baala.Syntax.Token.Clause
  ( -- * Token line
    TokenLine,
    tokenLines, tokenLinesBz,
    tokenLinesBzTextAssert,
    isShortPrefix,
    readTokenLines,
  
    -- * Token clause
    TokenClause,
    tokenClauses,
    readClauses,

    -- * Abbreviation
    toks,
    toksPrint,
    clausePrint,
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


-- --------------------------------------------  Token line

-- | Token list on a line.
type TokenLine = B.CodeLine S.Token

-- | Split string into list of tokens.
--   Result token list does not contain newline characters.
tokens :: B.IxIOPoint -> S.InputText -> [S.Token]
tokens nio cs = concatMap B.lineTokens $ tokenLines nio cs

-- | Tokenize text.
tokenLines :: B.IxIOPoint -> S.InputText -> [TokenLine]
tokenLines = tokenLinesWith S.scanRel

-- | Tokenize lazy bytestring.
tokenLinesBz :: (B.ToCode code) => B.IxIOPoint -> code -> [TokenLine]
tokenLinesBz = tokenLinesBzWith S.scanRel

-- | Tokenize lazy bytestring.
tokenLinesBzTextAssert :: (B.ToCode code) => B.IxIOPoint -> code -> [TokenLine]
tokenLinesBzTextAssert = tokenLinesBzWith S.scanTextAssert

tokenLinesWith :: S.Scanner -> B.IxIOPoint -> S.InputText -> [TokenLine]
tokenLinesWith scan = B.codeScanUp $ scan changeSection

-- | Tokenize with code scanner.
tokenLinesBzWith :: (B.ToCode code) => S.Scanner -> B.IxIOPoint -> code -> [TokenLine]
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

-- | Read token lines from file.
readTokenLines :: FilePath -> IO (B.Ab [TokenLine])
readTokenLines path =
    do file <- B.readBzFile path
       return $ case B.bzFileException file of
         Nothing -> Right $ tokenLinesBz (B.pathIxIO path) (B.bzFileContent file)
         Just e  -> Left $ B.abortLines "Abort when reading file" [show e]


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

-- | Indent level.
tokenIndent :: S.Token -> Int
tokenIndent (S.TSpace _ n) = n
tokenIndent             _  = 0

-- | Read clause list from file.
--
-- Content of file @clause.txt@:
--
--   > **
--   > **  This is an example file for 'readClauses' function
--   > **  defined in Koshucode.Baala.Syntax.Token.Clause module.
--   > **
--   > 
--   > aa
--   > bb
--   >  cc
--   > 
--   > ** dd
--   > ee
--
-- Read this file and parse to token clauses.
--
--   >>> O.printList =<< B.abortLeft =<< readClauses "clause.txt"
--   CodeClause {clauseLines = ..., clauseTokens = [TText /0.1.0/ TextRaw "aa"]}
--   CodeClause {clauseLines = ..., clauseTokens = [TText /0.2.0/ TextRaw "bb",
--                                                  TText /0.3.1/ TextRaw "cc"]}
--   CodeClause {clauseLines = ..., clauseTokens = [TText /0.6.0/ TextRaw "ee"]}
--
readClauses :: FilePath -> IO (B.Ab [TokenClause])
readClauses path =
    do ls <- readTokenLines path
       return (tokenClauses <$> ls)


-- --------------------------------------------  Abbreviation

-- | Abbreviated tokenizer.
toks :: String -> [S.Token]
toks s = tokens (B.codeIxIO $ B.stringBz s) s

printToks :: [S.Token] -> IO ()
printToks ts =
    do putStrLn $ "********** " ++ (B.cpLineText $ B.getCP ts)
       mapM_ print ts

-- | Tokenize and print for debug.
--
--  Words and quotations.
--
--    >>> toksPrint ["aa", "'bb", "\"cc\"", "<dd>", "'/ee", "|012|"]
--    TText /0.1.0/ TextRaw "aa"
--    TText /0.2.0/ TextQ "bb"
--    TText /0.3.0/ TextQQ "cc"
--    TText /0.4.0/ TextUnk "dd"
--    TText /0.5.0/ TextTerm "ee"
--    TText /0.6.0/ TextBar "|012|"
--
--  Judgement.
--
--    >>> toksPrint ["|-- R  /a A0 /b 31"]
--    TText /0.1.0/ TextBar "|--"
--    TSpace /0.1.3/ 1
--    TText /0.1.4/ TextRaw "R"
--    TSpace /0.1.5/ 2
--    TTerm /0.1.7/ "/a"
--    TSpace /0.1.9/ 1
--    TText /0.1.10/ TextRaw "A0"
--    TSpace /0.1.12/ 1
--    TTerm /0.1.13/ "/b"
--    TSpace /0.1.15/ 1
--    TText /0.1.16/ TextRaw "31"
--  
--  Brackets.
--
--    >>> toksPrint ["aa (bb x y (z))"]
--    TText /0.1.0/ TextRaw "aa"
--    TSpace /0.1.2/ 1
--    TOpen /0.1.3/ "("
--    TText /0.1.4/ TextRaw "bb"
--    TSpace /0.1.6/ 1
--    TText /0.1.7/ TextRaw "x"
--    TSpace /0.1.8/ 1
--    TText /0.1.9/ TextRaw "y"
--    TSpace /0.1.10/ 1
--    TOpen /0.1.11/ "("
--    TText /0.1.12/ TextRaw "z"
--    TClose /0.1.13/ ")"
--    TClose /0.1.14/ ")"
--
--  Comment.
--
--    >>> toksPrint ["abc ** this is a comment", "def",""]
--    TText /0.1.0/ TextRaw "abc"
--    TSpace /0.1.3/ 1
--    TComment /0.1.4/ " this is a comment"
--    TText /0.2.0/ TextRaw "def"
--
toksPrint :: [String] -> IO ()
toksPrint ss = printToks $ toks $ unlines ss


-- | Parse string to clause and print it for inspection.
--
--   >>> clausePrint ["|-- A /x 0 /y 1", "", "|== B : ", "  source A /x /y"]
--   ********** |-- A /x 0 /y 1
--   TText /0.1.0/ TextBar "|--"
--   TText /0.1.4/ TextRaw "A"
--   TTerm /0.1.6/ "/x"
--   TText /0.1.9/ TextRaw "0"
--   TTerm /0.1.11/ "/y"
--   TText /0.1.14/ TextRaw "1"
--   ********** |== B : 
--   TText /0.3.0/ TextBar "|=="
--   TText /0.3.4/ TextRaw "B"
--   TText /0.3.6/ TextRaw ":"
--   TText /0.4.2/ TextRaw "source"
--   TText /0.4.9/ TextRaw "A"
--   TTerm /0.4.11/ "/x"
--   TTerm /0.4.14/ "/y"
--
clausePrint :: [String] -> IO ()
clausePrint ss =
    do let ls = tokenClauses $ tokenLines B.def $ unlines ss
       printToks `mapM_` (B.clauseTokens <$> ls)


{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Syntax.Line
  ( -- * Line
    LineNumber,
    NumberedLine,
    linesCrlfNumbered,
    linesCrlf,
    linesFrom,

    -- * CodeLine
    CodeLine (..),
    IndentSize,
    lineIndentPair,

    -- * CodeRoll
    CodeRoll (..), WordTable,
    codeRollUp,
    codeUpdate, codeUpdateWords,
    codeChange,
  ) where

import qualified Data.Generics                        as G
import qualified Data.Map                             as Map
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Syntax.Message  as Msg


-- ----------------------  Line

-- | Line number.
type LineNumber = Int
-- | Line with number.
type NumberedLine = (LineNumber, String)

-- | Line number and its content.
linesCrlfNumbered :: String -> [NumberedLine]
linesCrlfNumbered = zip [1..] . linesCrlf

-- | Split string into lines.
--   The result strings do not contain
--   carriage returns (@\\r@)
--   and line feeds (@\\n@).
linesCrlf :: String -> [String]
linesCrlf "" = []
linesCrlf s = ln : next s2 where
    (ln, s2) = break (`elem` "\r\n") s
    next ('\r' : s3) = next s3
    next ('\n' : s3) = linesCrlf s3
    next s3          = linesCrlf s3

linesFrom :: (Show a) => a -> [String]
linesFrom = lines . show


-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine a = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: String        -- ^ Line content without newline.
    , lineTokens  :: [a]           -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Write (CodeLine a) where
    writeDocWith sh (CodeLine _ line _) = B.writeDocWith sh line

instance (B.CodePtr a) => B.CodePtr (CodeLine a) where
    codePtList (CodeLine _ _ ts) = B.codePtList $ head ts

type IndentSize = Int

lineIndentPair :: (a -> IndentSize) -> CodeLine a -> (IndentSize, CodeLine a)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)


-- ----------------------  CodeRoll

type WordTable = Map.Map String String

data CodeRoll a =
    CodeRoll { codeMap     :: B.AbMap (CodeRoll a)
             , codeInputPt :: B.CodePt
             , codeInput   :: String
             , codeOutput  :: [a]
             , codeWords   :: WordTable
             }

-- | Split source text into 'CodeLine' list.
--
--   1. Split source text into lines by line delimiters
--      (carriage return @\\r@ or line feed @\\n@).
--
--   2. Numbering lines from 1.
--      Internally, this is represented as
--      a list of pairs @(@'B.LineNumber'@,@ 'String'@)@.
--
--   3. Tokenize each lines,
--      and put tokens together in 'CodeLine'.
--
codeRollUp :: B.AbMap (CodeRoll a) -> B.CodePiece -> String -> B.Ab [CodeLine a]
codeRollUp f res = loop (CodeRoll f cp "" [] Map.empty) . linesCrlfNumbered where
    cp    = B.codePtZero { B.codePtSource = res }

    loop _ [] = Right []
    loop r ((num, line) : ls) =
       do let cp' = setLine num line cp
          r' <- Msg.abCode [cp'] $ codeRoll $ setRoll cp' line r
          let toks = reverse $ codeOutput r'
          ls' <- loop r' ls
          Right $ CodeLine num line toks : ls'

setLine :: LineNumber -> String -> B.Map B.CodePt
setLine num line cp =
    cp { B.codePtLineNo    = num
       , B.codePtLineText  = line
       , B.codePtText      = line }

setRoll :: B.CodePt -> String -> CodeRoll a -> CodeRoll a
setRoll cp line roll =
    roll { codeInputPt = cp
         , codeInput   = line
         , codeOutput  = [] }

codeRoll :: B.AbMap (CodeRoll a)
codeRoll roll@CodeRoll { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input  = call
    | otherwise   = call >>= codeRoll
    where call    = f roll { codeInputPt = setText input cp }

setText :: String -> B.Map B.CodePt
setText text cp = cp { B.codePtText = text }

-- | Update 'codeInput' and push result element to 'codeOutput'.
codeUpdate :: String -> a -> B.Map (CodeRoll a)
codeUpdate cs tok roll =
    roll { codeInput  = cs
         , codeOutput = tok : codeOutput roll }

codeUpdateWords :: WordTable -> String -> a -> B.Map (CodeRoll a)
codeUpdateWords ws cs tok roll =
    roll { codeInput  = cs
         , codeOutput = tok : codeOutput roll
         , codeWords  = ws }

-- | Change mapper of code roll.
codeChange :: B.AbMap (CodeRoll a) -> B.Map (CodeRoll a)
codeChange f roll = roll { codeMap = f }

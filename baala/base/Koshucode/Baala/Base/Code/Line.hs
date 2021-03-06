{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Code.Line
  ( -- * Line
    ToLines (..),
    bzDropBom, bsDropBom,
    bzLines,
    linesCrlf, linesCrlfNumbered,

    -- * CodeLine
    LineNumber,
    CodeLine (..),
    IndentSize,
    lineIndentPair,
  ) where

import qualified Data.ByteString                      as Bs
import qualified Data.ByteString.Lazy                 as Bz
import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.Prelude         as B


-- ============================================  Line

-- | Split input into textual lines.
--
--   >>> toLines "aaa\nbbb\r\nccc\n\nddd\n" :: [String]
--   ["aaa", "bbb", "ccc", "", "ddd"]
--
class ToLines i where
    toLines :: (O.Textual t) => i -> [t]

instance ToLines O.Bz where
    toLines = map O.bzT . bzLines

instance ToLines String where
    toLines = csLines . O.stringT

instance ToLines O.Tx where
    toLines = csLines . O.txT

instance ToLines O.Tz where
    toLines = csLines . O.tzT

{-| Remove UTF-8 BOM (EF BB BF in hexadecimal) from lazy bytestring.

    >>> bzDropBom "\xEF\xBB\xBF|foo bar baz"
    "|foo bar baz"

    >>> bzDropBom "\xEF|foo bar baz"
    "\239|foo bar baz"
    -}
bzDropBom :: O.Bz -> O.Bz
bzDropBom = dropBom Bz.splitAt

{-| Remove UTF-8 BOM (EF BB BF in hexadecimal) from strict bytestring. -}
bsDropBom :: O.Bs -> O.Bs
bsDropBom = dropBom Bs.splitAt

dropBom :: (Integral n, B.IsString a, Eq a) => (n -> a -> (a, a)) -> a -> a
dropBom f bz = case f 3 bz of
               (bom, body) | "\xEF\xBB\xBF" == bom -> body
                           | otherwise             -> bz

-- | Split lazy bytestring by newline character sequence.
--   If bytestring begins with the UTF-8 BOM, this function drops it.
bzLines :: O.Bz -> [O.Bz]
bzLines = loop . bzDropBom where
    loop bz | Bz.null bz  = []
            | otherwise   = case Bz.break crlf bz of
                              (ln, bz') -> ln : loop (strip bz')
                          
    crlf n = (n == lf) || (n == cr)
    lf     = 10 -- '\n'
    cr     = 13 -- '\r'

    strip bz2@(Bz.uncons -> Just (c , bz2'))
               | c == cr   = strip bz2'
               | c == lf   = bz2'
               | otherwise = bz2
    strip bz2 = bz2
                  
-- | Split /textual-value-#1/ into lines.
--   The result texts do not contain
--   carriage returns (@\\r@)
--   and line feeds (@\\n@).
--
--   >>> csLines "aaa\nbbb\r\nccc\n\nddd\n"
--   ["aaa", "bbb", "ccc", "", "ddd"]
--
csLines :: (O.Textual t) => t -> [t]
csLines s
    | O.tIsEmpty s = []
    | otherwise = ln : next s2
    where
      (ln, s2) = O.tWhileNot isCrlf s
      next (O.cut -> O.Jp '\r' s3) = next s3
      next (O.cut -> O.Jp '\n' s3) = csLines s3
      next s3                      = csLines s3

isCrlf :: Char -> Bool
isCrlf '\r' = True
isCrlf '\n' = True
isCrlf _    = False

-- | Split string into lines.
{-# DEPRECATED linesCrlf "Consider 'bzLines'." #-}
linesCrlf :: String -> [String]
linesCrlf = csLines

-- | Line number and its content.
{-# DEPRECATED linesCrlfNumbered "Consider 'bzLines'." #-}
linesCrlfNumbered :: String -> [(LineNumber, String)]
linesCrlfNumbered = zip [1..] . linesCrlf


-- ============================================  CodeLine

-- | Line number.
type LineNumber = Int

-- | Tokens in line.
data CodeLine k t = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: t             -- ^ Line content without newline.
    , lineTokens  :: [k t]         -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord)

instance (Functor k) => Functor (CodeLine k) where
    fmap f ln@CodeLine { lineContent = c, lineTokens = ts } =
        ln { lineContent = f c, lineTokens = f O.<$$> ts }

instance (B.GetCodePos (k t)) => B.GetCodePos (CodeLine k t) where
    getCPs (CodeLine _ _ toks) = B.getCPs toks

-- | Type for indent size.
type IndentSize = Int

-- | Calculate indent of line and pairing it.
lineIndentPair :: (k t -> IndentSize) -> CodeLine k t -> (IndentSize, CodeLine k t)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)


{-# OPTIONS_GHC -Wall #-}

{-| Text lines delimited by carriage returns or line feeds. -}

module Koshucode.Baala.Base.Syntax.Line
( LineNumber,
  NumberedLine,
  linesCrlfNumbered,
  linesCrlf,
  putLines,
) where

{-| Line number. -}
type LineNumber = Int
type NumberedLine = (LineNumber, String)

{-| Line number and its content. -}
linesCrlfNumbered :: String -> [NumberedLine]
linesCrlfNumbered = zip [1..] . linesCrlf

{-| Split string into lines.
    The result strings do not contain
    carriage returns (@\\r@)
    and line feeds (@\\n@). -}
linesCrlf :: String -> [String]
linesCrlf "" = []
linesCrlf s = ln : next s2 where
    (ln, s2) = break (`elem` "\r\n") s
    next ('\r' : s3) = next s3
    next ('\n' : s3) = linesCrlf s3
    next s3          = linesCrlf s3

putLines :: [String] -> IO ()
putLines = putStr . unlines


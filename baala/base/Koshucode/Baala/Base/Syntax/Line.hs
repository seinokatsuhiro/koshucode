{-# OPTIONS_GHC -Wall #-}

{-| Lines. -}

module Koshucode.Baala.Base.Syntax.Line
( LineNumber,
  linesCrlfNumbered,
  linesCrlf,
  putLines,
) where

{-| Line number. -}
type LineNumber = Int

{-| Line number and its content. -}
linesCrlfNumbered :: String -> [(LineNumber, String)]
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
    next ('\n' : s3) = next s3
    next s3 = linesCrlf s3

putLines :: [String] -> IO ()
putLines = putStr . unlines


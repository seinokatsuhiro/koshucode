{-# OPTIONS_GHC -Wall #-}

-- | Reporting abort reasons.

--  Text
--
--    ABORTED  (main reason)
--    -------- --------------------------------------------
--    Detail   (detailed reason)
--             (detailed reason)
--    Source   (line #) (col #) (filename)
--             > (line text)
--             (line #) (col #) (filename)
--             > (line text)
--    Command  koshu
--             (arg 1)
--             (arg 2)

module Koshucode.Baala.Base.Abort.Report
  ( CommandLine,
    abort, abortPrint,
    abortMessage, bug,
  ) where

import qualified Koshucode.Baala.Base.Abort.Reason  as B
import qualified Koshucode.Baala.Base.List          as B
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Text          as B


-- | Command name and its arguments.
type CommandLine = [String]

-- | Stop program execution abnormally.
abort :: CommandLine -> B.AbortReason -> IO x
abort cmd a =
  do abortPrint cmd a
     B.putCommentLines ["Exit with status 2", ""]
     B.exitWith $ B.ExitFailure 2

-- | Print abort message.
abortPrint :: CommandLine -> B.AbortReason -> IO ()
abortPrint cmd a = B.putCommentLines $ abortMessage cmd a

-- | Convert abort reason to message lines.
abortMessage :: CommandLine -> B.AbortReason -> [String]
abortMessage cmd a = B.squeezeEmptyLines $ map B.trimRight texts where
    texts  = sandwich "" "" $ B.renderTable " " tab ++ note
    tab    = B.alignTable $ title : rule : rows
    title  = [ B.textCell B.Front "ABORTED "
             , B.textCell B.Front $ B.abortReason a ]
    rule   = [r, r, r]
    r      = B.textRuleCell '-'
    p text = (text, "")
    rows   = concatMap row
             [ ("Detail"  , map p  $ B.abortDetail a)
             , ("Source"  , source $ B.abortPoint a)
             , ("Command" , map p  $ cmd)
             ]

    note = case B.abortNote a of
             n | n == []   -> []
               | otherwise -> "" : "Note" : "" : map ("  " ++) n

    row :: (String, [(String, String)]) -> [[B.Cell]]
    row (_, []) = []
    row (name, xs)
        = let (codes, tags) = unzip xs
          in [[ B.textCell      B.Front name
              , B.textBlockCell B.Front codes
              , B.textBlockCell B.Front $ map dots tags ]]

    dots :: B.Map String
    dots ""   = ""
    dots text = ".. " ++ text

    sandwich :: a -> a -> B.Map [a]
    sandwich open close xs = open : xs ++ [close]

source :: [(String, B.CodePt)] -> [(String, String)]
source = concatMap B.codePtDisplay . B.unique . reverse

-- | Stop on error @'BUG DISCOVERED'@
bug :: String -> a
bug msg = error $ "BUG DISCOVERED: " ++ msg


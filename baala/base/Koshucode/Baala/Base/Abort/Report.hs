{-# OPTIONS_GHC -Wall #-}

-- | Report abort reasons.
--
-- Abortable process returns @Left@ 'B.AbortReason' when aborted.
-- The 'B.abortable' function adds code positions to the reason.
-- 'abortPrint' outputs aborted message in the following format.
--
-- > ** ABORTED  {main reason}
-- > ** -------- --------------------------------------------
-- > ** Detail   {detailed reason}
-- > **          {detailed reason}
-- > ** Source   {line #} {char #} {source}
-- > **          > {code string}
-- > **          {line #} {char #} {source}
-- > **          > {code string}
-- > ** Command  {program name}
-- > **          {arg 1}
-- > **          {arg 2}

module Koshucode.Baala.Base.Abort.Report
  ( -- * Message
    CommandLine,
    cpMessage,
    abortMessage,
    abortPrint,

    -- * Abort
    abort,
    abortCommand,
    abortLeft,
    bug,
  ) where

import Koshucode.Baala.Overture ((&))
import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.System               as O
import qualified Koshucode.Baala.Base.List            as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Abort.CodePos   as B
import qualified Koshucode.Baala.Base.Abort.Reason    as B


-- --------------------------------------------  Message

-- | Command name and its arguments.
type CommandLine = [String]

-- | Create position and line information.
cpMessage :: B.CodePosInfo -> [(String, B.AbortTag)]
cpMessage (cp@B.CodePos { B.cpLineNo = lno }, tag)
    | lno <= 0      = []
    | null before'  = [ trunc pos     & ""
                      , indent after' & tag
                      , "" & "" ]
    | otherwise     = [ trunc pos      & ""
                      , indent before' & ""
                      , indent after'  & tag
                      , "" & "" ]
    where
      trunc   = O.truncateString 48
      indent s = "  " ++ trunc s
      pos     = show lno ++ "." ++ show cno ++ " " ++ O.getIOPath cp
      cno     = B.cpColumnNo cp
      before' = O.trimBegin before
      after'  = replicate (min 4 $ length before') ' ' ++ O.trimBegin after
      (before, after) = B.cpSplit cp

-- | Convert abort reason to message lines.
abortMessage :: CommandLine -> B.AbortReason -> [String]
abortMessage cmd a = B.squeezeEmptyLines $ map O.trimEnd texts where
    texts  = sandwich "" "" $ B.renderTable " " tab ++ note
    tab    = B.alignTable $ title : rule : rows
    title  = [ B.textCell B.Front "ABORTED "
             , B.textCell B.Front $ B.abortReason a ]
    rule   = [r, r, r]
    r      = B.textRuleCell '-'
    p text = (text, "")
    rows   = concatMap row
             [ "Detail"  & detail $ B.abortDetail a
             , "Source"  & source $ B.abortPoint a
             , "Command" & p <$> cmd ]

    row :: (String, [(String, String)]) -> [[B.Cell]]
    row (_, []) = []
    row (name, xs)
        = let (codes, tags) = unzip xs
          in [[ B.textCell      B.Front name
              , B.textBlockCell B.Front codes
              , B.textBlockCell B.Front $ map dots tags ]]

    detail [] = []
    detail ln = p <$> ln ++ [""]

    source :: [B.CodePosInfo] -> [(String, B.AbortTag)]
    source = concatMap cpMessage . B.unique . reverse

    dots :: O.StringMap
    dots ""   = ""
    dots text = ".. " ++ text

    sandwich :: a -> a -> O.Map [a]
    sandwich open close xs = open : xs ++ [close]

    note = case B.abortNote a of
             n | n == []   -> []
               | otherwise -> "" : "Note" : "" : (("  " ++) <$> n)

-- | Print abort message.
--
-- Prepare code position and abort reason.
--
--   >>> let cp = B.CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   >>> let Left a = B.abortable "tag" cp $ Left $ B.abortBecause "Bad luck"
--
-- Print it.
--
--   >>> abortPrint (words "prog x y") a
--   **
--   **  ABORTED  Bad luck
--   **  -------- ----------- ------
--   **  Source   1.3 <stdin>
--   **             abc
--   **                defg   .. tag
--   **
--   **  Command  prog
--   **           x
--   **           y
--   **
--
abortPrint :: CommandLine -> B.AbortReason -> IO ()
abortPrint cmd a = B.putCommentLines $ abortMessage cmd a


-- --------------------------------------------  Abort

-- | Stop program execution abnormally.
abort :: B.AbortReason -> IO x
abort a =
  do (prog, args) <- O.progAndArgs
     abortCommand (prog : args) a

-- | Abort with a command line.
abortCommand :: CommandLine -> B.AbortReason -> IO x
abortCommand cmd a =
  do abortPrint cmd a
     B.putCommentLines ["Exit with status 2", ""]
     O.exit 2

-- | Abort when getting abort reason.
abortLeft :: B.Ab b -> IO b
abortLeft (Right b)  = return b
abortLeft (Left a)   = abort a

-- | Stop on error @'BUG DISCOVERED'@
bug :: String -> a
bug msg = error $ "BUG DISCOVERED: " ++ msg


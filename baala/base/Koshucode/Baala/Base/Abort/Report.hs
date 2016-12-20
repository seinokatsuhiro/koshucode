{-# OPTIONS_GHC -Wall #-}

-- | Report abort reasons.
--
-- Abortable process returns @Left@ 'B.AbortReason' when aborted.
-- The 'B.abortable' function adds code positions to the reason.
-- 'abortPrint' outputs aborted message in the following format.
--
-- > **
-- > ** ABORTED  {reason}
-- > ** -------- ---------------------------------- ------
-- > ** Detail   {description}
-- > **          {description}
-- > **
-- > ** Source   {line}.{char} {path}
-- > **            {before}
-- > **                {after}                      {tag}
-- > **
-- > **          {line}.{char} {path}
-- > **            {before}
-- > **                {after}                      {tag}
-- > **
-- > ** Command  {program}
-- > **          {arg}
-- > **          {arg}
-- > **
-- >
-- > === note
-- > 
-- > {note}
-- > {note}
-- > 
-- > === rel

module Koshucode.Baala.Base.Abort.Report
  ( -- * Message
    CommandLine,
    cpMessage, cpMessageLines,
    abortMessage,
    abortPrint,

    -- * Abort
    abort,
    abortCommand,
    abortLeft,
    bug,
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.System               as O
import qualified Koshucode.Baala.Base.List            as B
import qualified Koshucode.Baala.Base.Text            as B
import qualified Koshucode.Baala.Base.Abort.CodePos   as B
import qualified Koshucode.Baala.Base.Abort.Reason    as B


-- --------------------------------------------  Message

-- | Command name and its arguments.
type CommandLine = [String]

-- | Convert code position into message text:
--   location, before code, after code, and abort tag.
--
--   >>> cpMessage $ B.CodePos 0 "<stdin>" 1 "abcdefg" "defg"
--   (Just (1,3,"<stdin>"), Just "abc", Just "defg")
--
cpMessage :: B.CodePos -> (Maybe (Int, Int, FilePath),
                           Maybe String, Maybe String)
cpMessage cp@B.CodePos { B.cpLineNo = lno }
    | lno <= 0      = (Nothing, Nothing, Nothing)
    | null before'  = (Just pos, Nothing, Just after')
    | otherwise     = (Just pos, Just before', Just after')
    where
      pos      = (lno, cno, O.getIOPath cp)
      cno      = B.cpColumnNo cp
      before'  = O.trimBegin b
      after'   = O.trimBegin a
      (b, a)   = B.cpSplit cp

-- | Create position and line information.
cpMessageLines :: B.CodePosInfo -> [(String, B.AbortTag)]
cpMessageLines (cp, tag)
    = case cpMessage cp of
        (Just p, Nothing, Just a)
            -> [ no     $ trunc (pos p)
               , tagged $ indent a
               , no     $ "" ]
        (Just p, Just b, Just a)
            -> [ no     $ trunc (pos p)
               , no     $ indent b
               , tagged $ indent (b # a)
               , no     $ "" ]
        _   -> []
    where
      no         = (O.& "")
      tagged     = (O.& tag)
      trunc      = O.truncateString 48
      indent s   = "  " ++ trunc s
      b # a      = replicate (min 4 $ length b) ' ' ++ trunc a
      pos (lno, cno, path) = show lno ++ "." ++ show cno ++ " " ++ path

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
             [ "Detail"  O.& detail $ B.abortDetail a
             , "Source"  O.& source $ B.abortPointUp a
             , "Command" O.& p <$> cmd ]

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
    source = concatMap cpMessageLines

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


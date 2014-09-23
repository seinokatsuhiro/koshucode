{-# OPTIONS_GHC -Wall #-}

-- | Read section.
module Koshucode.Baala.Core.Section.Read
( readSection,
  readSectionText,
  readJudges,
) where

import qualified System.Directory                     as Dir
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Msg

-- | Read section from certain resource.
readSection :: (C.CContent c) => C.Section c -> B.Resource -> IO (B.Ab (C.Section c))
readSection root res = dispatch res where
    dispatch (B.ResourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False -> return $ Msg.noFile path
               True  -> do code <- readFile path
                           return $ readSectionCode root res code

    dispatch (B.ResourceText code)
        = return $ readSectionCode root res code

    dispatch (B.ResourceURL _)
        = error "Not implemented read from URL"

readSectionCode
    :: (C.CContent c)
    => C.Section c         -- ^ Root section
    -> B.Resource          -- ^ Resource name
    -> String              -- ^ Source text
    -> B.Ab (C.Section c)  -- ^ Resulting section
readSectionCode root res =
    C.consSection root res . C.consClause B.<=< B.tokenLines res

-- | Read section from text.
readSectionText :: (C.CContent c) => C.Section c -> String -> B.Ab (C.Section c)
readSectionText root code = readSectionCode root (B.ResourceText code) code

-- | Read judges from text.
readJudges :: (C.CContent c) => String -> B.Ab [B.Judge c]
readJudges code =
    do sec <- readSectionText C.emptySection code
       Right $ C.secJudge sec


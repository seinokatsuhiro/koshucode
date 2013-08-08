{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.SectionIO
(
  -- * Reading section
  sectionRead,
  sectionFile,

  -- * Running section
  runSection,
  runSectionIO,
  hRunSectionIO,
) where

import qualified System.IO as IO
import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap
import Koshucode.Baala.Core.Section.Section
import Koshucode.Baala.Core.Section.Clause



-- ----------------------  Reading section

{-| Read section from text. -}
sectionRead
    :: (CContent c)
    => Section c   -- ^ Section that is same type to result section
    -> String      -- ^ Resource name
    -> String      -- ^ Source text
    -> B.AbortOr (Section c)  -- ^ Result section from source text
sectionRead root res src = sec where
    (RelmapCons half full) = sectionCons root
    sec = consSection full res $ consClause half $ B.sourceLines src

{-| Read section from file. -}
sectionFile
    :: (CContent c)
    => Section c    -- ^ Root section
    -> FilePath     -- ^ Path of section file
    -> IO (B.AbortOr (Section c)) -- ^ Result section
sectionFile root path =
    do code <- readFile path
       return $ sectionRead root path code



-- ----------------------  Running section

{-| Run section.
    Output section has judges calculated
    from assertions in input section. -}
runSection
    :: (CContent c)
    => Section c             -- ^ Input section
    -> B.AbortOr (Section c) -- ^ Output section
runSection sec =
    do let calc  = sectionLinkedAssert sec
           input = sectionJudge sec
       output <- runAssertJudges calc input
       Right $ sec { sectionJudge = output }

{-| Run section and output judges. -}
runSectionIO :: (CContent c) => Section c -> IO ()
runSectionIO = hRunSectionIO IO.stdout

hRunSectionIO :: (CContent c) => IO.Handle -> Section c -> IO ()
hRunSectionIO h = B.abortIO (B.hPutJudges h . sectionJudge) . runSection


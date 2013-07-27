{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.SectionIO
(
-- * Reading section
  sectionRead
, sectionFile

-- * Running section
, runSection
, runSectionIO
, hRunSectionIO
) where

import qualified System.IO as IO

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap
import Koshucode.Baala.Core.Section.Section
import Koshucode.Baala.Core.Section.Clause



-- ----------------------  Reading section

{-| Read section from text. -}
sectionRead
    :: (CContent v)
    => Section v   -- ^ Section that is same type to result section
    -> String      -- ^ Source text
    -> AbortOr (Section v)  -- ^ Result section from source text
sectionRead root src = sec where
    (RelmapCons half full) = sectionCons root
    sec = consSection full $ consClause half $ sourceLines src

{-| Read section from file. -}
sectionFile
    :: (CContent v)
    => Section v    -- ^ Root section
    -> FilePath     -- ^ Path of section file
    -> IO (AbortOr (Section v)) -- ^ Result section
sectionFile root path =
    do code <- readFile path
       return $ sectionRead root code



-- ----------------------  Running section

{-| Run section.
    Output section has judges calculated
    from assertions in input section. -}
runSection
    :: (CContent v)
    => Section v           -- ^ Input section
    -> AbortOr (Section v) -- ^ Output section
runSection sec =
    do let calc  = sectionLinkedAssert sec
           input = sectionJudge sec
       output <- runAssertJudges calc input
       Right $ sec { sectionJudge = output }

{-| Run section and output judges. -}
runSectionIO :: (CContent v) => Section v -> IO ()
runSectionIO = hRunSectionIO IO.stdout

hRunSectionIO :: (CContent v) => IO.Handle -> Section v -> IO ()
hRunSectionIO h = abortIO (hPutJudges h . sectionJudge) . runSection


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Section.SectionIO
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

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Relmap
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content.Class
import Koshucode.Baala.Base.Section.Section
import Koshucode.Baala.Base.Section.Clause



-- ----------------------  Reading section

{-| Read section from text. -}
sectionRead
    :: (Value v)
    => Section v   -- ^ Section that is same type to result section
    -> String      -- ^ Source text
    -> AbortOr (Section v)  -- ^ Result section from source text
sectionRead root src = sec where
    (RelmapCons half full) = sectionCons root
    sec = consSection full $ consClause half $ sourceLines src

{-| Read section from file. -}
sectionFile
    :: (Value v)
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
    :: (Value v)
    => Section v           -- ^ Input section
    -> AbortOr (Section v) -- ^ Output section
runSection sec =
    do let calc  = sectionLinkedAssert sec
           input = sectionJudge sec
       output <- runAssertJudges calc input
       Right $ sec { sectionJudge = output }

{-| Run section and output judges. -}
runSectionIO :: (Value v) => Section v -> IO ()
runSectionIO = hRunSectionIO IO.stdout

hRunSectionIO :: (Value v) => IO.Handle -> Section v -> IO ()
hRunSectionIO h = abortIO (hPutJudges h . sectionJudge) . runSection


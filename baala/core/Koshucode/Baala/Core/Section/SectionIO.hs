{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Section.SectionIO
(
  -- * Reading section
  sectionRead,
  sectionFile,
  readJudges,

  -- * Running section
  runSection,
  runSectionIO,
  hRunSectionIO,
) where

import qualified System.IO                            as IO
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C



-- ----------------------  Reading section

{-| Read section from text. -}
sectionRead
    :: (C.CContent c)
    => C.Section c  -- ^ Section that is same type to result section
    -> String       -- ^ Resource name
    -> String       -- ^ Source text
    -> B.AbortOr (C.Section c)  -- ^ Result section from source text
sectionRead root res src =
    let (C.RelmapCons half full) = C.sectionCons root
    in case C.consClause half $ B.tokenize src of
         Right cs -> C.consSection full res cs
         Left a   -> Left (a, [], [])

{-| Read section from file. -}
sectionFile
    :: (C.CContent c)
    => C.Section c   -- ^ Root section
    -> FilePath      -- ^ Path of section file
    -> IO (B.AbortOr (C.Section c)) -- ^ Result section
sectionFile root path =
    do code <- readFile path
       return $ sectionRead root path code

readJudges :: (C.CContent c) => String -> B.AbortOr [B.Judge c]
readJudges code =
    do sec <- sectionRead C.emptySection "text" code
       Right $ C.sectionJudge sec


-- ----------------------  Running section

{-| Run section.
    Output section has judges calculated
    from assertions in input section. -}
runSection
    :: (C.CContent c)
    => C.Section c             -- ^ Input section
    -> B.AbortOr (C.Section c) -- ^ Output section
runSection sec =
    do let calc  = C.sectionLinkedAssert sec
           input = C.sectionJudge sec
       output <- C.runAssertJudges calc input
       Right $ sec { C.sectionJudge = output }

{-| Run section and output judges. -}
runSectionIO :: (C.CContent c) => C.Section c -> IO ()
runSectionIO = hRunSectionIO IO.stdout

hRunSectionIO :: (C.CContent c) => IO.Handle -> C.Section c -> IO ()
hRunSectionIO h = B.abortIO (B.hPutJudges h . C.sectionJudge) . runSection


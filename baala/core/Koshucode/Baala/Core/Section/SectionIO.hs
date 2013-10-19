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
import qualified Koshucode.Baala.Core.Assert          as C
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
    do let (C.RelmapCons half full) = C.sectionCons root
       cs <- C.consClause half $ B.tokenize src
       C.consSection full res cs

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
    do let assert      = C.sectionLinkedAssert sec
           input       = C.sectionJudge sec
           vioFilter p = filter (p C.AssertViolate . C.assertType) assert
           assViolate  = vioFilter (==)
           assOutput   = vioFilter (/=)
       violate <- C.runAssertJudges assViolate input
       output  <- C.runAssertJudges assOutput  input
       Right $ sec { C.sectionViolate = violate
                   , C.sectionJudge   = output }

{-| Run section and output judges. -}
runSectionIO :: (C.CContent c) => C.Section c -> IO Int
runSectionIO = hRunSectionIO IO.stdout

hRunSectionIO :: (C.CContent c) => IO.Handle -> C.Section c -> IO Int
hRunSectionIO h = B.abortIO f . runSection where
    f C.Section { C.sectionViolate = violate
                , C.sectionJudge   = judge }
        = if null violate
          then B.hPutJudges h 0 judge
          else B.hPutJudges h 1 violate


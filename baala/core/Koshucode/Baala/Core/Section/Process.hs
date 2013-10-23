{-# OPTIONS_GHC -Wall #-}

{-| Read, run, and write sections. -}
module Koshucode.Baala.Core.Section.Process
(
  -- * Read
  readSectionCode,
  readSectionFile,
  readJudges,

  -- * Run
  runSection,

  -- * Write
  hPutSection,
) where

import qualified System.IO                            as IO
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C

{-| Read section from text. -}
readSectionCode
    :: (C.CContent c)
    => C.Section c  -- ^ Root section
    -> String       -- ^ Resource name
    -> String       -- ^ Source text
    -> B.AbortOr (C.Section c)  -- ^ Resulting section
readSectionCode root res code =
    do let (C.RelmapCons half full) = C.sectionCons root
       clauses <- C.consClause half $ B.tokenize code
       C.consSection full res clauses

{-| Read section from file. -}
readSectionFile
    :: (C.CContent c)
    => C.Section c   -- ^ Root section
    -> FilePath      -- ^ Path of section file
    -> IO (B.AbortOr (C.Section c)) -- ^ Resulting section
readSectionFile root path =
    do code <- readFile path
       return $ readSectionCode root path code

{-| Read judges from text. -}
readJudges :: (C.CContent c) => String -> B.AbortOr [B.Judge c]
readJudges code =
    do sec <- readSectionCode C.emptySection "text" code
       Right $ C.sectionJudge sec

{-| Run section.
    Output section has judges calculated
    from assertions in input section. -}
runSection
    :: (C.CContent c)
    => C.Section c              -- ^ Input section
    -> B.AbortOr (C.Section c)  -- ^ Output section
runSection sec =
    do let input       = C.sectionJudge sec
           allAsserts  = sectionLinkedAssert sec
           asserts p   = filter (p . C.assertType) allAsserts
           judges p    = C.runAssertJudges (asserts p) input
       violate <- judges (== C.AssertViolate)
       output  <- judges (/= C.AssertViolate)
       Right $ sec { C.sectionJudge   = output
                   , C.sectionViolate = violate }

{-| Select assertions like 'sectionAssert'.
    It returns relmap-liked assertions.
    We can run these assertions using 'C.runAssertJudges'. -}
sectionLinkedAssert :: C.Section c -> [C.Assert c]
sectionLinkedAssert C.Section { C.sectionRelmap = ms
                              , C.sectionAssert = ass }
    = map linker ass where
      linker = C.assertMap $ C.relmapLinker ms

{-| Output judges in section.
    If violated judges are found, output they.
    If no violations, output regular judges.  -}
hPutSection :: (C.CContent c) => IO.Handle -> C.Section c -> IO Int
hPutSection h C.Section { C.sectionJudge   = output
                        , C.sectionViolate = violate }
    | null violate = B.hPutJudges h 0 output
    | otherwise    = B.hPutJudges h 1 violate


{-# OPTIONS_GHC -Wall #-}

{-| Read, run, and write sections. -}
module Koshucode.Baala.Core.Section.Process
( -- * Read
  readSection,
  readSectionText,

  readJudges,

  -- * Run
  runSection,

  -- * Write
  hPutSection,
) where

import qualified System.IO                            as IO
import qualified System.Directory                     as Dir
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C



-- --------------------------------------------  Read

{-| Read section from certain resource. -}
readSection :: (C.CContent c) => C.Section c -> B.Resource -> IO (B.Ab (C.Section c))
readSection root res = dispatch res where
    dispatch (B.ResourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False -> return $ Left $ B.AbortIO $ B.AIONoFile path
               True  -> do code <- readFile path
                           return $ readSectionCode root res code

    dispatch (B.ResourceText code)
        = return $ readSectionCode root res code

    dispatch (B.ResourceURL _)
        = error "Not implemented read from URL"

readSectionCode
    :: (C.CContent c)
    => C.Section c  -- ^ Root section
    -> B.Resource   -- ^ Resource name
    -> String       -- ^ Source text
    -> B.Ab (C.Section c)  -- ^ Resulting section
readSectionCode root res code =
    do let (C.RelmapCons half full) = C.sectionCons root
       clauses <- C.consClause half $ B.tokenLines res code
       C.consSection full res clauses

{-| Read section from text. -}
readSectionText :: (C.CContent c) => C.Section c -> String -> B.Ab (C.Section c)
readSectionText root code =
    readSectionCode root (B.ResourceText code) code

{-| Read judges from text. -}
readJudges :: (C.CContent c) => String -> B.Ab [B.Judge c]
readJudges code =
    do sec <- readSectionText C.emptySection code
       Right $ C.sectionJudge sec


-- --------------------------------------------  Run

{-| Run section.
    Output section has judges calculated
    from assertions in input section. -}
runSection :: (C.CContent c) => C.Global c -> B.AbMap (C.Section c)
runSection global sec =
    do let g2          = global { C.globalJudges = C.sectionJudge sec }
           allAsserts  = sectionLinkedAssert sec
           asserts p   = filter (p . C.assertType) allAsserts
           judges p    = C.runAssertJudges g2 (asserts p)
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


-- --------------------------------------------  Write

{-| Output judges in section.
    If violated judges are found, output they.
    If no violations, output regular judges.  -}
hPutSection :: (C.CContent c) => IO.Handle -> C.Section c -> IO Int
hPutSection h C.Section { C.sectionJudge   = output
                        , C.sectionViolate = violate }
    | null violate = B.hPutJudges h 0 output
    | otherwise    = B.hPutJudges h 1 violate


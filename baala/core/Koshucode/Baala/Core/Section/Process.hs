{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Read, run, and write sections.
module Koshucode.Baala.Core.Section.Process
( -- * Read
  readSection,
  readSectionText,

  readJudges,

  -- * Run
  runSection,
) where

import qualified System.Directory                     as Dir
import qualified Data.Monoid                          as M
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Message



-- --------------------------------------------  Read

-- | Read section from certain resource.
readSection :: (C.CContent c) => C.Section c -> B.Resource -> IO (B.Ab (C.Section c))
readSection root res = dispatch res where
    dispatch (B.ResourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False -> return $ Message.noFile path
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
    do let (C.RelmapCons lx full) = C.sectionCons root
       clauses <- C.consClause lx $ B.tokenLines res code
       C.consSection full res clauses

-- | Read section from text.
readSectionText :: (C.CContent c) => C.Section c -> String -> B.Ab (C.Section c)
readSectionText root code =
    readSectionCode root (B.ResourceText code) code

-- | Read judges from text.
readJudges :: (C.CContent c) => String -> B.Ab [B.Judge c]
readJudges code =
    do sec <- readSectionText C.emptySection code
       Right $ C.sectionJudge sec


-- --------------------------------------------  Run

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab ([B.ShortJudge c], [B.ShortJudge c])
runSection global sects =
    do let s2 = M.mconcat sects
           g2 = global { C.globalJudges = C.sectionJudge s2 }
       runSectionBody g2 s2

runSectionBody :: forall c. (Ord c, C.CRel c, C.CNil c) =>
  C.Global c -> C.Section c -> B.Ab ([B.ShortJudge c], [B.ShortJudge c])
runSectionBody global C.Section { C.sectionRelmap = rdef, C.sectionAssert = ass2 } =
    do judgesV <- run $ C.assertViolated ass2
       judgesN <- run $ C.assertNormal   ass2
       Right (B.shortTrim judgesV, B.shortTrim judgesN)
    where
      run :: C.AbbrAsserts c -> B.Ab ([B.ShortJudge c])
      run = sequence . map B.shortAb . run2

      run2 :: C.AbbrAsserts c -> [B.Short (B.Ab [B.Judge c])]
      run2 = B.shortMap $ C.runAssertJudges global rdef


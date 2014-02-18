{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Read, run, and write sections. -}
module Koshucode.Baala.Core.Section.Process
( -- * Read
  readSection,
  readSectionText,

  readJudges,

  -- * Run
  runSection,
  sectionLinkedAssert,
) where

import qualified System.Directory                     as Dir
import qualified Data.Monoid                          as M
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

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab ([B.AbbrJudge c], [B.AbbrJudge c])
runSection global sects =
    do let s2 = M.mconcat sects
           g2 = global { C.globalJudges = C.sectionJudge s2 }
       runSectionBody g2 s2

runSectionBody :: forall c. (C.CNil c, Ord c) => C.Global c -> C.Section c -> B.Ab ([B.AbbrJudge c], [B.AbbrJudge c])
runSectionBody global sec =
    do judgesV <- run $ C.assertViolated ass
       judgesN <- run $ C.assertNormal ass
       Right (B.abbrTrim judgesV, B.abbrTrim judgesN)
    where
      ass :: C.AbbrAsserts c
      ass = sectionLinkedAssert sec

      run :: C.AbbrAsserts c -> B.Ab ([B.AbbrJudge c])
      run = sequence . map B.abbrAb . run2

      run2 :: C.AbbrAsserts c -> [B.Abbr (B.Ab [B.Judge c])]
      run2 = B.abbrMap $ C.runAssertJudges global

-- {-| Run section.
--     Output section has judges calculated
--     from assertions in input section. -}
-- runSection :: (C.CContent c) => C.Global c -> B.AbMap (C.Section c)
-- runSection global sec =
--     do let g2          = global { C.globalJudges = C.sectionJudge sec }
--            allAsserts  = sectionLinkedAssert sec
--            asserts p   = filter (p . C.assertType) allAsserts
--            judges p    = C.runAssertJudges g2 (asserts p)
--        violate <- judges (== C.AssertViolate)
--        output  <- judges (/= C.AssertViolate)
--        Right $ sec { C.sectionJudge   = output
--                    , C.sectionViolate = violate }

{-| Select assertions like 'sectionAssert'.
    It returns relmap-liked assertions.
    We can run these assertions using 'C.runAssertJudges'. -}
sectionLinkedAssert :: forall c. C.Section c -> [B.Abbr [C.Assert c]]
sectionLinkedAssert C.Section { C.sectionRelmap = rs, C.sectionAssert = ass }
    = linkeAssert rs ass

linkeAssert :: forall c. [B.Named (C.Relmap c)] -> B.Map [B.Abbr [C.Assert c]]
linkeAssert relmaps = map $ fmap $ map link where
    link :: B.Map (C.Assert c)
    link = C.assertMap $ C.relmapLinker relmaps


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Struct.SectionIO
(
-- * Reading section
  sectionRead
, sectionFile

-- * Running section
, runSectionJudges
, runSectionString
, runSectionIO
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Section
import Koshucode.Baala.Base.Struct.Half.Clause
import Koshucode.Baala.Base.Struct.Half.HalfRelmap
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Prelude as Prelude



-- ----------------------  Reading section

sectionRead
    :: (StringValue v)
    => Section v   -- ^ Section that is same type to result section
    -> String     -- ^ Source text
    -> AbortOr (Section v)  -- ^ Result section from source text
sectionRead md src = md2 where
    (ConsRelmap half full) = sectionCons md
    md2 = consSection full $ consClause half $ tokens src

-- | Read section from file.
sectionFile
    :: (StringValue v) => Section v -> FilePath
    -> IO (AbortOr (Section v))
sectionFile md path =
    do code <- readFile path
       return $ sectionRead md code



-- ----------------------  Running section

-- | Convert input judges into output judges by editing section.
--   This function calls 'runAssert' for list of assertions in section,
--   and @runAssert@ calls 'Koshucode.Baala.Base.Struct.Full.Relmap.runRelmap'
--   for relmap in assertion.
runSectionJudges
    :: (Value v)
    => Section v   -- ^ Editing section
    -> [Judge v]  -- ^ Input judges
    -> AbortOr [Judge v]  -- ^ Output judges
runSectionJudges md =
    let asserts = sectionLinkedAssert md
    in runAssertJudges asserts  -- run assertions
       . Prelude.unique         -- remove duplicate judges
       . map abcJudge           -- sort terms of judges

-- | Convert input data into output data by editing section.
runSectionString :: (Value v) => Section v -> String -> AbortOr String
runSectionString md input = do
  inputMd <- sectionRead md input         -- String     -> Section v  : read section
  let inputJs = sectionJudge inputMd      -- Section v  -> [Judges v] : select here data
  outputJs <- runSectionJudges md inputJs -- [Judges v] -> [Judges v] : run section
  Right $ showJudges outputJs             -- [Judges v] -> String     : show judges

showJudges :: (Ord v, Pretty v) => [Judge v] -> String
showJudges = unlines . map (show . Prelude.doc)

-- | Run editing section.
runSectionIO :: (Value v) => Section v -> IO ()
runSectionIO md = do
  input <- getContents
  abortIO putStr $ runSectionString md input


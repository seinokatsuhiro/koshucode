{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Struct.ModuleIO
(
-- * Reading module
  moduleRead
, moduleFile

-- * Running module
, runModuleJudges
, runModuleString
, runModuleIO
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Module
import Koshucode.Baala.Base.Struct.Half.HalfModule
import Koshucode.Baala.Base.Struct.Half.HalfRelmap
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Prelude as Prelude



-- ----------------------  Reading module

moduleRead
    :: (StringValue v)
    => Module v   -- ^ Module that is same type to result module
    -> String     -- ^ Source text
    -> AbortOr (Module v)  -- ^ Result module from source text
moduleRead md src = md2 where
    (ConsRelmap half full) = moduleCons md
    md2 = consFullModule full $ consHalfModule half $ tokens src

-- | Read module from file.
moduleFile
    :: (StringValue v) => Module v -> FilePath
    -> IO (AbortOr (Module v))
moduleFile md path =
    do code <- readFile path
       return $ moduleRead md code



-- ----------------------  Running module

-- | Convert input judges into output judges by editing module.
--   This function calls 'runAssert' for list of assertions in module,
--   and @runAssert@ calls 'Koshucode.Baala.Base.Struct.Full.Relmap.runRelmap'
--   for relmap in assertion.
runModuleJudges
    :: (Value v)
    => Module v   -- ^ Editing module
    -> [Judge v]  -- ^ Input judges
    -> AbortOr [Judge v]  -- ^ Output judges
runModuleJudges md =
    let asserts = moduleLinkedAssert md
    in runAssertJudges asserts  -- run assertions
       . Prelude.unique         -- remove duplicate judges
       . map abcJudge           -- sort terms of judges

-- | Convert input data into output data by editing module.
runModuleString :: (Value v) => Module v -> String -> AbortOr String
runModuleString md input = do
  inputMd <- moduleRead md input         -- String     -> Module v   : read module
  let inputJs = moduleJudge inputMd      -- Module v   -> [Judges v] : select here data
  outputJs <- runModuleJudges md inputJs -- [Judges v] -> [Judges v] : run module
  Right $ showJudges outputJs            -- [Judges v] -> String     : show judges

showJudges :: (Ord v, Pretty v) => [Judge v] -> String
showJudges = unlines . map (show . Prelude.doc)

-- | Run editing module.
runModuleIO :: (Value v) => Module v -> IO ()
runModuleIO md = do
  input <- getContents
  abortIO putStr $ runModuleString md input


{-# OPTIONS_GHC -Wall #-}

-- | Read section.

module Koshucode.Baala.Core.Section.Read
( readSection,
  readSectionText,
) where

import qualified System.Directory                     as Dir
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Message         as Msg

-- | Read section from certain resource.
readSection :: (C.CContent c) => C.Section c -> B.Resource -> IO (B.Ab (C.Section c))
readSection root res = dispatch $ B.resourceName res where
    dispatch (B.ResourceFile path)
        = do exist <- Dir.doesFileExist path
             case exist of
               False  ->  return $ Msg.noFile path
               True   ->  ioRead =<< readFile path

    dispatch (B.ResourceText code)  =  ioRead code
    dispatch (B.ResourceStdin)      =  ioRead =<< getContents
    dispatch (B.ResourceURL _)      =  error "Not implemented read from URL"

    ioRead = return . readSectionCode root res

readSectionCode :: (C.CContent c)
    => C.Section c -> B.Resource -> String -> B.Ab (C.Section c)
readSectionCode root res =
    C.consSection root res . C.consClause B.<=< B.tokenLines res

-- | Read section from text.
readSectionText :: (C.CContent c) => C.Section c -> String -> B.Ab (C.Section c)
readSectionText root code = readSectionCode root (B.resourceOf code) code


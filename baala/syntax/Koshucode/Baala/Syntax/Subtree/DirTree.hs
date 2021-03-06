{-# OPTIONS_GHC -Wall #-}

-- | Directory tree.

module Koshucode.Baala.Syntax.Subtree.DirTree
  ( dirTrees,
    withCurrentDirectory,
    listDirectory,
  ) where

import qualified System.Directory                        as Dir
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax.Subtree.Subtree  as S

-- | Create selective directory trees.
--
--   >>> import Koshucode.Baala.Syntax.Subtree.Filter as S
--   >>> B.printTrees O.# dirTrees [] "." [S.SubtreeR (S.sivmapId O.++ S.sivmapOmit "dist") [S.subtreeL $ S.sivmapKeep "S(*)"]]
--   > [] "data"
--     - "SLOC.k"
--   > [] "Koshucode"
--     > [] "Baala"
--       > [] "Syntax"
--         > [] "Attr"
--           - "Slot.hs"
--         > [] "Subtree"
--           - "Subtree.hs"
--         - "Subtree.hs"
--         > [] "Symbol"
--           - "Short.hs"
--           - "Symbol.hs"
--         - "Symbol.hs"
--         > [] "Token"
--           - "Section.hs"
--         > [] "Tree"
--           - "Split.hs"
--       - "Syntax.hs"
--
dirTrees ::
    [FilePath]                  -- ^ Exclude directory names
    -> FilePath                 -- ^ Beginning directory
    -> [S.SubtreePattern]       -- ^ Directory tree patterns
    -> IO [S.Subtree FilePath]  -- ^ Result directory trees
dirTrees exclude path ps =
    withCurrentDirectory path $ do
      ts <- dirTreesOne exclude ps
      dirTreesRec exclude ts

dirTreesOne :: [FilePath] -> [S.SubtreePattern] -> IO [S.Subtree String]
dirTreesOne exclude ps =
    do fs <- listDirectory "."
       zs <- tree O.<#> filter (`notElem` exclude) fs
       return $ S.subtreeOne ps zs
    where
      tree f = do isDir <- Dir.doesDirectoryExist f
                  return $ case isDir of
                             True  -> B.TreeB ps f []
                             False -> B.TreeL f

dirTreesRec :: [FilePath] -> [S.Subtree String] -> IO [S.Subtree String]
dirTreesRec exclude ts = p O.<#++> ts where
    p t@(B.TreeL _) = return [t]
    p (B.TreeB ps y _) =
        withCurrentDirectory y $ do
           zs <- dirTreesOne exclude ps
           case zs of
             [] -> return []
             _  -> do zs' <- dirTreesRec exclude zs
                      return [B.TreeB [] y zs']

-- | Same as 'Dir.withCurrentDirectory'.
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action =
  B.bracket Dir.getCurrentDirectory Dir.setCurrentDirectory $ \ _ -> do
    Dir.setCurrentDirectory dir
    action

-- | Same as 'Dir.listDirectory'.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = filter f <$> Dir.getDirectoryContents path where
    f filename = filename /= "." && filename /= ".."


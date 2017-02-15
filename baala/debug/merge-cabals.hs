{-# OPTIONS_GHC -Wall #-}
--
--  Merge cabal files for debugging the 'koshu' command.
--  >> runghc merge-cabals.hs ../*/*.cabal
--

module Main (main) where

import Data.Monoid ((<>))
import qualified System.Environment                           as E
import qualified Distribution.License                         as L
import qualified Distribution.Package                         as C
import qualified Distribution.PackageDescription              as D
import qualified Distribution.PackageDescription.Parse        as P
import qualified Distribution.PackageDescription.PrettyPrint  as P
import qualified Distribution.Verbosity                       as V

-- --------------------------------------------  Main

main :: IO ()
main = do

  -- Input library cabal files.
  files <- E.getArgs
  let files' = filter (`notElem` excludes) files
  mapM_ inputMessage files'

  -- Merge files.
  cabals <- P.readPackageDescription V.silent `mapM` files'
  let cabal = mergeCabals cabals
      debug = debugCabal cabal

  -- Output debug cabal file.
  P.writeGenericPackageDescription debugFile debug
  outputMessage debugFile

excludes :: [FilePath]
excludes = [ "../debug/" ++ debugFile
           , "../toolkit/koshucode-baala-toolkit.cabal"]

inputMessage :: FilePath -> IO ()
inputMessage path = putStrLn $ "Input from " ++ q path

outputMessage :: FilePath -> IO ()
outputMessage path = putStrLn $ "Output to " ++ q path

q :: String -> String
q s = "'" ++ s ++ "'"

-- --------------------------------------------  Debug Cabal file

type Cabal = D.GenericPackageDescription
type Lib = D.CondTree D.ConfVar [C.Dependency] D.Library
type Join a = a -> a -> a

debugName :: String
debugName = "debug"

debugFile :: FilePath
debugFile = debugName ++ ".cabal"

debugCabal :: Cabal -> Cabal
debugCabal cabal =
    cabal { D.packageDescription
            = let desc = D.packageDescription cabal
              in desc { D.package
                        = let pkg = D.package desc
                          in pkg { C.pkgName = C.PackageName debugName }
                      , D.license = L.OtherLicense
                      , D.licenseFiles = []
                      }}

mergeCabals :: [Cabal] -> Cabal
mergeCabals (d:ds) = foldr joinCabal d ds
mergeCabals [] = D.GenericPackageDescription
                 D.emptyPackageDescription [] Nothing [] [] []

joinCabal :: Join Cabal
joinCabal l r
    = l { D.genPackageFlags = D.genPackageFlags l <> D.genPackageFlags r
        , D.condLibrary     = joinMaybeLib (D.condLibrary l) (D.condLibrary r)
        , D.condExecutables = [] }

joinMaybeLib :: Join (Maybe Lib)
joinMaybeLib (Just l) (Just r) = Just $ joinLib l r
joinMaybeLib _ _ = error "No library"

joinLib :: Join Lib
joinLib l r = lr where
    lib    = join D.condTreeData
    join f = f l <> f r
    lr     = l { D.condTreeData        = lib { D.exposedModules = [] }
               , D.condTreeConstraints = join D.condTreeConstraints
               , D.condTreeComponents  = join D.condTreeComponents }


{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Get
( ropBase,
  ropBuild,
  getCox,
  getContent,
  getOptContent,
  getContents,
  getFiller,
  getTermCoxes,
  getNamedCoxes,
) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op

ropBase :: C.RopUse c -> [C.Cop c]
ropBase = C.globalFunction . C.ropGlobal

ropBuild :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
ropBuild = C.coxBuild . C.globalSyntax . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas use = mapM (B.namedMapM $ ropBuild use)

getCox :: (C.CContent c) => C.RopUse c -> String -> B.Ab (C.Cox c)
getCox use = ropBuild use . B.treeWrap B.<=< Op.getTrees use

getNamedCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getNamedCoxes use = ropNamedAlphas use B.<=< Op.getWordTrees use 

getTermCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getTermCoxes use = ropNamedAlphas use B.<=< Op.getTermTrees use

getContents :: (C.CContent c) => C.RopUse c -> String -> B.Ab [c]
getContents use name =
    do trees <- Op.getTrees use name
       let trees2 = B.treeWrap `map` B.divideTreesByColon trees
       runCoxTree use `mapM` trees2

getContent :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getContent use name =
    do tree <- Op.getTree use name
       runCoxTree use tree

runCoxTree :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab c
runCoxTree use tree =
    do alpha <- ropBuild use tree
       C.coxRunCox (ropBase use, []) B.mempty [] alpha

getOptContent :: (C.CContent c) => c -> C.RopUse c -> String -> B.Ab c
getOptContent opt = Op.getOption opt getContent

getFiller :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getFiller = getOptContent C.empty


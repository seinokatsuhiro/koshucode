{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Get
( -- * Cox
  getCox,
  getTermCoxes,
  getNamedCoxes,
  getWhere,

  -- * Content
  getContent,
  getContents,
  getOptContent,
  getFiller,

  getInt,
) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Message  as Msg


-- --------------------------------------------  Cox

-- | Get relmap attribute as single cox.
getCox :: (C.CContent c) => C.RopUse c -> String -> B.Ab (C.Cox c)
getCox use = ropBuild use . B.wrapTrees B.<=< Op.getTrees use

-- | Get relmap attribute as cox list with name.
getNamedCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getNamedCoxes use = ropNamedAlphas use B.<=< Op.getWordTrees use 

-- | Get relmap attribute as cox list with term name.
getTermCoxes :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getTermCoxes use = ropNamedAlphas use B.<=< Op.getTermTrees use

ropBuild :: (C.CContent c) => C.RopUse c -> B.TTreeToAb (C.Cox c)
ropBuild = C.coxBuildG . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.RopUse c -> [B.NamedTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas use = mapM (B.namedMapM $ ropBuild use)


-- --------------------------------------------  Where

getWhere :: (C.CContent c) => C.RopUse c -> String -> B.Ab (C.CopSet c)
getWhere u name =
    do wh <- Op.getOption [] getWhereBody u name
       let copset = C.globalCopset $ C.ropGlobal u
       Right $ copset { C.copsetDerived = wh }

getWhereBody :: (C.CContent c) => C.RopUse c -> String -> B.Ab [C.NamedCox c]
getWhereBody u name =
    do xs <- Op.getTreesByColon u name
       getWhereClause u `mapM` xs

getWhereClause :: (C.CContent c) => C.RopUse c -> [B.TTree] -> B.Ab (C.NamedCox c)
getWhereClause u trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- ropBuild u $ B.wrapTrees bo
       let cp = B.codePts $ head $ B.untrees trees
       case vs of
         [] -> Right (n, cox)
         _  -> Right (n, C.coxForm cp (Just n) vs cox)

getWhereHead :: [B.TTree] -> B.Ab (String, [String])
getWhereHead [] = Msg.adlib "getWhereHead"
getWhereHead (n : vs) =
    do n'  <- getTextFromTree n
       vs' <- mapM getTextFromTree vs
       Right (n', vs')

getTreesByEqual :: [B.TTree] -> B.Ab ([B.TTree], [B.TTree])
getTreesByEqual trees =
    case B.divideTreesByEqual trees of
      [left, right] -> Right (left, right)
      _             -> Msg.adlib "getTreesByEqual"

getTextFromTree :: B.TTree -> B.Ab String
getTextFromTree (B.TreeL (B.TText _ B.TextRaw n)) = Right n
getTextFromTree _ = Msg.adlib "getTextFromTree"




-- --------------------------------------------  Content

-- | Get relmap attribute as calculated content.
getContent :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getContent use name =
    do tree <- Op.getTree use name
       calcTree use tree

-- | Get relmap attribute as list of calculated contents.
getContents :: (C.CContent c) => C.RopUse c -> String -> B.Ab [c]
getContents use name =
    do trees <- Op.getTrees use name
       let trees2 = B.wrapTrees `map` B.divideTreesByColon trees
       calcTree use `mapM` trees2

calcTree :: (C.CContent c) => C.RopUse c -> C.CalcContent c
calcTree = C.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (C.CContent c) => c -> C.RopUse c -> String -> B.Ab c
getOptContent opt = Op.getOption opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (C.CContent c) => C.RopUse c -> String -> B.Ab c
getFiller = getOptContent C.empty

getInt :: (C.CContent c) => C.RopUse c -> String -> B.Ab Int
getInt use name =
    do dec <- C.getDec $ getContent use name
       Right $ B.decimalNum dec


{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Cox.Get
  ( -- * Cox
    getCox, getMaybeCox, getOptionCox,
    getTermCoxes,
    getNamedCoxes,
    getWhere,
  
    -- * Content
    getContent, getContents,
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
getCox :: (C.CContent c) => Op.RopGet c (C.Cox c)
getCox med = ropBuild med . B.ttreeGroup B.<=< Op.getTrees med

getOptionCox :: (C.CContent c) => c -> Op.RopGet c (C.Cox c)
getOptionCox c = Op.getOption (C.CoxLit [] c) getCox

getMaybeCox :: (C.CContent c) => Op.RopGet c (Maybe (C.Cox c))
getMaybeCox = Op.getMaybe getCox

-- | Get relmap attribute as cox list with name.
getNamedCoxes :: (C.CContent c) => Op.RopGet c [C.NamedCox c]
getNamedCoxes med = ropNamedAlphas med B.<=< Op.getWordTrees med 

-- | Get relmap attribute as cox list with term name.
getTermCoxes :: (C.CContent c) => Op.RopGet c [C.NamedCox c]
getTermCoxes med = ropNamedAlphas med B.<=< Op.getTermTrees med

ropBuild :: (C.CContent c) => C.Intmed c -> B.TTreeToAb (C.Cox c)
ropBuild = C.coxBuildG . C.ropGlobal

ropNamedAlphas :: (C.CContent c) => C.Intmed c -> [B.NamedTree] -> B.Ab [C.NamedCox c]
ropNamedAlphas med = mapM (B.namedMapM $ ropBuild med)


-- --------------------------------------------  Where

getWhere :: (C.CContent c) => Op.RopGet c (C.CopSet c)
getWhere u name =
    do wh <- Op.getOption [] getWhereBody u name
       let copset = C.globalCopset $ C.ropGlobal u
       Right $ copset { C.copsetDerived = wh }

getWhereBody :: (C.CContent c) => Op.RopGet c [C.NamedCox c]
getWhereBody u name =
    do xs <- Op.getTreesByColon u name
       getWhereClause u `mapM` xs

getWhereClause :: (C.CContent c) => C.Intmed c -> [B.TTree] -> B.Ab (C.NamedCox c)
getWhereClause u trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- ropBuild u $ B.ttreeGroup bo
       let cp = B.codePtList $ head $ B.untrees trees
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
getTextFromTree (B.TextLeafRaw _ n)  = Right n
getTextFromTree _ = Msg.adlib "getTextFromTree"




-- --------------------------------------------  Content

-- | Get relmap attribute as calculated content.
getContent :: (C.CContent c) => Op.RopGet c c
getContent med name =
    do tree <- Op.getTree med name
       calcTree med tree

-- | Get relmap attribute as list of calculated contents.
getContents :: (C.CContent c) => Op.RopGet c [c]
getContents med name =
    do trees <- Op.getTrees med name
       let trees2 = B.ttreeGroup `map` B.divideTreesByColon trees
       calcTree med `mapM` trees2

calcTree :: (C.CContent c) => C.Intmed c -> C.CalcContent c
calcTree = C.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (C.CContent c) => c -> Op.RopGet c c
getOptContent opt = Op.getOption opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (C.CContent c) => Op.RopGet c c
getFiller = getOptContent C.empty

getInt :: (C.CContent c) => Op.RopGet c Int
getInt med name =
    do dec <- C.getDec $ getContent med name
       Right $ B.decimalNum dec


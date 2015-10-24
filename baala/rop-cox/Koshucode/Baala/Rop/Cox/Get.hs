{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Cox.Get
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
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Builtin  as Op
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- --------------------------------------------  Cox

-- | Get relmap attribute as single cox.
getCox :: (D.CContent c) => Op.RopGet c (D.Cox c)
getCox med = ropBuild med . D.ttreeGroup B.<=< Op.getTrees med

getOptionCox :: (D.CContent c) => c -> Op.RopGet c (D.Cox c)
getOptionCox c = Op.getOption (D.CoxLit [] c) getCox

getMaybeCox :: (D.CContent c) => Op.RopGet c (Maybe (D.Cox c))
getMaybeCox = Op.getMaybe getCox

-- | Get relmap attribute as cox list with name.
getNamedCoxes :: (D.CContent c) => Op.RopGet c [D.NamedCox c]
getNamedCoxes med = ropNamedAlphas med B.<=< Op.getWordTrees med 

-- | Get relmap attribute as cox list with term name.
getTermCoxes :: (D.CContent c) => Op.RopGet c [D.NamedCox c]
getTermCoxes med = ropNamedAlphas med B.<=< Op.getTermTrees med

ropBuild :: (D.CContent c) => C.Intmed c -> D.TTreeToAb (D.Cox c)
ropBuild = C.coxBuildG . C.ropGlobal

ropNamedAlphas :: (D.CContent c) => C.Intmed c -> [D.NamedTree] -> B.Ab [D.NamedCox c]
ropNamedAlphas med = mapM (B.namedMapM $ ropBuild med)


-- --------------------------------------------  Where

getWhere :: (D.CContent c) => Op.RopGet c (D.CopSet c)
getWhere u name =
    do wh <- Op.getOption [] getWhereBody u name
       let copset = C.globalCopset $ C.ropGlobal u
       Right $ copset { D.copsetDerived = wh }

getWhereBody :: (D.CContent c) => Op.RopGet c [D.NamedCox c]
getWhereBody u name =
    do xs <- Op.getTreesByColon u name
       getWhereClause u `mapM` xs

getWhereClause :: (D.CContent c) => C.Intmed c -> [D.TTree] -> B.Ab (D.NamedCox c)
getWhereClause u trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- ropBuild u $ D.ttreeGroup bo
       let cp = B.codePtList $ head $ B.untrees trees
       case vs of
         [] -> Right (n, cox)
         _  -> Right (n, D.coxForm cp (Just n) vs cox)

getWhereHead :: [D.TTree] -> B.Ab (String, [String])
getWhereHead [] = Msg.adlib "getWhereHead"
getWhereHead (n : vs) =
    do n'  <- getTextFromTree n
       vs' <- mapM getTextFromTree vs
       Right (n', vs')

getTreesByEqual :: [D.TTree] -> B.Ab ([D.TTree], [D.TTree])
getTreesByEqual trees =
    case D.divideTreesByEqual trees of
      [left, right] -> Right (left, right)
      _             -> Msg.adlib "getTreesByEqual"

getTextFromTree :: D.TTree -> B.Ab String
getTextFromTree (D.TextLeafRaw _ n)  = Right n
getTextFromTree _ = Msg.adlib "getTextFromTree"




-- --------------------------------------------  Content

-- | Get relmap attribute as calculated content.
getContent :: (D.CContent c) => Op.RopGet c c
getContent med name =
    do tree <- Op.getTree med name
       calcTree med tree

-- | Get relmap attribute as list of calculated contents.
getContents :: (D.CContent c) => Op.RopGet c [c]
getContents med name =
    do trees <- Op.getTrees med name
       let trees2 = D.ttreeGroup `map` D.divideTreesByColon trees
       calcTree med `mapM` trees2

calcTree :: (D.CContent c) => C.Intmed c -> D.ContentCalc c
calcTree = D.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (D.CContent c) => c -> Op.RopGet c c
getOptContent opt = Op.getOption opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (D.CContent c) => Op.RopGet c c
getFiller = getOptContent D.empty

getInt :: (D.CContent c) => Op.RopGet c D.DecimalInteger
getInt med name =
    do dec <- D.getDec $ getContent med name
       Right $ D.decimalNum dec


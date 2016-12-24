{-# OPTIONS_GHC -Wall #-}

-- | Content expressions in relmap attributes.

module Koshucode.Baala.Rop.Base.Get.Cox
  ( -- * Cox
    getCox, getMaybeCox, getOptionCox,
    getTermCoxes,
    getWhere,
  
    -- * Content
    getContent, getContents,
    getOptContent,
    getFiller,
  
    getInt,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Syntax.Pattern    as P
import qualified Koshucode.Baala.Rop.Base.Get.Get  as Rop
import qualified Koshucode.Baala.Rop.Base.Get.Rel  as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- --------------------------------------------  Cox

-- | Get relmap attribute as single cox.
getCox :: (K.CContent c) => Rop.RopGet c (K.Cox c)
getCox med = ropBuild med . K.ttreeGroup K.<.> Rop.getTrees med

-- | Get optional content expression with default content.
getOptionCox :: (K.CContent c) => c -> Rop.RopGet c (K.Cox c)
getOptionCox c = Rop.getOption (K.CoxLit [] c) getCox

-- | Get optional content expression.
getMaybeCox :: (K.CContent c) => Rop.RopGet c (Maybe (K.Cox c))
getMaybeCox = Rop.getMaybe getCox

-- | Get relmap attribute as cox list with term name.
getTermCoxes :: (K.CContent c) => Rop.RopGet c [K.Term (K.Cox c)]
getTermCoxes med = ropNamedAlphas med K.<.> Rop.getTermTrees med

ropBuild :: (K.CContent c) => C.Intmed c -> K.Tree -> K.Ab (K.Cox c)
ropBuild = C.treeCoxG . C.ropGlobal

ropNamedAlphas :: (K.CContent c) => C.Intmed c -> [(n, K.Tree)] -> K.Ab [(n, K.Cox c)]
ropNamedAlphas med = mapM (K.sndM $ ropBuild med)


-- --------------------------------------------  Where

-- | Get where attribute as operator set.
getWhere :: (K.CContent c) => Rop.RopGet c (K.CopSet c)
getWhere u name =
    do wh <- Rop.getOption [] getWhereBody u name
       let copset = C.globalCopset $ C.ropGlobal u
       Right $ copset { K.copsetDerived = wh }

getWhereBody :: (K.CContent c) => Rop.RopGet c [K.NamedCox c]
getWhereBody u name =
    do xs <- Rop.getTreesByColon u name
       getWhereClause u `mapM` xs

getWhereClause :: (K.CContent c) => C.Intmed c -> [K.Tree] -> K.Ab (K.NamedCox c)
getWhereClause u trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- ropBuild u $ K.ttreeGroup bo
       let cp = K.getCPs $ head $ K.untrees trees
       case vs of
         [] -> Right (n, cox)
         _  -> Right (n, K.coxForm cp (Just n) vs cox)

getWhereHead :: [K.Tree] -> K.Ab (String, [String])
getWhereHead [] = Msg.adlib "getWhereHead"
getWhereHead (n : vs) =
    do n'  <- getTextFromTree n
       vs' <- mapM getTextFromTree vs
       Right (n', vs')

getTreesByEqual :: [K.Tree] -> K.Ab ([K.Tree], [K.Tree])
getTreesByEqual trees =
    case K.divideTreesByEqual trees of
      [left, right] -> Right (left, right)
      _             -> Msg.adlib "getTreesByEqual"

getTextFromTree :: K.Tree -> K.Ab String
getTextFromTree (P.LRaw n) = Right n
getTextFromTree _ = Msg.adlib "getTextFromTree"




-- --------------------------------------------  Content

-- | Get relmap attribute as calculated content.
getContent :: (K.CContent c) => Rop.RopGet c c
getContent med name =
    do tree <- Rop.getTree med name
       calcTree med tree

-- | Get relmap attribute as list of calculated contents.
getContents :: (K.CContent c) => Rop.RopGet c [c]
getContents med name =
    do trees <- Rop.getTrees med name
       let trees2 = K.ttreeGroup `map` K.divideTreesByColon trees
       calcTree med `mapM` trees2

calcTree :: (K.CContent c) => C.Intmed c -> K.CalcContent c
calcTree = K.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (K.CContent c) => c -> Rop.RopGet c c
getOptContent opt = Rop.getOption opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (K.CContent c) => Rop.RopGet c c
getFiller = getOptContent K.empty

-- | Get decimal integer content.
getInt :: (K.CContent c) => Rop.RopGet c K.DecimalInteger
getInt med name =
    do dec <- K.getDec $ getContent med name
       Right $ K.decimalNum dec


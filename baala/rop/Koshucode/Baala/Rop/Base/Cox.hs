{-# OPTIONS_GHC -Wall #-}

-- | Content expressions in relmap attributes.

module Koshucode.Baala.Rop.Base.Cox
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
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Syntax.Pattern    as P
import qualified Koshucode.Baala.Rop.Base.Get      as Rop
import qualified Koshucode.Baala.Rop.Base.Rel      as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- --------------------------------------------  Cox

-- | Get relmap attribute as single cox.
getCox :: (D.CContent c) => Rop.RopGet c (D.Cox c)
getCox med = ropBuild med . S.ttreeGroup B.<.> Rop.getTrees med

-- | Get optional content expression with default content.
getOptionCox :: (D.CContent c) => c -> Rop.RopGet c (D.Cox c)
getOptionCox c = Rop.getOption (D.CoxLit [] c) getCox

-- | Get optional content expression.
getMaybeCox :: (D.CContent c) => Rop.RopGet c (Maybe (D.Cox c))
getMaybeCox = Rop.getMaybe getCox

-- | Get relmap attribute as cox list with name.
getNamedCoxes :: (D.CContent c) => Rop.RopGet c [D.NamedCox c]
getNamedCoxes med = ropNamedAlphas med B.<.> Rop.getWordTrees med 

-- | Get relmap attribute as cox list with term name.
getTermCoxes :: (D.CContent c) => Rop.RopGet c [S.Term (D.Cox c)]
getTermCoxes med = ropNamedAlphas med B.<.> Rop.getTermTrees med

ropBuild :: (D.CContent c) => C.Intmed c -> S.Tree -> B.Ab (D.Cox c)
ropBuild = C.treeCoxG . C.ropGlobal

ropNamedAlphas :: (D.CContent c) => C.Intmed c -> [(n, S.Tree)] -> B.Ab [(n, D.Cox c)]
ropNamedAlphas med = mapM (B.sndM $ ropBuild med)


-- --------------------------------------------  Where

-- | Get where attribute as operator set.
getWhere :: (D.CContent c) => Rop.RopGet c (D.CopSet c)
getWhere u name =
    do wh <- Rop.getOption [] getWhereBody u name
       let copset = C.globalCopset $ C.ropGlobal u
       Right $ copset { D.copsetDerived = wh }

getWhereBody :: (D.CContent c) => Rop.RopGet c [D.NamedCox c]
getWhereBody u name =
    do xs <- Rop.getTreesByColon u name
       getWhereClause u `mapM` xs

getWhereClause :: (D.CContent c) => C.Intmed c -> [S.Tree] -> B.Ab (D.NamedCox c)
getWhereClause u trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- ropBuild u $ S.ttreeGroup bo
       let cp = B.getCPs $ head $ B.untrees trees
       case vs of
         [] -> Right (n, cox)
         _  -> Right (n, D.coxForm cp (Just n) vs cox)

getWhereHead :: [S.Tree] -> B.Ab (String, [String])
getWhereHead [] = Msg.adlib "getWhereHead"
getWhereHead (n : vs) =
    do n'  <- getTextFromTree n
       vs' <- mapM getTextFromTree vs
       Right (n', vs')

getTreesByEqual :: [S.Tree] -> B.Ab ([S.Tree], [S.Tree])
getTreesByEqual trees =
    case S.divideTreesByEqual trees of
      [left, right] -> Right (left, right)
      _             -> Msg.adlib "getTreesByEqual"

getTextFromTree :: S.Tree -> B.Ab String
getTextFromTree (P.LRaw n) = Right n
getTextFromTree _ = Msg.adlib "getTextFromTree"




-- --------------------------------------------  Content

-- | Get relmap attribute as calculated content.
getContent :: (D.CContent c) => Rop.RopGet c c
getContent med name =
    do tree <- Rop.getTree med name
       calcTree med tree

-- | Get relmap attribute as list of calculated contents.
getContents :: (D.CContent c) => Rop.RopGet c [c]
getContents med name =
    do trees <- Rop.getTrees med name
       let trees2 = S.ttreeGroup `map` S.divideTreesByColon trees
       calcTree med `mapM` trees2

calcTree :: (D.CContent c) => C.Intmed c -> D.CalcContent c
calcTree = D.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (D.CContent c) => c -> Rop.RopGet c c
getOptContent opt = Rop.getOption opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (D.CContent c) => Rop.RopGet c c
getFiller = getOptContent D.empty

-- | Get decimal integer content.
getInt :: (D.CContent c) => Rop.RopGet c D.DecimalInteger
getInt med name =
    do dec <- D.getDec $ getContent med name
       Right $ D.decimalNum dec

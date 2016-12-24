{-# OPTIONS_GHC -Wall #-}

-- | Content expressions in relmap attributes.

module Koshucode.Baala.Rop.Base.Get.Cox
  ( -- * Cox
    getCox, getMaybeCox, getOptCox,
    getTermCoxes,
    getWhere,
  
    -- * Content
    getContent, getContents,
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

-- | Get required single content expression.
getCox :: (K.CContent c) => Rop.RopGet (K.Cox c) c
getCox med = buildCox med K.<.> Rop.getTree med

-- | Get optional single content expression.
getMaybeCox :: (K.CContent c) => Rop.RopGet (K.MaybeCox c) c
getMaybeCox = Rop.getMaybe getCox

-- | Get optional content expression with default content.
getOptCox :: (K.CContent c) => c -> Rop.RopGet (K.Cox c) c
getOptCox c = Rop.getOpt (K.coxLit c) getCox

-- | Get list of expression terms.
--   Empty terms are filled with empty contents,
--   e.g., \/a \/b /E/ is equivalent to \/a () \/b /E/.
getTermCoxes :: (K.CContent c) => Rop.RopGet [K.Term (K.Cox c)] c
getTermCoxes med = buildCoxTerms med K.<.> Rop.getTermTrees med

-- | Build content expression.
buildCox :: (K.CContent c) => C.Intmed c -> K.Tree -> K.Ab (K.Cox c)
buildCox = K.treeCox . copset

-- | Build terms of content expression.
buildCoxTerms :: (K.CContent c) => C.Intmed c -> [K.Term K.Tree] -> K.Ab [K.Term (K.Cox c)]
buildCoxTerms = mapM . K.sndM . buildCox

copset :: C.Intmed c -> K.CopSet c
copset = C.globalCopset . C.getGlobal


-- --------------------------------------------  Where

-- | Get @-where@ parameter as operator set.
getWhere :: (K.CContent c) => Rop.RopGet (K.CopSet c) c
getWhere med name =
    do wh <- Rop.getOpt [] getWhereBody med name
       let cops = copset med
       Right $ cops { K.copsetDerived = wh }

getWhereBody :: (K.CContent c) => Rop.RopGet [K.NamedCox c] c
getWhereBody med name =
    do xs <- Rop.getTreesByColon med name
       getWhereClause med K.<#> xs

getWhereClause :: (K.CContent c) => C.Intmed c -> [K.Tree] -> K.Ab (K.NamedCox c)
getWhereClause med trees =
    do (he, bo) <- getTreesByEqual trees
       (n, vs)  <- getWhereHead he
       cox      <- buildCox med $ K.ttreeGroup bo
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
getContents :: (K.CContent c) => Rop.RopGet [c] c
getContents med name =
    do trees <- Rop.getTrees med name
       let trees2 = K.ttreeGroup `map` K.divideTreesByColon trees
       calcTree med `mapM` trees2

calcTree :: (K.CContent c) => C.Intmed c -> K.CalcContent c
calcTree = K.calcContent . C.ropCopset

-- | Get relmap attribute as optional content.
getOptContent :: (K.CContent c) => c -> Rop.RopGet c c
getOptContent opt = Rop.getOpt opt getContent

-- | Get relmap attribute as filler content, i.e., given content or empty.
getFiller :: (K.CContent c) => Rop.RopGet c c
getFiller = getOptContent K.empty

-- | Get decimal integer content.
getInt :: (K.CContent c) => Rop.RopGet K.DecimalInteger c
getInt med name =
    do dec <- K.getDec $ getContent med name
       Right $ K.decimalNum dec


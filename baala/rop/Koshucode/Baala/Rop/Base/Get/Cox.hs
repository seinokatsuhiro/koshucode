{-# OPTIONS_GHC -Wall #-}

-- | Content expressions in relmap attributes.

module Koshucode.Baala.Rop.Base.Get.Cox
  ( -- * Content
    getContent, getContents,
    getFiller, getInt,
    getNamedContentTerms, getOptContentTerms,

    -- * Cox
    getCox, getMaybeCox, getOptCox,
    getCoxTerms, getNamedCoxTerms, getOptCoxTerms,
    getWhere,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Syntax.Pattern    as P
import qualified Koshucode.Baala.Rop.Base.Get.Get  as Rop
import qualified Koshucode.Baala.Rop.Base.Get.Rel  as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- --------------------------------------------  Content

-- | Get relmap parameter as calculated content.
getContent :: (K.CContent c) => Rop.RopGet c c
getContent med name =
    do tree <- Rop.getTree med name
       calcTree med tree

-- | Get relmap parameter as list of calculated contents.
getContents :: (K.CContent c) => Rop.RopGet [c] c
getContents med name =
    do trees <- Rop.getTrees med name
       let trees2 = K.ttreeGroup `map` K.divideTreesByColon trees
       calcTree med `mapM` trees2

-- | Calculate closed content expression.
calcTree :: (K.CContent c) => C.Intmed c -> K.CalcContent c
calcTree = K.calcContent . C.ropCopset

-- | Get relmap parameter as optional content.
getOptContent :: (K.CContent c) => c -> Rop.RopGet c c
getOptContent opt = Rop.getOpt opt getContent

-- | Get relmap parameter as filler content, i.e., given content or empty.
getFiller :: (K.CContent c) => Rop.RopGet c c
getFiller = getOptContent K.empty

-- | Get decimal integer content.
getInt :: (K.CContent c) => Rop.RopGet K.DecimalInteger c
getInt med name =
    do dec <- K.getDec $ getContent med name
       Right $ K.decimalNum dec

-- | Get list of content terms.
getNamedContentTerms :: (K.CContent c) => Rop.RopGet [K.Term c] c
getNamedContentTerms = getOptContentTerms K.pTerm

-- | Get list of content terms.
getOptContentTerms :: (K.CContent c) => (K.TermName -> c) -> Rop.RopGet [K.Term c] c
getOptContentTerms f med = optContentTerms f med K.<.> Rop.getTreesTerms med

-- | Build terms of content.
optContentTerms :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> [K.Term [K.Tree]] -> K.Ab [K.Term c]
optContentTerms f = mapM . optContent f

optContent :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> K.Term [K.Tree] -> K.Ab (K.Term c)
optContent f _   (n, []) = Right (n, f n)
optContent _ med (n, ts) = do c <- calcTree med $ K.ttreeGroup ts
                              Right (n, c)


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

-- | Get list of content expression terms.
--   Empty terms are filled with empty contents,
--   e.g., __\/a__ __\/b__ /E/ is equivalent to __\/a__ () __\/b__ /E/.
getCoxTerms :: (K.CContent c) => Rop.RopGet [K.Term (K.Cox c)] c
getCoxTerms = getOptCoxTerms $ const K.empty

-- | Get list of content expression terms.
--   Empty terms are filled with its term name.
--   e.g., __\/a__ __\/b__ /E/ is equivalent to __\/a__ '\/a __\/b__ /E/.
getNamedCoxTerms :: (K.CContent c) => Rop.RopGet [K.Term (K.Cox c)] c
getNamedCoxTerms = getOptCoxTerms K.pTerm

-- | Get list of content expression terms.
--   Contents of empty terms can be generated using its term name.
getOptCoxTerms :: (K.CContent c) => (K.TermName -> c) -> Rop.RopGet [K.Term (K.Cox c)] c
getOptCoxTerms f med = optCoxTerms f med K.<.> Rop.getTreesTerms med

-- | Build content expression.
buildCox :: (K.CContent c) => C.Intmed c -> K.Tree -> K.Ab (K.Cox c)
buildCox = K.treeCox . C.ropCopset

optCox :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> K.Term [K.Tree] -> K.Ab (K.Term (K.Cox c))
optCox f _   (n, []) = Right (n, K.coxLit $ f n)
optCox _ med (n, ts) = do cox <- buildCox med $ K.ttreeGroup ts
                          Right (n, cox)

-- | Build terms of content expression.
optCoxTerms :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> [K.Term [K.Tree]] -> K.Ab [K.Term (K.Cox c)]
optCoxTerms f = mapM . optCox f


-- --------------------------------------------  Where

-- | Get @-where@ parameter as operator set.
getWhere :: (K.CContent c) => Rop.RopGet (K.CopSet c) c
getWhere med name =
    do wh <- Rop.getOpt [] getWhereBody med name
       let cops = C.ropCopset med
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

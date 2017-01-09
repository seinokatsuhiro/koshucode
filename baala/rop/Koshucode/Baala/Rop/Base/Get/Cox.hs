{-# OPTIONS_GHC -Wall #-}

-- | Content expressions in relmap attributes.

module Koshucode.Baala.Rop.Base.Get.Cox
  ( -- * Content
    getContent, getContents,
    getFiller, getInt, getInteger,
    getNamedContentTerms, getOptContentTerms,

    -- * Content expression
    getCox, getMaybeCox, getOptCox,
    getCoxTerms, getNamedCoxTerms, getOptCoxTerms,

    -- * Derived expression
    getLet, getLetR,

    -- * Portion
    getPotionCount, getPotionCountR,
    getPotionRatio, getPotionRatioR,
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
       K.calcTree med tree

-- | Get relmap parameter as list of calculated contents.
getContents :: (K.CContent c) => Rop.RopGet [c] c
getContents med name =
    do trees <- Rop.getTrees med name
       let trees2 = K.ttreeGroup <$> K.divideTreesByColon trees
       K.calcTree med K.<#> trees2

-- | Get relmap parameter as optional content.
getOptContent :: (K.CContent c) => c -> Rop.RopGet c c
getOptContent opt = Rop.getOpt opt getContent

-- | Get relmap parameter as filler content, i.e., given content or empty.
getFiller :: (K.CContent c) => Rop.RopGet c c
getFiller = getOptContent K.empty

-- | Get int content.
getInt :: (K.CContent c) => Rop.RopGet Int c
getInt med name = fromInteger <$> getInteger med name

-- | Get integer content.
getInteger :: (K.CContent c) => Rop.RopGet Integer c
getInteger med name =
    do dec <- K.getDec $ getContent med name
       Right $ K.decimalNum dec

-- | Get list of content terms.
getNamedContentTerms :: (K.CContent c) => Rop.RopGet [K.Term c] c
getNamedContentTerms = getOptContentTerms K.pTerm

-- | Get list of content terms.
getOptContentTerms :: (K.CContent c) => (K.TermName -> c) -> Rop.RopGet [K.Term c] c
getOptContentTerms f med = optContentTerms f med K.<#.> Rop.getTreesTerms med

-- | Build terms of content.
optContentTerms :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> [K.Term [K.Tree]] -> K.Ab [K.Term c]
optContentTerms f = mapM . optContent f

optContent :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> K.Term [K.Tree] -> K.Ab (K.Term c)
optContent f _   (n, []) = Right (n, f n)
optContent _ med (n, ts) = do c <- K.calcTree med $ K.ttreeGroup ts
                              Right (n, c)


-- --------------------------------------------  Cox

-- | Get required single content expression.
getCox :: (K.CContent c) => Rop.RopGet (K.Cox c) c
getCox med = buildCox med K.<#.> Rop.getTrees med

-- | Get optional single content expression.
getMaybeCox :: (K.CContent c) => Rop.RopGet (K.MaybeCox c) c
getMaybeCox = Rop.getMaybe getCox

-- | Get optional content expression with default content.
getOptCox :: (K.CContent c) => c -> Rop.RopGet (K.Cox c) c
getOptCox c = Rop.getOpt (K.coxLit c) getCox

-- | Get list of content expression terms.
--   Empty terms are filled with empty contents,
--   e.g., __\/a__ __\/b__ /E/ is equivalent to __\/a__ () __\/b__ /E/.
getCoxTerms :: (K.CContent c) => Rop.RopGet [K.TermCox c] c
getCoxTerms = getOptCoxTerms $ const K.empty

-- | Get list of content expression terms.
--   Empty terms are filled with its term name.
--   e.g., __\/a__ __\/b__ /E/ is equivalent to __\/a__ '\/a __\/b__ /E/.
getNamedCoxTerms :: (K.CContent c) => Rop.RopGet [K.TermCox c] c
getNamedCoxTerms = getOptCoxTerms K.pTerm

-- | Get list of content expression terms.
--   Contents of empty terms can be generated using its term name.
getOptCoxTerms :: (K.CContent c) => (K.TermName -> c) -> Rop.RopGet [K.TermCox c] c
getOptCoxTerms f med = optCoxTerms f med K.<#.> Rop.getTreesTerms med

-- | Build content expression.
buildCox :: (K.CContent c) => C.Intmed c -> [K.Tree] -> K.AbCox c
buildCox med = K.treeCox (K.getCops med) . K.ttreeGroup

optCox :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> K.Term [K.Tree] -> K.Ab (K.TermCox c)
optCox f _   (n, []) = Right (n, K.coxLit $ f n)
optCox _ med (n, ts) = do cox <- buildCox med ts
                          Right (n, cox)

-- | Build terms of content expression.
optCoxTerms :: (K.CContent c) => (K.TermName -> c) -> C.Intmed c -> [K.Term [K.Tree]] -> K.Ab [K.TermCox c]
optCoxTerms f = mapM . optCox f


-- --------------------------------------------  Let

-- | Get @-let@ style parameter as set of derived functions.
getLet :: (K.CContent c) => Rop.RopGet (K.CopSet c) c
getLet med name =
    do fs <- Rop.getOpt [] getLetBody med name
       let cops = K.getCops med
       Right $ cops { K.copsetDerived = fs }

-- | 'getLet' with regular parameter name: @-let@.
getLetR :: (K.CContent c) => Rop.RopGet0 (K.CopSet c) c
getLetR med = getLet med "-let"

getLetBody :: (K.CContent c) => Rop.RopGet [K.NamedCox c] c
getLetBody med name =
    do ts <- getLetTrees med name
       getLetClause med K.<#> ts

-- | Get trees delimited by vertical bar.
getLetTrees :: Rop.RopGet [[K.Tree]] c
getLetTrees = Rop.getWith (K.omit null . parse) where
    parse [P.BSet ts] = K.divideTreesByBar ts
    parse ts          = [ts]

getLetClause :: (K.CContent c) => C.Intmed c -> [K.Tree] -> K.Ab (K.NamedCox c)
getLetClause med ts =
    Msg.abPara ts $ do
      (lhs, rhs) <- treesEqDef ts
      (n, vs)    <- getLetHead lhs
      cox        <- buildCox med rhs
      case vs of
        [] -> Right (n, cox)
        _  -> Right (n, K.coxForm ts (Just n) vs cox)

getLetHead :: [K.Tree] -> K.Ab (String, [String])
getLetHead [] = Msg.reqRawText
getLetHead (n : vs) =
    do n'  <- treeRawText n
       vs' <- treeRawText K.<#> vs
       Right (n', vs')

-- | Extract definition form: /L/ = /R/
treesEqDef :: [K.Tree] -> K.Ab (K.Twin [K.Tree])
treesEqDef ts =
    case K.divideTreesByEqual ts of
      [lhs, rhs] -> Right (lhs, rhs)
      _          -> Msg.abPara ts Msg.reqSingleEqual

-- | Extract raw text from token tree.
treeRawText :: K.Tree -> K.Ab String
treeRawText (P.LRaw n) = Right n
treeRawText t          = Msg.abPara t Msg.reqRawText


-- --------------------------------------------  Portion

-- | 'getPotionCount' with regular parameter names:
--   @-order@, @-top@, @-mid@, and @-bot@.
getPotionCountR :: (K.CContent c) => Rop.RopGet0 ([K.TermName], K.Portion) c
getPotionCountR med = getPotionCount med "-order" "-top" "-mid" "-bot"

-- | Get count-type portion parameter.
getPotionCount :: (K.CContent c) => Rop.RopGet4 ([K.TermName], K.Portion) c
getPotionCount med nOrder nTop nMid nBot =
    do order <- Rop.getTerms med nOrder
       top   <- Rop.getMaybe getInt med nTop
       mid   <- Rop.getMaybe getInt med nMid
       bot   <- Rop.getMaybe getInt med nBot
       Right (order, K.def { K.portionTop    = top
                           , K.portionMiddle = mid
                           , K.portionBottom = bot })

-- | 'getPotionRatio' with regular parameter names:
--   @-order@, @-part@, and @-of@.
getPotionRatioR :: (K.CContent c) => Rop.RopGet0 ([K.TermName], K.Portion) c
getPotionRatioR med = getPotionRatio med "-order" "-part" "-of"

-- | Get ratio-type portion parameter.
getPotionRatio :: (K.CContent c) => Rop.RopGet3 ([K.TermName], K.Portion) c
getPotionRatio med nOrderer nPart nOf =
    do order <- Rop.getTerms med nOrderer
       part  <- getInt med nPart
       per   <- getInt med nOf
       Right (order, K.def { K.portionParts = [part - 1]
                           , K.portionPer   = Just per })

{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Rop.Base.Get.Rel
  ( -- * Relmap
    getRelmap, getOptRelmap,

    -- * Term
    getTerm, getTerm2, getMaybeTerm,
    getTerms, getTermsCo,
    getTermPairs, getTermsColon,
    getTermTrees,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base.Get.Get  as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet med = do
--   >   m <- getRelmap med "-relmap"
--   >   Right $ relmapMeet med m
--
getRelmap :: Rop.RopGet (C.Relmap c) c
getRelmap med name =
    case lookup name $ C.medSubmap med of
      Nothing -> Msg.reqRelmap 1
      Just m  -> Right m

-- | Get optional relmap.
getOptRelmap :: C.Relmap c -> Rop.RopGet (C.Relmap c) c
getOptRelmap rmap0 med = right rmap0 . getRelmap med

-- | Replace 'Left' value to 'Right' value.
right :: b -> K.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x


-- ----------------------  Term

-- | Get a term name from named attribute.
getTerm :: Rop.RopGet K.TermName c
getTerm = Rop.getFromTree get where
    get [x] = K.treeFlatName x
    get _   = Msg.unexpAttr "Require one term"

-- | Get two term names.
getTerm2 :: Rop.RopGet (K.TermName, K.TermName) c
getTerm2 med n =
    do terms <- getTerms med n
       case terms of
         [x,y] -> Right (x, y)
         _     -> Msg.unexpAttr "Require two term"

-- | Get optional term name.
getMaybeTerm :: Rop.RopGet (Maybe K.TermName) c
getMaybeTerm = Rop.getMaybe getTerm

-- | Get list of term names from named attribute.
getTerms :: Rop.RopGet [K.TermName] c
getTerms = Rop.getFromTree K.treesFlatNames

-- | Get term names and complement sign (@~@) .
getTermsCo :: Rop.RopGet (Bool, [K.TermName]) c
getTermsCo = Rop.getFromTree K.treesFlatNamesCo

-- | Get list of term-name pairs from named attribute.
getTermPairs :: Rop.RopGet [K.TermName2] c
getTermPairs = Rop.getFromTree K.treesFlatNamePairs

-- | Get term names groups delimited by colons.
getTermsColon :: Rop.RopGet [[K.TermName]] c
getTermsColon = Rop.getFromTree K.treesNamesByColon

-- | Get list of tree terms.
getTermTrees :: Rop.RopGet [K.Term K.Tree] c
getTermTrees = Rop.getFromTree K.treesTerms1


{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Rop.Base.Rel
  ( -- * Relmap
    getRelmap, getOptRelmap,

    -- * Term
    getTerm, getTerm2, getTermOpt,
    getTerms, getTermsCo,
    getTermPairs, getTermsColon,
    getTermTrees,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base.Get      as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet med = do
--   >   m <- getRelmap med "-relmap"
--   >   Right $ relmapMeet med m
--
getRelmap :: Rop.RopGet c (C.Relmap c)
getRelmap med name =
    case lookup name $ C.medSubmap med of
      Nothing -> Msg.reqRelmap 1
      Just m  -> Right m

-- | Get optional relmap.
getOptRelmap :: C.Relmap c -> Rop.RopGet c (C.Relmap c)
getOptRelmap rmap0 med = right rmap0 . getRelmap med

-- | Replace 'Left' value to 'Right' value.
right :: b -> K.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x


-- ----------------------  Term

-- | Get a term name from named attribute.
getTerm :: Rop.RopGet c K.TermName
getTerm = Rop.getFromTree get where
    get [x] = K.treeFlatName x
    get _   = Msg.unexpAttr "Require one term"

-- | Get two term names.
getTerm2 :: Rop.RopGet c (K.TermName, K.TermName)
getTerm2 med n =
    do terms <- getTerms med n
       case terms of
         [x,y] -> Right (x, y)
         _     -> Msg.unexpAttr "Require two term"

-- | Get optional term name.
getTermOpt :: Rop.RopGet c (Maybe K.TermName)
getTermOpt = Rop.getMaybe getTerm

-- | Get list of term names from named attribute.
getTerms :: Rop.RopGet c [K.TermName]
getTerms = Rop.getFromTree K.treesFlatNames

-- | Get term names and complement sign (@~@) .
getTermsCo :: Rop.RopGet c (Bool, [K.TermName])
getTermsCo = Rop.getFromTree K.treesFlatNamesCo

-- | Get list of term-name pairs from named attribute.
getTermPairs :: Rop.RopGet c [K.TermName2]
getTermPairs = Rop.getFromTree K.treesFlatNamePairs

-- | Get term names groups delimited by colons.
getTermsColon :: Rop.RopGet c [[K.TermName]]
getTermsColon = Rop.getFromTree K.treesNamesByColon

-- | Get list of tree terms.
getTermTrees :: Rop.RopGet c [K.Term K.Tree]
getTermTrees = Rop.getFromTree K.treesTerms1


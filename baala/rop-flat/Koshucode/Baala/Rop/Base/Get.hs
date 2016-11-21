{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Rop.Base.Get
  ( -- * Datatype
    RopGet,
  
    -- * Basic
    getTag, getTags,
    getOption, getMaybe,
    getSwitch, getWord,
  
    -- * Token tree
    getTree, getTrees,
    getWordTrees,
    getTreesByColon,
  
    -- * Relmap
    getRelmap, getOptRelmap,
  
    -- * Term
    getTerm, getTerm2, getTermOpt,
    getTerms, getTermsCo,
    getTermPairs, getTermsColon,
    getTermTrees,
  ) where

import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base.Message as Msg


-- ----------------------  Datatype

-- | Type for getting something from relmap intermidiate data.
type RopGet c a
    = C.Intmed c    -- ^ Use of relmap operator
    -> String       -- ^ Name of keyword, e.g., @\"-term\"@
    -> B.Ab a       -- ^ Attribute of relmap

lookupTree :: String -> C.Intmed c -> Maybe [S.TTree]
lookupTree = lookupAttr S.AttrNormal

lookupAttr :: (String -> S.AttrName) -> String -> C.Intmed c -> Maybe [S.TTree]
lookupAttr c ('-' : name) = S.paraLookupSingle (c name) . getPara
lookupAttr _ _ = B.bug "lookupAttr"

getAbortable :: ([S.TTree] -> B.Ab b) -> RopGet c b
getAbortable f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees

getAbortableOption :: b -> ([S.TTree] -> B.Ab b) -> RopGet c b
getAbortableOption y f med name =
    do m <- getMaybe getTrees med name
       case m of
         Nothing    -> Right y
         Just trees -> Msg.abAttrTrees trees $ f trees


-- ----------------------  Basic

-- | Get relmap parameter.
getPara :: C.Intmed c -> S.AttrPara
getPara = C.lexAttr . C.medLexmap

-- | Test usage tag.
getTag :: C.Intmed c -> String -> Bool
getTag med tag = tag `elem` getTags med

-- | Get usage tags.
getTags :: C.Intmed c -> [String]
getTags = S.paraTags . getPara

-- | Get optional parameter with default value.
getOption :: a -> RopGet c a -> RopGet c a
getOption y get med name =
    case lookupTree name med of
      Nothing -> Right y
      Just _  -> get med name

-- | Get parameter whenever given or not.
getMaybe :: RopGet c a -> RopGet c (Maybe a)
getMaybe get med name =
    case lookupTree name med of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get med name

-- | Get @True@ when attribute is given, @False@ otherwise.
getSwitch :: C.Intmed c -> String -> B.Ab Bool
getSwitch med name = getAbortableOption False get med name where
    get [] = Right True
    get _  = Msg.unexpAttr $ "Just type only " ++ name

-- | Get word from named attribute.
--
--   > consXxx :: RopCons c
--   > consXxx med = do
--   >   sign <- getWord med "-sign"
--   >   ...
getWord :: RopGet c String
getWord = getAbortable get where
    get [S.TextLeaf _ _ s] = Right s
    get _ = Msg.unexpAttr "Require one word"


-- ----------------------  Tree

-- | Get trees as single tree.
getTree :: RopGet c S.TTree
getTree med name =
    do trees <- getTrees med name
       Right $ S.ttreeGroup trees

-- | Get trees from parameter.
getTrees :: RopGet c [S.TTree]
getTrees med name =
    case lookupTree name med of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name

-- | Get word-and-tree list.
getWordTrees :: RopGet c [B.Named S.TTree]
getWordTrees med name =
    case lookupTree name med of
      Just trees -> wordTrees trees
      Nothing    -> Msg.noAttr name

wordTrees :: [S.TTree] -> B.Ab [B.Named S.TTree]
wordTrees []  = Right []
wordTrees [_] = Msg.unexpAttr "Require word and tree"
wordTrees (w : tree : xs) =
    do w'  <- word w
       xs' <- wordTrees xs
       Right $ (w', tree) : xs'

word :: S.TTree -> B.Ab String
word (S.TextLeaf _ _ w) = Right w
word _ = Msg.unexpAttr "Require one word"

-- | Get trees delimited by colon.
getTreesByColon :: RopGet c [[S.TTree]]
getTreesByColon med name =
    do trees <- getTrees med name
       Right $ B.omit null $ S.divideTreesByColon trees


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet med = do
--   >   m <- getRelmap med "-relmap"
--   >   Right $ relmapMeet med m
--
getRelmap :: RopGet c (C.Relmap c)
getRelmap med name =
    case lookup name $ C.medSubmap med of
      Nothing -> Msg.reqRelmap 1
      Just m  -> Right m

-- | Get optional relmap.
getOptRelmap :: C.Relmap c -> RopGet c (C.Relmap c)
getOptRelmap rmap0 med = right rmap0 . getRelmap med

-- | Replace 'Left' value to 'Right' value.
right :: b -> O.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x


-- ----------------------  Term

-- | Get a term name from named attribute.
getTerm :: RopGet c S.TermName
getTerm = getAbortable get where
    get [x] = D.treeFlatName x
    get _   = Msg.unexpAttr "Require one term"

-- | Get two term names.
getTerm2 :: RopGet c (S.TermName, S.TermName)
getTerm2 med n =
    do terms <- getTerms med n
       case terms of
         [x,y] -> Right (x, y)
         _     -> Msg.unexpAttr "Require two term"

-- | Get optional term name.
getTermOpt :: RopGet c (Maybe S.TermName)
getTermOpt = getMaybe getTerm

-- | Get list of term names from named attribute.
getTerms :: RopGet c [S.TermName]
getTerms = getAbortable D.treesFlatNames

-- | Get term names and complement sign (@~@) .
getTermsCo :: RopGet c (Bool, [S.TermName])
getTermsCo = getAbortable D.treesFlatNamesCo

-- | Get list of term-name pairs from named attribute.
getTermPairs :: RopGet c [S.TermName2]
getTermPairs = getAbortable D.treesFlatNamePairs

-- | Get term names groups delimited by colons.
getTermsColon :: RopGet c [[S.TermName]]
getTermsColon = getAbortable D.treesNamesByColon

-- | Get list of tree terms.
getTermTrees :: RopGet c [S.Term S.TTree]
getTermTrees = getAbortable D.treesTerms1

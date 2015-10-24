{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Rop.Base.Get
  ( -- * Datatype
    RopGet,
  
    -- * Basic
    getOption, getMaybe,
    getSwitch, getWord,
  
    -- * Tree
    getTree, getTrees,
    getWordTrees,
    getTreesByColon,
  
    -- * Relmap
    getRelmap, getRelmaps, getOptRelmap,
  
    -- * Term
    getTerm, getTerm2, getTermOpt,
    getTerms, getTermsCo,
    getTermPairs, getTermsColon,
    getTermTrees,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base.Term    as Op
import qualified Koshucode.Baala.Rop.Flat.Message      as Msg


-- ----------------------  Datatype

type RopGet c a
    = C.Intmed c    -- ^ Use of relmap operator
    -> String       -- ^ Name of keyword, e.g., @\"-term\"@
    -> B.Ab a       -- ^ Attribute of relmap

lookupTree, lookupRelmap :: String -> C.Intmed c -> Maybe [D.TTree]
lookupTree    = lookupAttr C.AttrNormal
lookupRelmap  = lookupAttr C.AttrRelmapNormal `B.mappend` lookupAttr C.AttrRelmapLocal

lookupAttr :: (String -> C.AttrName) -> String -> C.Intmed c -> Maybe [D.TTree]
lookupAttr c ('-' : name) = D.paraLookupSingle (c name) . C.lexAttr . C.medLexmap
lookupAttr _ _ = B.bug "lookupAttr"

getAbortable :: ([D.TTree] -> B.Ab b) -> RopGet c b
getAbortable f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees

getAbortableOption :: b -> ([D.TTree] -> B.Ab b) -> RopGet c b
getAbortableOption y f med name =
    do m <- getMaybe getTrees med name
       case m of
         Nothing    -> Right y
         Just trees -> Msg.abAttrTrees trees $ f trees


-- ----------------------  Basic

getOption :: a -> RopGet c a -> RopGet c a
getOption y get med name =
    case lookupTree name med of
      Nothing -> Right y
      Just _  -> get med name

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
    get [D.TextLeaf _ _ s] = Right s
    get _ = Msg.unexpAttr "Require one word"


-- ----------------------  Tree

getTrees :: RopGet c [D.TTree]
getTrees med name =
    case lookupTree name med of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name

getTree :: RopGet c D.TTree
getTree med name =
    do trees <- getTrees med name
       Right $ D.ttreeGroup trees

getWordTrees :: RopGet c [B.Named D.TTree]
getWordTrees med name =
    case lookupTree name med of
      Just trees -> wordTrees trees
      Nothing    -> Msg.noAttr name

wordTrees :: [D.TTree] -> B.Ab [B.Named D.TTree]
wordTrees []  = Right []
wordTrees [_] = Msg.unexpAttr "Require word and tree"
wordTrees (w : tree : xs) =
    do w'  <- word w
       xs' <- wordTrees xs
       Right $ (w', tree) : xs'

word :: D.TTree -> B.Ab String
word (D.TextLeaf _ _ w) = Right w
word _ = Msg.unexpAttr "Require one word"

getTreesByColon :: RopGet c [[D.TTree]]
getTreesByColon med name =
    do trees <- getTrees med name
       Right $ B.omit null $ D.divideTreesByColon trees


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet med = do
--   >   m <- getRelmap med
--   >   Right $ relmapMeet med m
getRelmap :: C.Intmed c -> String -> B.Ab (C.Relmap c)
getRelmap med name =
    do ms    <- getRelmaps med
       trees <- getRelmapRaw med name
       Msg.abAttrTrees trees $ case ms of
         [m] -> Right m
         _   -> Msg.unexpAttr "Require one relmap"

getRelmapRaw :: RopGet c [D.TTree]
getRelmapRaw med name =
    case lookupRelmap name med of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name


-- | Get relmaps from operator use.
getRelmaps :: C.Intmed c -> B.Ab [C.Relmap c]
getRelmaps = Right . C.medSubmap

getOptRelmap :: C.Relmap c -> C.Intmed c -> String -> B.Ab (C.Relmap c)
getOptRelmap rmap0 med = B.right rmap0 . getRelmap med


-- ----------------------  Term

-- | Get a term name from named attribute.
getTerm :: RopGet c D.TermName
getTerm = getAbortable get where
    get [x] = Op.termName x
    get _   = Msg.unexpAttr "Require one term"

getTerm2 :: RopGet c (D.TermName, D.TermName)
getTerm2 med n =
    do terms <- getTerms med n
       case terms of
         [x,y] -> Right (x, y)
         _     -> Msg.unexpAttr "Require two term"

getTermOpt :: RopGet c (Maybe D.TermName)
getTermOpt = getMaybe getTerm

-- | Get list of term names from named attribute.
getTerms :: RopGet c [D.TermName]
getTerms = getAbortable Op.termNames

-- | Get term names and complement sign (@~@) .
getTermsCo :: RopGet c (Bool, [D.TermName])
getTermsCo = getAbortable Op.termNamesCo

-- | Get list of term-name pairs from named attribute.
getTermPairs :: RopGet c [D.TermName2]
getTermPairs = getAbortable Op.termNamePairs

getTermsColon :: RopGet c [[D.TermName]]
getTermsColon = getAbortable Op.termNamesColon

getTermTrees :: RopGet c [B.Named D.TTree]
getTermTrees = getAbortable D.treesToTerms1

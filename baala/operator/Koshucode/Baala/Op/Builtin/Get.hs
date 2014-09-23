{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Op.Builtin.Get
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
  getTerm, getTerms,
  getTermsCo,
  getTermPairs,
  getWithTerms,
  getTermTrees,
) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Builtin.Term as Op
import qualified Koshucode.Baala.Op.Message      as Msg


-- ----------------------  Datatype

type RopGet c a
    = C.RopUse c    -- ^ Use of relmap operator
    -> String       -- ^ Name of keyword, e.g., @\"-term\"@
    -> B.Ab a       -- ^ Attribute of relmap

lookupTree, lookupRelmap :: String -> C.RopUse c -> Maybe [B.TokenTree]
lookupTree   = lookupAttr C.AttrTree
lookupRelmap = lookupAttr C.AttrRelmap

lookupAttr :: (String -> C.AttrName) -> String -> C.RopUse c -> Maybe [B.TokenTree]
lookupAttr c name = lookup (c name) . C.lexAttr . C.ropLexmap

getAbortable :: ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortable f u name =
    do trees <- getTrees u name
       Msg.abAttrTrees trees $ f trees

getAbortableOption :: b -> ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortableOption y f u name =
    do m <- getMaybe getTrees u name
       case m of
         Nothing    -> Right y
         Just trees -> Msg.abAttrTrees trees $ f trees


-- ----------------------  Basic

getOption :: a -> RopGet c a -> RopGet c a
getOption y get u name =
    case lookupTree name u of
      Nothing -> Right y
      Just _  -> get u name

getMaybe :: RopGet c a -> RopGet c (Maybe a)
getMaybe get u name =
    case lookupTree name u of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get u name

-- | Get @True@ when attribute is given, @False@ otherwise.
getSwitch :: C.RopUse c -> String -> B.Ab Bool
getSwitch u name = getAbortableOption False get u name where
    get [] = Right True
    get _  = Msg.unexpAttr $ "Just type only " ++ name

-- | Get word from named attribute.
--
--   > consXxx :: RopCons c
--   > consXxx u = do
--   >   sign <- getWord u "-sign"
--   >   ...
getWord :: RopGet c String
getWord = getAbortable get where
    get [B.TreeL (B.TText _ _ s)] = Right s
    get _ = Msg.unexpAttr "Require one word"


-- ----------------------  Tree

getTrees :: RopGet c [B.TokenTree]
getTrees u name =
    case lookupTree name u of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr

getTree :: RopGet c B.TokenTree
getTree u name =
    do trees <- getTrees u name
       Right $ B.wrapTrees trees

getWordTrees :: RopGet c [B.Named B.TokenTree]
getWordTrees u name =
    case lookupTree name u of
      Just trees -> wordTrees trees
      Nothing    -> Msg.noAttr

wordTrees :: [B.TokenTree] -> B.Ab [B.Named B.TokenTree]
wordTrees []  = Right []
wordTrees [_] = Msg.unexpAttr "Require word and tree"
wordTrees (w : tree : xs) =
    do w'  <- word w
       xs' <- wordTrees xs
       Right $ (w', tree) : xs'

word :: B.TokenTree -> B.Ab String
word (B.TreeL (B.TText _ _ w)) = Right w
word _ = Msg.unexpAttr "Require one word"

getTreesByColon :: RopGet c [[B.TokenTree]]
getTreesByColon u name =
    do trees <- getTrees u name
       Right $ B.omit null $ B.divideTreesByColon trees


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet u = do
--   >   m <- getRelmap u
--   >   Right $ relmapMeet u m
getRelmap :: C.RopUse c -> String -> B.Ab (C.Relmap c)
getRelmap u name =
    do ms    <- getRelmaps u
       trees <- getRelmapRaw u name
       Msg.abAttrTrees trees $ case ms of
         [m] -> Right m
         _   -> Msg.unexpAttr "Require one relmap"

getRelmapRaw :: RopGet c [B.TokenTree]
getRelmapRaw u name =
    case lookupRelmap name u of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr


-- | Get relmaps from operator use.
getRelmaps :: C.RopUse c -> B.Ab [C.Relmap c]
getRelmaps = Right . C.ropSubrelmap

getOptRelmap :: C.Relmap c -> C.RopUse c -> String -> B.Ab (C.Relmap c)
getOptRelmap rmap0 u = B.right rmap0 . getRelmap u


-- ----------------------  Term

-- | Get a term name from named attribute.
getTerm :: RopGet c B.TermName
getTerm = getAbortable get where
    get [x] = Op.termName x
    get _   = Msg.unexpAttr "Require one term"

-- | Get list of term names from named attribute.
getTerms :: RopGet c [B.TermName]
getTerms = getAbortable Op.termNames

-- | Get term names and complement sign (@~@) .
getTermsCo :: RopGet c (Bool, [B.TermName])
getTermsCo = getAbortable Op.termNamesCo

-- | Get list of term-name pairs from named attribute.
getTermPairs :: RopGet c [B.TermName2]
getTermPairs = getAbortable Op.termNamePairs

getWithTerms :: RopGet c [B.Terminal String]
getWithTerms = getAbortable C.withTerms

getTermTrees :: RopGet c [B.Named B.TokenTree]
getTermTrees = getAbortable C.getTermedTrees

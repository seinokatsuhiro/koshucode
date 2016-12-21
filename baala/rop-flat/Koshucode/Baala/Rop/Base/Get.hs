{-# OPTIONS_GHC -Wall #-}

-- | Attribute getters: Extract attribute from use of relmap.

module Koshucode.Baala.Rop.Base.Get
  ( -- * Datatype
    RopGet,
    getFromTree,

    -- * Tree
    getTree, getTrees,
    getWordTrees,
    getTreesByColon,

    -- * Basic
    getTag, getTags,
    getOption, getMaybe,
    getSwitch, getWord,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Syntax.Pattern   as P
import qualified Koshucode.Baala.Rop.Base.Message as Msg


-- ----------------------  Datatype

-- | Type for getting something from relmap intermidiate data.
type RopGet c a
    = C.Intmed c    -- ^ Use of relmap operator
    -> String       -- ^ Name of keyword, e.g., @\"-term\"@
    -> B.Ab a       -- ^ Attribute of relmap

-- | Lookup parameter tree.
lookupTree :: String -> C.Intmed c -> Maybe [S.Tree]
lookupTree = lookupAttr S.AttrNormal

lookupAttr :: (String -> S.AttrName) -> String -> C.Intmed c -> Maybe [S.Tree]
lookupAttr c ('-' : name) = S.paraLookupSingle (c name) . getPara
lookupAttr _ _ = B.bug "lookupAttr"

-- | Get from trees.
getFromTree :: ([S.Tree] -> B.Ab b) -> RopGet c b
getFromTree f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees

getFromTreeOption :: b -> ([S.Tree] -> B.Ab b) -> RopGet c b
getFromTreeOption y f med name =
    do m <- getMaybe getTrees med name
       case m of
         Nothing    -> Right y
         Just trees -> Msg.abAttrTrees trees $ f trees


-- ----------------------  Tree

-- | Get trees as single tree.
getTree :: RopGet c S.Tree
getTree med name =
    do trees <- getTrees med name
       Right $ S.ttreeGroup trees

-- | Get trees from parameter.
getTrees :: RopGet c [S.Tree]
getTrees med name =
    case lookupTree name med of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name

-- | Get word-and-tree list.
getWordTrees :: RopGet c [B.Named S.Tree]
getWordTrees med name =
    case lookupTree name med of
      Just trees -> wordTrees trees
      Nothing    -> Msg.noAttr name

wordTrees :: [S.Tree] -> B.Ab [B.Named S.Tree]
wordTrees []  = Right []
wordTrees [_] = Msg.unexpAttr "Require word and tree"
wordTrees (w : tree : xs) =
    do w'  <- word w
       xs' <- wordTrees xs
       Right $ (w', tree) : xs'

word :: S.Tree -> B.Ab String
word (P.LText _ w) = Right w
word _ = Msg.unexpAttr "Require one word"

-- | Get trees delimited by colon.
getTreesByColon :: RopGet c [[S.Tree]]
getTreesByColon med name =
    do trees <- getTrees med name
       Right $ B.omit null $ S.divideTreesByColon trees


-- ----------------------  Basic

-- | Get relmap parameter.
getPara :: C.Intmed c -> S.AttrPara
getPara = C.lexAttr . C.medLexmap

-- | Test usage tag.
getTag :: C.Intmed c -> S.ParaTag -> Bool
getTag med tag = tag `elem` getTags med

-- | Get usage tags.
getTags :: C.Intmed c -> [S.ParaTag]
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
getSwitch med name = getFromTreeOption False get med name where
    get [] = Right True
    get _  = Msg.unexpAttr $ "Just type only " ++ name

-- | Get word from named attribute.
--
--   > consXxx :: RopCons c
--   > consXxx med = do
--   >   sign <- getWord med "-sign"
--   >   ...
getWord :: RopGet c String
getWord = getFromTree get where
    get [P.LText _ s] = Right s
    get _ = Msg.unexpAttr "Require one word"

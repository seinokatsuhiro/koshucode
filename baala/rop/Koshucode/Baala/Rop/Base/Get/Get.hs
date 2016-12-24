{-# OPTIONS_GHC -Wall #-}

-- | Get parameters of relmap operator.

module Koshucode.Baala.Rop.Base.Get.Get
  ( -- * Datatype
    RopGet,
    getFromTree,

    -- * Tree
    getTree, getTrees,
    getTreesByColon,

    -- * Basic
    getTag, getTags,
    getSwitch, getMaybe, getOpt,
    getWord,
  ) where

import qualified Koshucode.Baala.DataPlus         as K
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Syntax.Pattern   as P
import qualified Koshucode.Baala.Rop.Base.Message as Msg


-- ----------------------  Datatype

-- | Type for getting something from relmap intermidiate data.
type RopGet a c
    = C.Intmed c    -- ^ Intermediate relmap
    -> String       -- ^ Parameter name, e.g., @\"-term\"@
    -> K.Ab a       -- ^ Parameter value

-- | Lookup parameter tree.
lookupTree :: String -> C.Intmed c -> Maybe [K.Tree]
lookupTree = lookupAttr K.AttrNormal

lookupAttr :: (String -> K.AttrName) -> String -> C.Intmed c -> Maybe [K.Tree]
lookupAttr c ('-' : name) = K.paraLookupSingle (c name) . getPara
lookupAttr _ _ = K.bug "lookupAttr"

-- | Get from trees.
getFromTree :: ([K.Tree] -> K.Ab b) -> RopGet b c
getFromTree f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees

getFromTreeOption :: b -> ([K.Tree] -> K.Ab b) -> RopGet b c
getFromTreeOption y f med name =
    do m <- getMaybe getTrees med name
       case m of
         Nothing    -> Right y
         Just trees -> Msg.abAttrTrees trees $ f trees


-- ----------------------  Tree

-- | Get trees as single tree.
getTree :: RopGet K.Tree c
getTree med name =
    do trees <- getTrees med name
       Right $ K.ttreeGroup trees

-- | Get trees from parameter.
getTrees :: RopGet [K.Tree] c
getTrees med name =
    case lookupTree name med of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name

-- | Get trees delimited by colon.
getTreesByColon :: RopGet [[K.Tree]] c
getTreesByColon med name =
    do trees <- getTrees med name
       Right $ K.omit null $ K.divideTreesByColon trees


-- ----------------------  Basic

-- | Get relmap parameter.
getPara :: C.Intmed c -> K.AttrPara
getPara = C.lexAttr . C.medLexmap

-- | Test usage tag.
getTag :: C.Intmed c -> K.ParaTag -> Bool
getTag med tag = tag `elem` getTags med

-- | Get usage tags.
getTags :: C.Intmed c -> [K.ParaTag]
getTags = K.paraTags . getPara

-- | Get @True@ when parameter is given, @False@ otherwise.
getSwitch :: RopGet Bool c
getSwitch med name = getFromTreeOption False get med name where
    get [] = Right True
    get _  = Msg.unexpAttr $ "Just type only " ++ name

-- | Get parameter whenever given or not.
getMaybe :: RopGet a c -> RopGet (Maybe a) c
getMaybe get med name =
    case lookupTree name med of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get med name

-- | Get optional parameter with default value.
getOpt
    :: a            -- ^ Default value
    -> RopGet a c   -- ^ Non-optional getter
    -> RopGet a c   -- ^ Optional getter
getOpt y get med name =
    case lookupTree name med of
      Nothing -> Right y
      Just _  -> get med name

-- | Get word.
--
--   > consXxx :: RopCons c
--   > consXxx med = do
--   >   sign <- getWord med "-sign"
--   >   ...
--
getWord :: RopGet String c
getWord = getFromTree get where
    get [P.LText _ s] = Right s
    get _ = Msg.unexpAttr "Require one word"

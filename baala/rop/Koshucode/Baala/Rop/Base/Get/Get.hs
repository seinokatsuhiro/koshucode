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

-- | Lookup relmap parameter.
(?) :: C.Intmed c -> String -> Maybe [K.Tree]
(?) med ('-' : name) = K.AttrNormal name `K.paraLookupSingle` getPara med
(?) _ _ = K.bug "rop"

-- | Get relmap parameter.
getFromTree :: ([K.Tree] -> K.Ab b) -> RopGet b c
getFromTree f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees


-- ----------------------  Tree

-- | Get trees as single tree.
getTree :: RopGet K.Tree c
getTree med name =
    do trees <- getTrees med name
       Right $ K.ttreeGroup trees

-- | Get trees from parameter.
getTrees :: RopGet [K.Tree] c
getTrees med name =
    case med ? name of
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
getSwitch med name =
    case med ? name of
      Nothing  -> Right False
      Just []  -> Right True
      Just _   -> Msg.unexpAttr $ "Just type only " ++ name

-- | Get parameter whenever given or not.
getMaybe :: RopGet a c -> RopGet (Maybe a) c
getMaybe get med name =
    case med ? name of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get med name

-- | Get optional parameter with default value.
getOpt :: a            -- ^ Default value
       -> RopGet a c   -- ^ Non-optional getter
       -> RopGet a c   -- ^ Optional getter
getOpt y get med name =
    case med ? name of
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

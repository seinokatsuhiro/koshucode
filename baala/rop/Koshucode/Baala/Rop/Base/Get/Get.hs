{-# OPTIONS_GHC -Wall #-}

-- | Get parameters of relmap operator.

module Koshucode.Baala.Rop.Base.Get.Get
  ( -- * Type
    RopGet,

    -- * Tree
    getTrees,
    getWith, getWithAb,
    getTree, 

    -- * Basic
    getTag, getTags,
    getSwitch, getMaybe, getOpt,
    getWord,
  ) where

import qualified Koshucode.Baala.DataPlus         as K
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Syntax.Pattern   as P
import qualified Koshucode.Baala.Rop.Base.Message as Msg


-- ----------------------  Type

-- | Type for getting something from relmap intermidiate data.
type RopGet a c
    = C.Intmed c    -- ^ Intermediate relmap
    -> String       -- ^ Parameter name, e.g., @\"-term\"@
    -> K.Ab a       -- ^ Parameter value


-- ----------------------  Tree

-- | Lookup relmap parameter.
(?) :: C.Intmed c -> String -> Maybe [K.Tree]
(?) med ('-' : name) = K.AttrNormal name `K.paraLookupSingle` getPara med
(?) _ _ = K.bug "rop"

-- | Get trees from relmap parameter.
--   This is the most basic getter
--   because relmap parameters are syntactically represented as tree list.
getTrees :: RopGet [K.Tree] c
getTrees med name =
    case med ? name of
      Just trees -> Right trees
      Nothing    -> Msg.noAttr name

-- | Get relmap parameter and convert using function.
getWith :: ([K.Tree] -> b) -> RopGet b c
getWith f med name = Right . f =<< getTrees med name

-- | Get relmap parameter and convert using abortable function.
getWithAb :: ([K.Tree] -> K.Ab b) -> RopGet b c
getWithAb f med name =
    do trees <- getTrees med name
       Msg.abAttrTrees trees $ f trees

-- | Get tree from relmap parameter.
getTree :: RopGet K.Tree c
getTree = getWith K.ttreeGroup


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
getWord = getWithAb word where
    word [P.LText _ s] = Right s
    word _ = Msg.unexpAttr "Require one word"

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute mapping, or attribute editor.

module Koshucode.Baala.Core.Lexmap.Attrmap
  ( Attrmap, AttrmapBody (..),
    consAttrmap, runAttrmap,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Msg


-- ----------------------  Data type

-- | Attribute mapping with source code information.
type Attrmap = B.Sourced AttrmapBody

-- | Operators for attribute mappings.
data AttrmapBody
    = AttrmapId                             -- ^ Identity mapping
    | AttrmapAdd     Bool String [B.TTree]  -- ^ Add attribute
    | AttrmapRename  (String, String)       -- ^ Rename attribute keyword
    | AttrmapFill    [Maybe B.TTree]        -- ^ Fill positional attributes
    | AttrmapAppend  [Attrmap]              -- ^ Append mappings
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Cons and run

-- | Construct attribute mapping.
consAttrmap :: [B.TTree] -> B.Ab Attrmap
consAttrmap = loop where
    notKeyword ('-' : _)  = False
    notKeyword _          = True

    fill (B.TextLeafRaw _ "*" : xs)  = Nothing : fill xs
    fill (x : xs)                    = Just x  : fill xs
    fill []                          = []

    right :: [B.TTree] -> AttrmapBody -> B.Ab Attrmap
    right trees = Right . B.Sourced (concatMap B.codePtList $ B.untrees trees)

    loop trees =
        Msg.abAttrTrees trees $ case B.divideTreesByBar trees of
          [ B.TextLeafRaw _ op : xs ]
            | op == "id"        -> right trees $ AttrmapId
            | op == "fill"      -> right trees $ AttrmapFill $ fill xs

          [ B.TextLeafRaw _ op : B.TextLeafRaw _ k : xs ]
            | notKeyword k      -> Msg.reqAttrName k
            | op == "add"       -> right trees $ AttrmapAdd False k xs
            | op == "opt"       -> right trees $ AttrmapAdd True  k xs

          [ B.TextLeafRaw _ op : B.TextLeafRaw _ k'
                : B.TextLeafRaw _ k : _ ]
            | notKeyword k'     -> Msg.reqAttrName k'
            | notKeyword k      -> Msg.reqAttrName k
            | op == "rename"    -> right trees $ AttrmapRename (k', k)

          [[ B.TreeB B.BracketGroup _ xs ]]  ->  loop xs

          [[]]                  -> right [] AttrmapId
          [_]                   -> Msg.adlib "unknown attribute mapping"
          trees2                -> do subs <- mapM loop trees2
                                      right trees $ AttrmapAppend subs

-- | Edit relmap attributes.
runAttrmap :: Attrmap -> B.AbMap [C.AttrTree]
runAttrmap = loop where
    loop (B.Sourced toks edit) attr =
        let Just pos = lookup C.attrNameTrunk attr
        in Msg.abAttr toks $ case edit of
          AttrmapId                -> Right  attr
          AttrmapAdd opt k xs      -> add    attr opt (C.AttrNameNormal k) xs
          AttrmapRename (k', k)    -> rename attr (C.AttrNameNormal k') (C.AttrNameNormal k)
          AttrmapFill xs           -> do xs2 <- fill pos xs
                                         Right $ (C.attrNameTrunk, xs2) : attr
          AttrmapAppend rs         -> B.foldM (flip loop) attr rs

    add attr opt k xs =
        case lookup k attr of
          Just _ | opt           -> Right attr
                 | otherwise     -> Msg.extraAttr
          Nothing                -> do xs' <- C.substSlot [] attr xs
                                       Right $ (k, xs') : attr

    rename :: [C.AttrTree] -> C.AttrName -> C.AttrName -> B.Ab [C.AttrTree]
    rename attr k' k =
        case lookup k attr of
          Just _                 -> Right $ B.assocRename1 k' k attr
          Nothing                -> Msg.reqAttr $ C.attrNameText k

    fill :: [B.TTree] -> [Maybe B.TTree] -> B.Ab [B.TTree]
    fill (p : ps) (Nothing : xs)  = Right . (p:) =<< fill ps xs
    fill (p : ps) (_       : xs)  = Right . (p:) =<< fill ps xs
    fill []       (Just x  : xs)  = Right . (x:) =<< fill [] xs
    fill []       (Nothing : _ )  = Msg.reqAttr "*"
    fill ps       []              = Right $ ps


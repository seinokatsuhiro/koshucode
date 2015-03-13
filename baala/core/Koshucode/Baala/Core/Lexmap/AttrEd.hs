{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute editor.

module Koshucode.Baala.Core.Lexmap.AttrEd
  ( AttrEd, AttrEdBody (..),
    consAttrEd, runAttrEd,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.AttrPos    as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Msg


-- ----------------------  Data type

-- | Attribute editor with source code information.
type AttrEd = B.Sourced AttrEdBody

-- | Operators for attribute editors.
data AttrEdBody
    = AttrEdId                             -- ^ Identity editor
    | AttrEdAdd     Bool String [B.TTree]  -- ^ Add attribute
    | AttrEdRename  (String, String)       -- ^ Rename attribute keyword
    | AttrEdFill    [Maybe B.TTree]        -- ^ Fill positional attributes
    | AttrEdTerm    String [B.TTree]       -- ^ Make term name
    | AttrEdNest    String [B.TTree]       -- ^ Nested relation reference
    | AttrEdAppend  [AttrEd]               -- ^ Append editors
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Cons and run

-- | Construct attribute editor.
consAttrEd :: [B.TTree] -> B.Ab AttrEd
consAttrEd = loop where
    notKeyword ('-' : _)  = False
    notKeyword _          = True

    fill (B.TextLeafRaw _ "*" : xs)  = Nothing : fill xs
    fill (x : xs)                    = Just x  : fill xs
    fill []                          = []

    right :: [B.TTree] -> AttrEdBody -> B.Ab AttrEd
    right trees = Right . B.Sourced (concatMap B.codePtList $ B.untrees trees)

    loop trees =
        Msg.abAttrTrees trees $ case B.divideTreesByBar trees of
          [ B.TextLeafRaw _ op : xs ]
            | op == "id"        -> right trees $ AttrEdId
            | op == "fill"      -> right trees $ AttrEdFill $ fill xs

          [ B.TextLeafRaw _ op : B.TextLeafRaw _ ('-' : k) : xs ]
            | op == "add"       -> right trees $ AttrEdAdd False k xs
            | op == "opt"       -> right trees $ AttrEdAdd True  k xs
            | op == "term"      -> right trees $ AttrEdTerm k xs
            | op == "nest"      -> right trees $ AttrEdNest k xs

          [ B.TextLeafRaw _ op : B.TextLeafRaw _ k'
                : B.TextLeafRaw _ k : _ ]
            | notKeyword k'     -> Msg.reqAttrName k'
            | notKeyword k      -> Msg.reqAttrName k
            | op == "rename"    -> right trees $ AttrEdRename (k', k)

          [[ B.TreeB B.BracketGroup _ xs ]]  ->  loop xs

          [[]]                  -> right [] AttrEdId
          [_]                   -> Msg.adlib "unknown attribute editor"
          trees2                -> do subs <- mapM loop trees2
                                      right trees $ AttrEdAppend subs

-- | Edit relmap attributes.
runAttrEd :: AttrEd -> B.AbMap [C.AttrTree]
runAttrEd (B.Sourced toks edit) attr = run where
    run = Msg.abAttr toks $ case edit of
            AttrEdId                -> Right attr
            AttrEdAdd opt k xs      -> add opt (C.AttrNormal k) xs
            AttrEdRename (k', k)    -> rename (C.AttrNormal k') (C.AttrNormal k)
            AttrEdFill xs           -> do xs2 <- fill pos xs
                                          Right $ (C.attrNameTrunk, xs2) : attr
            AttrEdTerm  k xs        -> term (C.AttrNormal k) xs
            AttrEdNest  k xs        -> nest (C.AttrNormal k) xs
            AttrEdAppend rs         -> B.foldM (flip runAttrEd) attr rs

    Just pos = lookup C.attrNameTrunk attr

    add opt k xs = case lookup k attr of
                     Just _ | opt           -> Right attr
                            | otherwise     -> Msg.extraAttr
                     Nothing                -> do xs' <- C.substSlot [] attr xs
                                                  Right $ (k, xs') : attr

    term k xs = do xs' <- C.substSlot [] attr xs
                   n   <- termPath xs'
                   Right $ (k, n) : attr

    nest k xs = do xs' <- C.substSlot [] attr xs
                   n   <- nestName xs'
                   Right $ (k, n) : attr

    rename :: C.AttrName -> C.AttrName -> B.Ab [C.AttrTree]
    rename k' k = case lookup k attr of
          Just _   -> Right $ B.assocRename1 k' k attr
          Nothing  -> Msg.reqAttr $ C.attrNameText k

    fill :: [B.TTree] -> [Maybe B.TTree] -> B.Ab [B.TTree]
    fill (p : ps) (Nothing : xs)   = Right . (p:) =<< fill ps xs
    fill (p : ps) (_       : xs)   = Right . (p:) =<< fill ps xs
    fill []       (Just x  : xs)   = Right . (x:) =<< fill [] xs
    fill []       (Nothing : _ )   = Msg.reqAttr "*"
    fill ps       []               = Right $ ps

termPath :: B.AbMap [B.TTree]
termPath = loop [] where
    loop path [] = Right [B.TreeL $ B.TTerm B.codePtZero B.TermTypePath $ reverse path]
    loop path (B.TermLeafPath _ ps : xs)  = loop (reverse ps ++ path) xs
    loop _ _                              = Msg.adlib "require term name"

nestName :: B.AbMap [B.TTree]
nestName [B.TermLeafPath _ [p]] = Right [B.TreeL $ B.TLocal B.codePtZero (B.LocalNest p) (-1) []]
nestName _ = Msg.adlib "require term name"


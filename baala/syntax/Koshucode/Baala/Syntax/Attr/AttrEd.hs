{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Attribute editor.

module Koshucode.Baala.Syntax.Attr.AttrEd
  ( AttrEd, AttrEdBody (..),
    consAttrEd, runAttrEd,
  ) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Token           as D
import qualified Koshucode.Baala.Syntax.Attr.AttrPos    as D
import qualified Koshucode.Baala.Syntax.Attr.Slot       as D
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Attr.Message    as Msg


-- ----------------------  Data type

-- | Attribute editor with source code information.
type AttrEd = B.Sourced AttrEdBody

-- | Operators for attribute editors.
data AttrEdBody
    = AttrEdId                             -- ^ Identity editor
    | AttrEdAdd     Bool String [D.TTree]  -- ^ Add attribute
    | AttrEdRename  (String, String)       -- ^ Rename attribute keyword
    | AttrEdFill    [Maybe D.TTree]        -- ^ Fill positional attributes
    | AttrEdTerm    String [D.TTree]       -- ^ Make term name
    | AttrEdNest    String [D.TTree]       -- ^ Nested relation reference
    | AttrEdAppend  [AttrEd]               -- ^ Append editors
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Cons and run

-- | Construct attribute editor.
consAttrEd :: [D.TTree] -> B.Ab AttrEd
consAttrEd = loop where
    notKeyword ('-' : _)  = False
    notKeyword _          = True

    fill (D.TextLeafRaw _ "*" : xs)  = Nothing : fill xs
    fill (x : xs)                    = Just x  : fill xs
    fill []                          = []

    right :: [D.TTree] -> AttrEdBody -> B.Ab AttrEd
    right trees = Right . B.Sourced (concatMap B.codePtList $ B.untrees trees)

    loop trees =
        Msg.abAttrTrees trees $ case D.divideTreesByBar trees of
          [ D.TextLeafRaw _ op : xs ]
            | op == "id"        -> right trees $ AttrEdId
            | op == "fill"      -> right trees $ AttrEdFill $ fill xs

          [ D.TextLeafRaw _ op : D.TextLeafRaw _ ('-' : k) : xs ]
            | op == "add"       -> right trees $ AttrEdAdd False k xs
            | op == "opt"       -> right trees $ AttrEdAdd True  k xs
            | op == "term"      -> right trees $ AttrEdTerm k xs
            | op == "nest"      -> right trees $ AttrEdNest k xs

          [ D.TextLeafRaw _ op : D.TextLeafRaw _ k'
                : D.TextLeafRaw _ k : _ ]
            | notKeyword k'     -> Msg.reqAttrName k'
            | notKeyword k      -> Msg.reqAttrName k
            | op == "rename"    -> right trees $ AttrEdRename (k', k)

          [[ B.TreeB D.BracketGroup _ xs ]]  ->  loop xs

          [[]]                  -> right [] AttrEdId
          [_]                   -> Msg.adlib "unknown attribute editor"
          trees2                -> do subs <- mapM loop trees2
                                      right trees $ AttrEdAppend subs

-- | Edit relmap attributes.
runAttrEd :: AttrEd -> B.AbMap [D.AttrTree]
runAttrEd (B.Sourced toks edit) attr = run where
    run = Msg.abAttr toks $ case edit of
            AttrEdId                -> Right attr
            AttrEdAdd opt k xs      -> add opt (D.AttrNormal k) xs
            AttrEdRename (k', k)    -> rename (D.AttrNormal k') (D.AttrNormal k)
            AttrEdFill xs           -> do xs2 <- fill pos xs
                                          Right $ (D.attrNameTrunk, xs2) : attr
            AttrEdTerm  k xs        -> term (D.AttrNormal k) xs
            AttrEdNest  k xs        -> nest (D.AttrNormal k) xs
            AttrEdAppend rs         -> B.foldM (flip runAttrEd) attr rs

    Just pos = lookup D.attrNameTrunk attr

    add opt k xs = case lookup k attr of
                     Just _ | opt           -> Right attr
                            | otherwise     -> Msg.extraAttr
                     Nothing                -> do xs' <- D.substSlot [] attr xs
                                                  Right $ (k, xs') : attr

    term k xs = do xs' <- D.substSlot [] attr xs
                   n   <- termPath xs'
                   Right $ (k, n) : attr

    nest k xs = do xs' <- D.substSlot [] attr xs
                   n   <- nestName xs'
                   Right $ (k, n) : attr

    rename :: D.AttrName -> D.AttrName -> B.Ab [D.AttrTree]
    rename k' k = case lookup k attr of
          Just _   -> Right $ B.assocRename1 k' k attr
          Nothing  -> Msg.reqAttr $ D.attrNameText k

    fill :: [D.TTree] -> [Maybe D.TTree] -> B.Ab [D.TTree]
    fill (p : ps) (Nothing : xs)   = Right . (p:) =<< fill ps xs
    fill (p : ps) (_       : xs)   = Right . (p:) =<< fill ps xs
    fill []       (Just x  : xs)   = Right . (x:) =<< fill [] xs
    fill []       (Nothing : _ )   = Msg.reqAttr "*"
    fill ps       []               = Right $ ps

termPath :: B.AbMap [D.TTree]
termPath = loop [] where
    loop path [] = Right [B.TreeL $ D.TTermPath B.codePtZero $ reverse path]
    loop path (D.TermLeafName _ _ p : xs)  = loop (p : path) xs
    loop path (D.TermLeafPath _ ps  : xs)  = loop (reverse ps ++ path) xs
    loop _ _                               = Msg.adlib "require term name"

nestName :: B.AbMap [D.TTree]
nestName [D.TermLeafName _ _ p] = Right [B.TreeL $ D.TLocal B.codePtZero (D.LocalNest p) (-1) []]
nestName [D.TermLeafPath _ [p]] = Right [B.TreeL $ D.TLocal B.codePtZero (D.LocalNest p) (-1) []]
nestName _ = Msg.adlib "require term name"


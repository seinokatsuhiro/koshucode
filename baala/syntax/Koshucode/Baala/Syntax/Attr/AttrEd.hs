{-# OPTIONS_GHC -Wall #-}

-- | Attribute editor.

module Koshucode.Baala.Syntax.Attr.AttrEd
  ( AttrEd, AttrEdBody (..),
    consAttrEd, runAttrEd,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol          as S
import qualified Koshucode.Baala.Syntax.Token           as S
import qualified Koshucode.Baala.Syntax.Tree            as S
import qualified Koshucode.Baala.Syntax.Attr.AttrName   as S
import qualified Koshucode.Baala.Syntax.Attr.Slot       as S
import qualified Koshucode.Baala.Syntax.Pattern         as P
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Syntax.Attr.Message    as Msg


-- ----------------------  Data type

-- | Attribute editor with source code information.
type AttrEd = B.Codic AttrEdBody

-- | Operators for attribute editors.
data AttrEdBody
    = AttrEdId                            -- ^ __id:__ Identity editor
    | AttrEdAdd     Bool String [S.Tree]  -- ^ __add:__ Add attribute
    | AttrEdRename  (String, String)      -- ^ __rename:__ Rename attribute keyword
    | AttrEdFill    [Maybe S.Tree]        -- ^ __fill:__ Fill positional attributes
    | AttrEdNest    String [S.Tree]       -- ^ __nest:__ Nested relation reference
    | AttrEdAppend  [AttrEd]              -- ^ Append editors
      deriving (Show, Eq, Ord)


-- ----------------------  Cons and run

-- | Construct attribute editor.
consAttrEd :: [S.Tree] -> B.Ab AttrEd
consAttrEd = loop where
    notKeyword ('-' : _)  = False
    notKeyword _          = True

    fill (P.LRaw "*" : xs) = Nothing : fill xs
    fill (x : xs)          = Just x  : fill xs
    fill []                = []

    right :: [S.Tree] -> AttrEdBody -> B.Ab AttrEd
    right trees = Right . B.codic trees

    loop trees =
        Msg.abAttrTrees trees $ case S.divideTreesByBar trees of
          [P.LRaw op : xs]
            | op == "id"      -> right trees $ AttrEdId
            | op == "fill"    -> right trees $ AttrEdFill $ fill xs

          [P.LRaw op : P.LRaw ('-' : k) : xs]
            | op == "add"     -> right trees $ AttrEdAdd False k xs
            | op == "opt"     -> right trees $ AttrEdAdd True  k xs
            | op == "nest"    -> right trees $ AttrEdNest k xs

          [P.LRaw op : P.LRaw k' : P.LRaw k : _]
            | notKeyword k'   -> Msg.reqAttrName k'
            | notKeyword k    -> Msg.reqAttrName k
            | op == "rename"  -> right trees $ AttrEdRename (k', k)

          [[P.BGroup xs]]     -> loop xs

          [[]]                -> right [] AttrEdId
          [_]                 -> Msg.adlib "unknown attribute editor"
          trees2              -> do subs <- mapM loop trees2
                                    right trees $ AttrEdAppend subs

-- | Edit relmap attributes.
runAttrEd :: AttrEd -> B.AbMap [S.AttrTree]
runAttrEd (B.Codic cp edit) attr = run where
    run = Msg.abAttr cp $ case edit of
            AttrEdId                -> Right attr
            AttrEdAdd opt k xs      -> add opt (S.AttrNormal k) xs
            AttrEdRename (k', k)    -> rename (S.AttrNormal k') (S.AttrNormal k)
            AttrEdFill xs           -> do xs2 <- fill pos xs
                                          Right $ (S.attrNameTrunk, xs2) : attr
            AttrEdNest  k xs        -> nest (S.AttrNormal k) xs
            AttrEdAppend rs         -> B.foldM (flip runAttrEd) attr rs

    Just pos = lookup S.attrNameTrunk attr

    add opt k xs = case lookup k attr of
                     Just _ | opt           -> Right attr
                            | otherwise     -> Msg.extraAttr
                     Nothing                -> do xs' <- S.substSlot [] attr xs
                                                  Right $ (k, xs') : attr

    nest k xs = do xs' <- S.substSlot [] attr xs
                   n   <- nestName xs'
                   Right $ (k, n) : attr

    rename :: S.AttrName -> S.AttrName -> B.Ab [S.AttrTree]
    rename k' k = case lookup k attr of
          Just _   -> Right $ B.assocRename1 k' k attr
          Nothing  -> Msg.reqAttr $ S.attrNameText k

    fill :: [S.Tree] -> [Maybe S.Tree] -> B.Ab [S.Tree]
    fill (p : ps) (Nothing : xs)   = Right . (p:) =<< fill ps xs
    fill (p : ps) (_       : xs)   = Right . (p:) =<< fill ps xs
    fill []       (Just x  : xs)   = Right . (x:) =<< fill [] xs
    fill []       (Nothing : _ )   = Msg.reqAttr "*"
    fill ps       []               = Right $ ps

nestName :: B.AbMap [S.Tree]
nestName [P.LTerm n] = Right $ localNest $ S.toTermName n
nestName _ = Msg.adlib "require term name"

localNest :: S.TermName -> [S.Tree]
localNest n = [B.TreeL $ S.TLocal B.def (S.LocalNest n) (-1) []]

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Mapping from attributes to attributes, aka., attribute editor.

module Koshucode.Baala.Core.Lexmap.Roamap
( Roamap,
  RoamapBody (..),
  roamapCons,
  roamapRun,
) where

import qualified Data.Generics                          as G
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Lexmap.Attribute  as C
import qualified Koshucode.Baala.Core.Lexmap.Slot       as C
import qualified Koshucode.Baala.Core.Message           as Message


-- ----------------------  Data type

-- | Roamap with source code information.
type Roamap = B.Sourced RoamapBody

-- | Roamap operators.
data RoamapBody
    = RoamapId
      -- ^ Identity roamap.
    | RoamapAdd     Bool String [B.TokenTree]
      -- ^ Add attribute.
    | RoamapRename  (String, String)
      -- ^ Rename attribute keyword.
    | RoamapFill    [Maybe B.TokenTree]
      -- ^ Fill positional attributes.
    | RoamapAppend  [Roamap]
      -- ^ Append roamaps.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Cons and run

-- | Construct roamap.
roamapCons :: [B.TokenTree] -> B.Ab Roamap
roamapCons = loop where
    notKeyword ('-' : _) = False
    notKeyword _         = True

    fill (B.TreeL (B.TText _ 0 "*") : xs) = Nothing : fill xs
    fill (x : xs)                         = Just x  : fill xs
    fill []                               = []

    right :: [B.TokenTree] -> RoamapBody -> B.Ab Roamap
    right trees = Right . B.Sourced (concatMap B.codePoint $ B.untrees trees)

    loop trees =
        B.abortableTrees "attr" trees $ case B.divideTreesByBar trees of
          [ B.TreeL (B.TText _ 0 op) : xs ]
            | op == "id"        ->  right trees $ RoamapId
            | op == "fill"      ->  right trees $ RoamapFill $ fill xs

          [ B.TreeL (B.TText _ 0 op) : B.TreeL (B.TText _ 0 k) : xs ]
            | notKeyword k      ->  Message.reqAttrName k
            | op == "add"       ->  right trees $ RoamapAdd False k xs
            | op == "opt"       ->  right trees $ RoamapAdd True  k xs

          [ B.TreeL (B.TText _ 0 op) : B.TreeL (B.TText _ 0 k')
                : B.TreeL (B.TText _ 0 k) : _ ]
            | notKeyword k'     ->  Message.reqAttrName k'
            | notKeyword k      ->  Message.reqAttrName k
            | op == "rename"    ->  right trees $ RoamapRename (k', k)

          [[ B.TreeB 1 _ xs ]]  ->  loop xs

          [[]]    ->  right [] RoamapId
          [_]     ->  Message.adlib "unknown roamap"
          trees2  ->  do subs <- mapM loop trees2
                         right trees $ RoamapAppend subs

-- | Run roamap.
roamapRun :: Roamap -> B.AbMap C.Roa
roamapRun = loop where
    loop (B.Sourced toks rmap) roa =
        let Just pos = lookup "@trunk" roa
        in B.abortable "attr" toks $ case rmap of
          RoamapId              ->  Right  roa
          RoamapAdd opt k xs    ->  add    roa opt k xs
          RoamapRename (k', k)  ->  rename roa k' k
          RoamapFill xs         ->  do xs2 <- fill pos xs
                                       Right $ ("@trunk", xs2) : roa
          RoamapAppend rs       ->  B.foldM (flip loop) roa rs

    add roa opt k xs =
        case lookup k roa of
          Nothing             ->  do xs' <- C.substSlot [] roa xs
                                     Right $ (k, xs') : roa
          Just _ | opt        ->  Right roa
                 | otherwise  ->  Message.extraAttr

    rename roa k' k =
        case lookup k roa of
          Just _              ->  Right $ B.assocRename1 k' k roa
          Nothing             ->  Message.reqAttr k

    fill :: [B.TokenTree] -> [Maybe B.TokenTree] -> B.Ab [B.TokenTree]
    fill (p : ps) (Nothing : xs)  =  Right . (p:) =<< fill ps xs
    fill (p : ps) (_       : xs)  =  Right . (p:) =<< fill ps xs
    fill []       (Just x  : xs)  =  Right . (x:) =<< fill [] xs
    fill []       (Nothing : _ )  =  Message.reqAttr "*"
    fill ps       []              =  Right $ ps


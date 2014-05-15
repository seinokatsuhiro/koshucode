{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Mapping from operand to operand, aka., operand editor.

module Koshucode.Baala.Core.Lexmap.Rodmap
( Rodmap,
  RodmapBody (..),
  rodmapCons,
  rodmapRun,
) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Lexmap.Operand  as C
import qualified Koshucode.Baala.Core.Lexmap.Slot     as C
import qualified Koshucode.Baala.Core.Message         as Message


-- ----------------------  Data type

-- | Rodmap with source code information.
type Rodmap = B.Sourced RodmapBody

-- | Rodmap operators.
data RodmapBody
    = RodmapId
      -- ^ Identity rodmap.
    | RodmapAdd     Bool String [B.TokenTree]
      -- ^ Add operand.
    | RodmapRename  (String, String)
      -- ^ Rename operand keyword.
    | RodmapAppend  [Rodmap]
      -- ^ Append rodmaps.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Cons and run

-- | Construct rodmap.
rodmapCons :: [B.TokenTree] -> B.Ab Rodmap
rodmapCons = loop where
    notKeyword ('-' : _) = False
    notKeyword _         = True

    right trees = Right . B.Sourced (B.untrees trees)

    loop trees =
        B.abortableTrees "rodmap" trees $ case B.divideTreesByBar trees of
          [ B.TreeL (B.TWord _ 0 op) : _ ]
            | op == "id"        ->  right trees $ RodmapId

          [ B.TreeL (B.TWord _ 0 op) : B.TreeL (B.TWord _ 0 k) : xs ]
            | notKeyword k      ->  Message.reqOperandName k
            | op == "add"       ->  right trees $ RodmapAdd False k xs
            | op == "opt"       ->  right trees $ RodmapAdd True  k xs

          [ B.TreeL (B.TWord _ 0 op) : B.TreeL (B.TWord _ 0 k')
                : B.TreeL (B.TWord _ 0 k) : _ ]
            | notKeyword k'     ->  Message.reqOperandName k'
            | notKeyword k      ->  Message.reqOperandName k
            | op == "rename"    ->  right trees $ RodmapRename (k', k)

          [[ B.TreeB 1 _ xs ]]  ->  loop xs

          [[]]    ->  right [] RodmapId
          [_]     ->  Message.adlib "unknown rodmap"
          trees2  ->  do subs <- mapM loop trees2
                         right trees $ RodmapAppend subs

-- | Run rodmap.
rodmapRun :: Rodmap -> B.AbMap C.Rod
rodmapRun = loop where
    loop (B.Sourced toks rmap) rod =
        B.abortable "rodmap" toks $ case rmap of
          RodmapId              ->  Right rod
          RodmapAdd opt k xs    ->  add rod opt k xs
          RodmapRename (k', k)  ->  rename rod k' k
          RodmapAppend rs       ->  B.foldM (flip loop) rod rs

    add rod opt k xs =
        case lookup k rod of
          Nothing             ->  do xs' <- C.slotTrees [] rod xs
                                     Right $ (k, xs') : rod
          Just _ | opt        ->  Right rod
                 | otherwise  ->  Message.extraOperand

    rename rod k' k =
        case lookup k rod of
          Just _              ->  Right $ B.assocRename1 k' k rod
          Nothing             ->  Message.reqOperand k


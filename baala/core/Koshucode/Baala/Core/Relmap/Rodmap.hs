{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Mapping of operand to operand.

module Koshucode.Baala.Core.Relmap.Rodmap
( -- * Data type
  Rodmap,
  RodmapBody (..),
  rodmap,
  rodmapRun,
) where

import qualified Data.Generics                        as G
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap.Operand  as C
import qualified Koshucode.Baala.Core.Message         as Message

type Rodmap = B.Sourced RodmapBody

data RodmapBody
    = RodmapId
    | RodmapAdd     Bool String [B.TokenTree]
    | RodmapRename  String String
    | RodmapAppend  [Rodmap]
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

rodmap :: [B.TokenTree] -> B.Ab Rodmap
rodmap = loop where
    notHyphen ('-' : _) = False
    notHyphen _         = True

    right trees = Right . B.Sourced (B.untrees trees)

    loop trees =
        B.abortableTrees "rodmap" trees $ case B.divideTreesByBar trees of
          [ B.TreeL (B.TWord _ 0 op) : _ ]
            | op == "id"        ->  right trees $ RodmapId

          [ B.TreeL (B.TWord _ 0 op) : B.TreeL (B.TWord _ 0 n) : xs ]
            | notHyphen n       ->  Message.reqOperandName n
            | op == "add"       ->  right trees $ RodmapAdd False n xs
            | op == "opt"       ->  right trees $ RodmapAdd True  n xs

          [ B.TreeL (B.TWord _ 0 op) : B.TreeL (B.TWord _ 0 n')
                : B.TreeL (B.TWord _ 0 n) : _ ]
            | notHyphen n'      ->  Message.reqOperandName n'
            | notHyphen n       ->  Message.reqOperandName n
            | op == "rename"    ->  right trees $ RodmapRename n' n

          [[ B.TreeB 1 _ xs ]]  ->  loop xs

          [[]]    ->  right [] RodmapId
          [_]     ->  Message.adlib "unknown rodmap"
          trees2  ->  do subs <- mapM loop trees2
                         right trees $ RodmapAppend subs

rodmapRun :: Rodmap -> B.AbMap C.Rod
rodmapRun = loop where
    loop (B.Sourced toks rmap) rod =
        B.abortable "rodmap" toks $ case rmap of
          RodmapId            ->  Right rod
          RodmapAdd opt n xs  ->  add rod opt n xs
          RodmapRename n' n   ->  rename rod n' n
          RodmapAppend rs     ->  B.foldM (exch loop) rod rs

    add rod opt n xs =
        case lookup n rod of
          Nothing             ->  Right $ (n, xs) : rod
          Just _ | opt        ->  Right rod
                 | otherwise  ->  Message.extraOperand

    rename rod n' n =
        case lookup n rod of
          Just _              ->  Right $ B.assocRename1 n' n rod
          Nothing             ->  Message.reqOperand n

exch :: (b -> a -> c) -> a -> b -> c
exch f a b = f b a


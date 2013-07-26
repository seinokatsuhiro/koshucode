{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Extension
(
  -- * Library
  litContent,
  litOperators,
  litJudge,
) where

import Koshucode.Baala.Base

import Koshucode.Baala.Core.Content.Class
import Koshucode.Baala.Core.Content.Literalize



-- ----------------------  Operator

litContent :: (CContent c) => LitTree c
litContent = litContentBy litOperators

litOperators :: (CContent c) => LitOperators c
litOperators =
    [ o "'"        litText
    ] where o = (,)

-- litText :: (CContent c) => LitTree c -> LitTrees c
-- litText lit xs = Right . joinContent =<< mapM lit xs

litText :: (CContent c) => LitTree c -> LitTrees c
litText _ xs =
    do ws <- litWords xs
       cs <- mapM loop ws
       Right . putText $ concatMap unbox $ intersperseBy sp cs
    where
      loop ('#' : w)
          = case hashWord w of
              Just w2 -> Right (False, w2)
              Nothing -> Left $ AbortUnknownSymbol ('#' : w)
      loop w = Right (True, w)

      unbox (_, w) = w

      sp (b1, _) (b2, _)
          | b1 && b2  = Just (True, " ")
          | otherwise = Nothing

litWords :: LitTrees [String]
litWords = mapM word where
    word (TreeL (TWord _ _ w)) = Right w
    word x = Left $ AbortReqText (show x)

intersperseBy :: (a -> a -> Maybe a) -> [a] -> [a]
intersperseBy f = loop where
    loop (x1 : xs@(x3 : _)) =
        case f x1 x3 of
          Just x2 -> x1 : x2 : loop xs
          Nothing -> x1 : loop xs
    loop [x1] = [x1]
    loop [] = []



-- ----------------------  Judge

litJudge
    :: (CContent c)
    => Bool                -- ^ Logical quality
    -> Relsign             -- ^ Judgement pattern
    -> LitTrees (Judge c)  -- ^ Convertor into judge
litJudge = litJudgeBy litOperators

{-| Construct judge from token trees.
    Judges itself are not content type.
    It can be only used in the top-level of sections. -}
litJudgeBy
    :: (CContent c)
    => [Named (LitTree c -> LitTrees c)]
    -> Bool                -- ^ Logical quality
    -> Relsign             -- ^ Judgement pattern
    -> LitTrees (Judge c)  -- ^ Convertor into judge
litJudgeBy ops q p xs =
  do xs' <- litTermset (litContentBy ops) xs
     Right $ Judge q p xs'



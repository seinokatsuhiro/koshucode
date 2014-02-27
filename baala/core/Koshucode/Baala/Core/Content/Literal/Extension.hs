{-# OPTIONS_GHC -Wall #-}

{-| Extensions of literalizer -}

module Koshucode.Baala.Core.Content.Literal.Extension
(
  -- * Library
  litContent,
  litOperators,
  litJudge,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Content.Literal.Class   as C
import qualified Koshucode.Baala.Core.Content.Literal.Literal as C



-- ----------------------  Operator

litContent :: (C.CContent c) => C.Literalize c
litContent = C.litContentBy litOperators

litOperators :: (C.CContent c) => C.LitOperators c
litOperators = [ ]

-- litText :: (CContent c) => Literalize c -> LitTrees c
-- litText lit xs = Right . joinContent =<< mapM lit xs

-- litText :: (CContent c) => Literalize c -> LitTrees c
-- litText _ xs =
--     do ws <- litWords xs
--        cs <- mapM loop ws
--        Right . pText $ concatMap unbox $ intersperseBy sp cs
--     where
--       loop ('#' : w)
--           = case B.hashWord w of
--               Just w2 -> Right (False, w2)
--               Nothing -> Left $ B.AbortUnkSymbol ('#' : w)
--       loop w = Right (True, w)

--       unbox (_, w) = w

--       sp (b1, _) (b2, _)
--           | b1 && b2  = Just (True, " ")
--           | otherwise = Nothing

-- litWords :: LitTrees [String]
-- litWords = mapM word where
--     word (B.TreeL (B.TWord _ _ w)) = Right w
--     word x = Left $ B.AbortReqText (show x)

-- intersperseBy :: (a -> a -> Maybe a) -> [a] -> [a]
-- intersperseBy f = loop where
--     loop (x1 : xs@(x3 : _)) =
--         case f x1 x3 of
--           Just x2 -> x1 : x2 : loop xs
--           Nothing -> x1 : loop xs
--     loop [x1] = [x1]
--     loop [] = []



-- ----------------------  Judge

litJudge
    :: (C.CContent c)
    => Bool                  -- ^ Logical quality
    -> B.JudgePattern        -- ^ Judgement pattern
    -> C.LitTrees (B.Judge c)  -- ^ Convertor into judge
litJudge = litJudgeBy litOperators

{-| Construct judge from token trees.
    Judges itself are not content type.
    It can be only used in the top-level of sections. -}
litJudgeBy
    :: (C.CContent c)
    => [B.Named (C.Literalize c -> C.LitTrees c)]
    -> Bool                   -- ^ Logical quality
    -> B.JudgePattern         -- ^ Judgement pattern
    -> C.LitTrees (B.Judge c) -- ^ Convertor into judge
litJudgeBy ops q p xs =
  do xs' <- C.litTermset (C.litContentBy ops) xs
     Right $ B.Judge q p xs'



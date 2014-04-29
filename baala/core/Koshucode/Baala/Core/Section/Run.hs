{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run section.
module Koshucode.Baala.Core.Section.Run
( runSection,
) where

import qualified Data.Monoid                          as M
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Message         as Message



-- --------------------------------------------  Run

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab (B.OutputResult c)
runSection global sects =
    do let s2 = M.mconcat sects
           g2 = global { C.globalJudges = C.secJudge s2 }
       runSectionBody g2 s2

runSectionBody :: forall c. (Ord c, B.Pretty c, C.CRel c, C.CNil c) =>
    C.Global c -> C.Section c -> B.Ab (B.OutputResult c)
runSectionBody global C.Section { C.secSlot   = slot
                                , C.secTokmap = tok
                                , C.secAssert = ass
                                , C.secCons   = cons } =

    do ass2    <- mapM consAssert `B.shortMapM` ass
       judgesV <- run $ C.assertViolated ass2
       judgesN <- run $ C.assertNormal   ass2
       Right (B.shortTrim judgesV, B.shortTrim judgesN)
    where
      run :: C.ShortAsserts c -> B.Ab [B.OutputChunks c]
      run = mapM B.shortM . run2

      run2 :: C.ShortAsserts c -> [B.Short (B.Ab [B.OutputChunk c])]
      run2 = B.shortMap $ C.runAssertJudges global

      lexmap = C.consLexmap cons
      relmap = C.consRelmap cons

      consAssert :: B.AbMap (C.Assert c)
      consAssert a =
          B.abortableFrom "assert" a $ do
            let lx = C.assLexmap a
            rmap  <- relmap lx
            lxs   <- substSlot slot lexmap lx tok
            parts <- B.sequenceSnd $ B.mapSndTo relmap lxs
            Right $ a { C.assRelmap = Just rmap
                      , C.assParts  = parts }



-- ----------------------  Slot substitution

substSlot :: [B.NamedTrees] -> C.ConsLexmap -> C.Lexmap
    -> [B.NamedTrees]
    -> B.Ab [C.Rody C.Lexmap]
substSlot slot cons lexmap def = loop lexmap where
    loop lx = let rop = C.lexOpText lx
                  rod = C.lexOperand lx
                  subuse = loops $ C.lexSubmap lx
              in B.abortableFrom "slot" lx $
                 case lookup rop def of
                   Nothing    -> subuse
                   Just trees ->
                       do trees' <- substTrees slot rod trees
                          lx2    <- cons trees'
                          use2   <- loop lx2
                          use3   <- subuse
                          Right $ ((rop, rod), lx2) : use2 ++ use3

    loops :: [C.Lexmap] -> B.Ab [C.Rody C.Lexmap]
    loops [] = Right []
    loops (lx : lxs) = do lx'  <- loop lx
                          lxs' <- loops lxs
                          Right $ lx' ++ lxs'

substTrees :: [B.NamedTrees] -> C.Rod -> B.AbMap [B.TokenTree]
substTrees slot rod trees =
    do trees' <- substTree slot rod `mapM` trees
       Right $ concat trees'

substTree :: [B.NamedTrees] -> C.Rod -> B.TokenTree -> B.Ab [B.TokenTree]
substTree slot rod tree = B.abortableTree "slot" tree $ loop tree where
    loop (B.TreeB p q sub) =
        do sub' <- mapM loop sub
           Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (B.TSlot _ n name))
        | n == 0  = case lookup "@trunk" rod of
                      Nothing -> Message.noSlotLeaf name
                      Just od -> od `pos` name
        | n == 1  = case lookup ('-' : name) rod of
                      Nothing -> Message.noSlotLeaf name
                      Just od -> Right od
        | n == 2  = case lookup name slot of
                      Nothing -> Message.noSlotLeaf name
                      Just od -> Right od
        | otherwise = Message.noSlotLeaf name
    loop tk = Right [tk]

    pos :: [B.TokenTree] -> String -> B.Ab [B.TokenTree]
    pos od "all" = Right od
    pos od n     = case (reads :: ReadS Int) n of
                     [(i, "")] -> Right . B.singleton =<< od `at` i
                     _         -> Message.noSlotLeaf n

    at = slotIndex $ unwords . map B.tokenContent . B.untree

slotIndex :: (a -> String) -> [a] -> Int -> B.Ab a
slotIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Message.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


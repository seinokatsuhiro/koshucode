{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Run section.
module Koshucode.Baala.Core.Section.Run
( runSection,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Content         as C
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Assert          as C
import qualified Koshucode.Baala.Core.Section.Section as C
import qualified Koshucode.Baala.Core.Message         as Message



-- --------------------------------------------  Run

runSection :: (C.CContent c) => C.Global c -> [C.Section c] -> B.Ab (B.OutputResult c)
runSection global sects =
    do s2 <- assembleRelmap $ B.mconcat sects
       let g2 = global { C.globalJudges = C.secJudge s2 }
       runSectionBody g2 s2

runSectionBody :: forall c. (Ord c, B.Pretty c, C.CRel c, C.CNil c) =>
    C.Global c -> C.Section c -> B.Ab (B.OutputResult c)
runSectionBody global C.Section { C.secAssert = ass } =
    do js1 <- run $ C.assertViolated ass
       js2 <- run $ C.assertNormal   ass
       Right (B.shortTrim js1, B.shortTrim js2)
    where
      run :: [C.ShortAssert c] -> B.Ab [B.OutputChunks c]
      run = mapM B.shortM . run2

      run2 :: [C.ShortAssert c] -> [B.Short (B.Ab [B.OutputChunk c])]
      run2 = B.shortMap $ C.runAssertJudges global

assembleRelmap :: forall c. B.AbMap (C.Section c)
assembleRelmap s@C.Section { C.secSlot   = gslot
                           , C.secTokmap = tokmaps
                           , C.secAssert = ass
                           , C.secCons   = C.RelmapCons
                                           { C.consLexmap = lexmap
                                           , C.consRelmap = relmap }} =
    do ass2 <- mapM assemble `B.shortMapM` ass
       Right $ s { C.secAssert = ass2 }
    where
      assemble :: B.AbMap (C.Assert c)
      assemble a =
          B.abortableFrom "assert" a $ do
            trees <- slotTrees gslot [] $ C.assTree a
            lx    <- lexmap trees
            rmap  <- relmap lx
            lxs   <- slotLexmap lexmap gslot tokmaps lx
            parts <- B.sequenceSnd $ B.mapSndTo relmap lxs
            Right $ a { C.assRelmap = Just rmap
                      , C.assParts  = parts }



-- ----------------------  Slot substitution

slotLexmap :: C.ConsLexmap -> [B.NamedTrees] -> [B.NamedTrees]
           -> C.Lexmap -> B.Ab [C.Rody C.Lexmap]
slotLexmap lexmap gslot tokmaps = loop where
    loop lx = let rop    = C.lexOpText  lx
                  rod    = C.lexOperand lx
                  subuse = loops $ C.lexSubmap lx
              in B.abortableFrom "slot" lx $
                 case lookup rop tokmaps of
                   Nothing    -> subuse
                   Just trees ->
                       do trees' <- slotTrees gslot rod trees
                          lx2    <- lexmap trees'
                          use2   <- loop lx2
                          use3   <- subuse
                          Right $ ((rop, rod), lx2) : use2 ++ use3

    loops :: [C.Lexmap] -> B.Ab [C.Rody C.Lexmap]
    loops [] = Right []
    loops (lx : lxs) = do lx'  <- loop lx
                          lxs' <- loops lxs
                          Right $ lx' ++ lxs'

slotTrees :: [B.NamedTrees] -> C.Rod -> B.AbMap [B.TokenTree]
slotTrees gslot rod trees =
    do trees' <- slotTree gslot rod `mapM` trees
       Right $ concat trees'

slotTree :: [B.NamedTrees] -> C.Rod -> B.TokenTree -> B.Ab [B.TokenTree]
slotTree gslot rod tree = B.abortableTree "slot" tree $ loop tree where
    loop (B.TreeB p q sub) = do sub' <- mapM loop sub
                                Right [B.TreeB p q $ concat sub']
    loop (B.TreeL (B.TSlot _ n name))
        | n == 0    = replace n name "@trunk"     rod  (`pos` name)
        | n == 1    = replace n name ('-' : name) rod  Right
        | n == 2    = replace n name name         gslot Right
        | otherwise = Message.noSlotName n name
    loop tk = Right [tk]

    replace n name key assoc f =
        case lookup key assoc of
          Just od -> f od
          Nothing -> Message.noSlotName n name

    pos :: [B.TokenTree] -> String -> B.Ab [B.TokenTree]
    pos od "all" = Right od
    pos od n     = case (reads :: ReadS Int) n of
                     [(i, "")] -> Right . B.singleton =<< od `at` i
                     _         -> Message.noSlotName 0 n

    at = slotIndex $ unwords . map B.tokenContent . B.untree

slotIndex :: (a -> String) -> [a] -> Int -> B.Ab a
slotIndex toString xxs n = loop xxs n where
    loop (x : _)  1 = Right x
    loop (_ : xs) i = loop xs $ i - 1
    loop _ _        = Message.noSlotIndex (number $ map toString xxs) n
    number xs = map pair $ zip [1 :: Int ..] xs
    pair (i, x) = "@'" ++ show i ++ " = " ++ x


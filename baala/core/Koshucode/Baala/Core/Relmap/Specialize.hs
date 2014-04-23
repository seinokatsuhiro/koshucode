{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialize generic relmaps to specific relmaps

module Koshucode.Baala.Core.Relmap.Specialize
( relmapSpecialize, 
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap.Lexmap   as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C
import qualified Koshucode.Baala.Core.Relmap.Rop      as C
import qualified Koshucode.Baala.Core.Message         as Message



relmapSpecialize :: forall c. C.Global c -> [C.RelmapAssoc c]
  -> [C.RelkitDef c] -> Maybe B.Relhead -> C.Relmap c -> B.Ab ([C.RelkitDef c], C.Relkit c)
relmapSpecialize global rdef = spec [] [] where
    spec :: [(String, B.Relhead)] -- name of nested relation, and its heading
         -> [C.RelkitKey]         -- information for detecting cyclic relmap
         -> [C.RelkitDef c]       -- list of known specialized relkits
         -> Maybe B.Relhead       -- input head feeding into generic relmap
         -> C.Relmap c            -- generic relmap to specialize
         -> B.Ab ([C.RelkitDef c], C.Relkit c)
    spec with keys kdef he1 rmap = s where
        s = case rmap of
              C.RelmapSource lx p ns -> post lx $ Right (kdef, C.relkitSource p ns)
              C.RelmapConst  lx rel  -> post lx $ Right (kdef, C.relkitConst rel)

              C.RelmapAppend rmap1 rmap2 ->
                  do (kdef2, kit2) <- spec with keys kdef he1 rmap1
                     (kdef3, kit3) <- spec with keys kdef2 (C.relkitHead kit2) rmap2
                     Right (kdef3, B.mappend kit2 kit3)

              C.RelmapCalc lx makeKit rmaps ->
                  post lx $ do
                     (kdef2, kits) <- list kdef rmaps
                     kit <- makeKit kits he1
                     Right (kdef2, kit)

              C.RelmapGlobal lx makeKit ->
                  post lx $ do
                     kit <- makeKit global he1
                     Right (kdef, kit)

              C.RelmapLink lx n od ->
                  post lx $ case lookup n with of
                     Just he -> Right (kdef, C.relkitNest n he)
                     Nothing -> case lookup (n, od) rdef of
                       Nothing    -> Message.unkRelmap n
                       Just rmap1 -> link n rmap1 (he1, C.relmapLexList rmap1)

              C.RelmapCopy lx n rmap1 ->
                  do let heJust = B.fromJust he1
                         with' = (n, heJust) : with
                     (kdef2, kit2) <- post lx $ spec with' keys kdef he1 rmap1
                     Right (kdef2, C.relkitCopy n kit2)

              C.RelmapWith lx with1 rmap1 ->
                  do let terms    = map fst with1
                         heJust   = B.fromJust he1
                         heWith   = B.assocPick terms $ B.headNested heJust
                         heInd    = terms `B.snipIndex` B.headNames heJust
                         with'    = heWith ++ with
                         withInd  = zip terms heInd
                     (kdef2, kit2) <- post lx $ spec with' keys kdef he1 rmap1
                     Right (kdef2, C.relkitWith withInd kit2)

        post :: C.Lexmap -> B.Map (B.Ab ([C.RelkitDef c], C.Relkit c))
        post lx result =
            B.abortableFrom "specialize" lx $ do
               (kdef2, kit) <- result
               Right (kdef2, C.relkitSetSource lx kit)

        -- specialize subrelmaps to subrelkits
        list :: [C.RelkitDef c] -> [C.Relmap c] -> B.Ab ([C.RelkitDef c], [C.Relkit c])
        list kdef1 [] = Right (kdef1, [])
        list kdef1 (rmap1 : rmaps) =
            do (kdef2, kit)  <- spec with keys kdef1 he1 rmap1
               (kdef3, kits) <- list kdef2 rmaps
               Right (kdef3, kit : kits)

        link :: String -> C.Relmap c -> C.RelkitKey -> B.Ab ([C.RelkitDef c], C.Relkit c)
        link n rmap1 key1
            | key1 `elem` keys = Right (kdef, cyclic)
            | otherwise = case lookup key1 kdef of
                Just kit -> Right (kdef, kit)
                Nothing  -> do (kdef2, kit1) <- spec with (key1 : keys) kdef he1 rmap1
                               let kdef3 = (key1, kit1) : kdef2
                               Right (kdef3, acyclic kit1)
            where
              cyclic       =  C.Relkit Nothing             body
              acyclic kit1 =  C.Relkit (C.relkitHead kit1) body
              body         =  B.Sourced [] $ C.RelkitLink n key1 Nothing

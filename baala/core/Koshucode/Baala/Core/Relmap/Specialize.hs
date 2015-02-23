{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialize generic relmaps to specific relmaps

module Koshucode.Baala.Core.Relmap.Specialize
  ( relmapSpecialize, 
    RelmapLinkTable',
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relmap.Global   as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C
import qualified Koshucode.Baala.Core.Relmap.Relmap   as C
import qualified Koshucode.Baala.Core.Message         as Msg

type RelmapLinkTable' h c = [(C.Lexmap, C.Relmap' h c)]

relmapSpecialize :: forall h. forall c. (C.GetGlobal h)
    => h c -> RelmapLinkTable' h c -> [C.RelkitDef c]
    -> Maybe B.Head -> C.Relmap' h c -> B.Ab ([C.RelkitDef c], C.Relkit c)
relmapSpecialize hook links = spec [] [] where
    spec :: [((B.Token, String), B.Head)]  -- name of nested relation, and its heading
         -> [C.RelkitKey]        -- information for detecting cyclic relmap
         -> [C.RelkitDef c]      -- list of known specialized relkits
         -> Maybe B.Head         -- input head feeding into generic relmap
         -> C.Relmap' h c        -- generic relmap to specialize
         -> B.Ab ([C.RelkitDef c], C.Relkit c)
    spec nest keys kdef he1 rmap = s where
        s = case rmap of
              C.RelmapSource lx p ns -> post lx $ Right (kdef, C.relkitSource p ns)
              C.RelmapConst  lx rel  -> post lx $ Right (kdef, C.relkitConst rel)

              C.RelmapAppend rmap1 rmap2 ->
                  do (kdef2, kit2) <- spec nest keys kdef he1 rmap1
                     (kdef3, kit3) <- spec nest keys kdef2 (C.relkitHead kit2) rmap2
                     Right (kdef3, B.mappend kit2 kit3)

              C.RelmapCalc lx makeKit rmaps ->
                  post lx $ do
                     (kdef2, kits) <- list kdef rmaps
                     kit <- makeKit kits he1
                     Right (kdef2, kit)

              C.RelmapHook lx makeKit ->
                  post lx $ do
                     kit <- makeKit hook he1
                     Right (kdef, kit)

              C.RelmapLink lx
                  | C.lexType lx == C.LexmapLocal ->
                      post lx $ case find ps n of
                           Just (p, he)  -> Right (kdef, C.relkitNestVar p n he)
                           Nothing       -> Msg.unkNestVar n ps nest
                  | otherwise ->
                      post lx $ case lookup lx links of
                           Just rmap1 -> link n rmap1 (he1, C.relmapLexmaps rmap1)
                           Nothing    -> Msg.unkRelmap n
                  where
                    n   = C.lexRopName lx
                    ps  = C.lexParent  lx

              C.RelmapCopy lx n rmap1 ->
                  do let p      = C.lexRopToken lx
                         heJust = B.fromJust he1
                         nest'  = ((p,n), heJust) : nest
                     (kdef2, kit2) <- post lx $ spec nest' keys kdef he1 rmap1
                     Right (kdef2, C.relkitCopy p n kit2)

              C.RelmapNest lx rmap1 ->
                  do let heJust    = B.fromJust he1
                         heNest    = B.headNested heJust
                         vars      = map fst heNest
                         p         = C.lexRopToken lx
                         tk (n,he) = ((p,n), he)
                         heInd     = vars `B.snipIndex` B.headNames heJust
                         nest'     = map tk heNest ++ nest
                         nestInd   = zip vars heInd
                     (kdef2, kit2) <- post lx $ spec nest' keys kdef he1 rmap1
                     Right (kdef2, C.relkitNest p nestInd kit2)

        post :: C.Lexmap -> B.Map (B.Ab ([C.RelkitDef c], C.Relkit c))
        post lx result =
            Msg.abSpecialize [lx] $ do
               (kdef2, kit) <- result
               Right (kdef2, C.relkitSetSource lx kit)

        find [] _     = Nothing
        find (p:ps) n = case lookup (p,n) nest of
                          Nothing -> find ps n
                          Just he -> Just (p,he)

        -- specialize subrelmaps to subrelkits
        list :: [C.RelkitDef c] -> [C.Relmap' h c] -> B.Ab ([C.RelkitDef c], [C.Relkit c])
        list kdef1 [] = Right (kdef1, [])
        list kdef1 (rmap1 : rmaps) =
            do (kdef2, kit)  <- spec nest keys kdef1 he1 rmap1
               (kdef3, kits) <- list kdef2 rmaps
               Right (kdef3, kit : kits)

        link :: String -> C.Relmap' h c -> C.RelkitKey -> B.Ab ([C.RelkitDef c], C.Relkit c)
        link n rmap1 key1
            | key1 `elem` keys = Right (kdef, cyclic)
            | otherwise = case lookup key1 kdef of
                Just kit -> Right (kdef, kit)
                Nothing  -> do (kdef2, kit1) <- spec nest (key1 : keys) kdef he1 rmap1
                               let kdef3 = (key1, kit1) : kdef2
                               Right (kdef3, acyclic kit1)
            where
              cyclic       =  C.Relkit Nothing             body
              acyclic kit1 =  C.Relkit (C.relkitHead kit1) body
              body         =  B.Sourced [] $ C.RelkitLink n key1 Nothing


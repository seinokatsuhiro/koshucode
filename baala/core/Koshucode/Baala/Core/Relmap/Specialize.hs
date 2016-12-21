{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Specialize generic relmaps.
--   Generic relmaps are represented by 'C.Relmap',
--   specialized relmaps by 'C.Relkit'.

module Koshucode.Baala.Core.Relmap.Specialize
  ( relmapSpecialize, 
    RelmapLinkTable',
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Type                 as T
import qualified Koshucode.Baala.Core.Lexmap          as C
import qualified Koshucode.Baala.Core.Relkit          as C
import qualified Koshucode.Baala.Core.Relmap.Relmap   as C
import qualified Koshucode.Baala.Core.Lexmap.Message  as Msg
import qualified Koshucode.Baala.Core.Relmap.Message  as Msg

-- | Table for referencing relmap of 'C.RelmapLink'.
type RelmapLinkTable' h c = [(C.Lexmap, C.Relmap' h c)]

-- | Specializes generic relmap to specialized relmap.
--   Specialized relmaps have fixed input/output headings.
--   In constrast, generic relmaps do not.
--   Specialized relmaps are also called relkits.
--   This function returns relkit and extended table of relkits.
relmapSpecialize :: forall h. forall c.
    h c                      -- ^ Hook.
    -> RelmapLinkTable' h c  -- ^ Lexmap-to-relmap search table.
    -> C.RelkitTable c       -- ^ List of known relkits, i.e.,
                             --   search table from relkit key to actual relkit.
    -> Maybe T.Head          -- ^ Input heading feeding into generic relmap.
    -> C.Relmap' h c         -- ^ Generic relmap to specialize.
    -> B.Ab (C.RelkitTable c, C.Relkit c) -- ^ Extended table and specialized relmap.
relmapSpecialize hook links = spec [] [] where
    spec :: [((S.Token, S.LocalRef), T.Head)] -- Name of local relation, and its heading
         -> [C.RelkitKey]                     -- Data for detecting cyclic relmap
         -> C.RelkitTable c -> Maybe T.Head   -- Arguments of this function
         -> C.Relmap' h c -> B.Ab (C.RelkitTable c, C.Relkit c)
    spec local keys kdef he1 rmap = s where
        s = case rmap of
              C.RelmapSource lx p ns -> post lx $ Right (kdef, C.relkitSource p ns)
              C.RelmapConst  lx rel  -> post lx $ Right (kdef, C.relkitConst rel)

              C.RelmapAppend rmap1 rmap2 ->
                  do (kdef2, kit2) <- spec local keys kdef he1 rmap1
                     (kdef3, kit3) <- spec local keys kdef2 (C.relkitOutput kit2) rmap2
                     Right (kdef3, kit2 O.++ kit3)

              C.RelmapCalc lx makeKit rmaps ->
                  post lx $ do
                     (kdef2, kits) <- list kdef rmaps
                     kit <- makeKit kits he1
                     Right (kdef2, kit)

              C.RelmapHook lx makeKit ->
                  post lx $ do
                     kit <- makeKit hook he1
                     Right (kdef, kit)

              C.RelmapCopy lx n rmap1 ->
                  do let p      = C.lexToken lx
                         heJust = B.fromJust he1
                         local' = ((p, S.LocalSymbol n), heJust) : local
                     (kdef2, kit2) <- post lx $ spec local' keys kdef he1 rmap1
                     Right (kdef2, C.relkitCopy p n kit2)

              C.RelmapNest lx rmap1 ->
                  do let heJust      = B.fromJust he1
                         heNest      = T.headNested heJust
                         nest        = map fst heNest  -- nest terms
                         p           = C.lexToken lx
                         tk (n, he)  = ((p, S.LocalNest n), he)
                         heInd       = T.pickTermsIndex $ T.termPicker nest heJust
                         local'      = map tk heNest ++ local
                         nestInd     = zip nest heInd
                     (kdef2, kit2)  <- post lx $ spec local' keys kdef he1 rmap1
                     Right (kdef2, C.relkitNest p nestInd kit2)

              C.RelmapLink lx
                  | C.lexType lx == C.LexmapLocal ->
                      post lx $ case loc of
                           Just (p,he,ref) -> Right (kdef, C.relkitLocal p ref he)
                           Nothing         -> Msg.unkNestVar n ps local
                  | otherwise ->
                      post lx $ case lookup lx links of
                           Just rmap1 -> link n rmap1 (he1, C.relmapLexmaps rmap1)
                           Nothing    -> Msg.unkRelmap n
                  where
                    tok = C.lexToken  lx
                    n   = C.lexName   lx
                    ps  = S.tokenParents tok
                    loc = do (p, he) <- find tok
                             ref     <- C.lexLocalRef lx
                             Just (p,he,ref)

        post :: C.Lexmap -> O.Map (B.Ab (C.RelkitTable c, C.Relkit c))
        post lx result =
            Msg.abSpecialize [lx] $ do
               (kdef2, kit) <- result
               Right (kdef2, C.relkitSetSource lx kit)

        find (S.TLocal cp v eid (p:ps)) =
            case lookup ((p,v)) local of
              Just he -> Just (p,he)
              Nothing -> find (S.TLocal cp v eid ps)
        find _ = Nothing

        -- specialize subrelmaps to subrelkits
        list :: C.RelkitTable c -> [C.Relmap' h c] -> B.Ab (C.RelkitTable c, [C.Relkit c])
        list kdef1 [] = Right (kdef1, [])
        list kdef1 (rmap1 : rmaps) =
            do (kdef2, kit)  <- spec local keys kdef1 he1 rmap1
               (kdef3, kits) <- list kdef2 rmaps
               Right (kdef3, kit : kits)

        link :: String -> C.Relmap' h c -> C.RelkitKey -> B.Ab (C.RelkitTable c, C.Relkit c)
        link n rmap1 key1
            | key1 `elem` keys = Right (kdef, cyclic)
            | otherwise = case lookup key1 kdef of
                Just kit -> Right (kdef, kit)
                Nothing  -> do (kdef2, kit1) <- spec local (key1 : keys) kdef he1 rmap1
                               let kdef3 = (key1, kit1) : kdef2
                               Right (kdef3, acyclic kit1)
            where
              cyclic        = C.Relkit Nothing Nothing body
              acyclic kit1  = C.Relkit Nothing (C.relkitOutput kit1) body
              body          = B.Codic [] $ C.RelkitLink n key1 Nothing


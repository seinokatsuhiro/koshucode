{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Operations on 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Relmap
( 
  -- * Select from relmap
  relmapSourceList,
  relmapNameList,

  -- * Construct relmap
  relmapSource,
  relmapConst,
  relmapFlow,
  relmapGlobal,
  relmapBinary,
  relmapConfl,
  relmapCopy,
  relmapWith,
  relmapLink,

  -- * Specialize relmap to relkit
  relmapSpecialize,

  -- * Append relmaps
  -- $AppendRelmaps
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap.Rop      as C
import qualified Koshucode.Baala.Core.Relmap.Lexical  as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C
import qualified Koshucode.Baala.Core.Message         as Message
import Data.Maybe (fromJust)


-- ----------------------  Select

-- | List of 'C.RelmapSource'
relmapSourceList :: C.Relmap c -> [C.Relmap c]
relmapSourceList = relmapList f where
    f rmap@(C.RelmapSource _ _ _) = [rmap]
    f _ = []

-- | List of name in 'C.RelmapLink'
relmapNameList :: C.Relmap c -> [String]
relmapNameList = relmapList f where
    f (C.RelmapLink _ n) = [n]
    f _ = []

relmapList :: B.Map (C.Relmap c -> [a])
relmapList f = loop where
    loop (C.RelmapAppend rmap1 rmap2) = loop rmap1 ++ loop rmap2
    loop (C.RelmapCalc _ _ rmaps)     = concatMap loop rmaps
    loop m = f m

-- relmapMap :: B.Map (C.Relmap c) -> B.Map (C.Relmap c)
-- relmapMap f = loop where
--     loop rmap =
--         case rmap of
--           C.RelmapCalc lx confl rmaps -> C.RelmapCalc lx confl $ map loop rmaps
--           C.RelmapWith lx with rmap1  -> C.RelmapWith lx with  $     loop rmap1
--           C.RelmapAppend rmap1 rmap2  -> C.RelmapAppend (loop rmap1) (loop rmap2)
--           _ -> f rmap

-- relmapAbMap :: B.AbMap (C.Relmap c) -> B.AbMap (C.Relmap c)
-- relmapAbMap f = loop where
--     loop rmap =
--         case rmap of
--           C.RelmapCalc lx confl rmaps -> do rmap2 <- loop `mapM` rmaps
--                                             Right $ C.RelmapCalc lx confl rmap2
--           C.RelmapWith lx with rmap1  -> do rmap2 <- loop rmap1
--                                             Right $ C.RelmapWith lx with rmap2
--           C.RelmapAppend rmap1 rmap2  -> do rmap3 <- loop rmap1
--                                             rmap4 <- loop rmap2
--                                             Right $ C.RelmapAppend rmap3 rmap4
--           _ -> f rmap


-- ----------------------  Construct

-- | Retrieve relation from dataset.
relmapSource :: C.RopUse c -> B.JudgePattern -> [B.TermName] -> (C.Relmap c)
relmapSource = C.RelmapSource . C.ropLex

-- | Make a constant relmap.
relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . C.ropLex

-- | Make a flow relmap.
--   Flow relmaps take no subrelmaps.
relmapFlow :: C.RopUse c -> C.RelkitCalc c -> C.Relmap c
relmapFlow use relkit = relmapConfl use (const relkit) []

-- | Make a global relmap.
--   Global relmaps are flow relmaps with globals.
relmapGlobal :: C.RopUse c -> C.RelkitGlobal c -> C.Relmap c
relmapGlobal = C.RelmapGlobal . C.ropLex

-- | Make a binary relmap.
--   Binary relmaps take one subrelmap.
relmapBinary :: C.RopUse c -> C.RelkitBinary c -> C.Relmap c -> C.Relmap c
relmapBinary use kit rmap = relmapConfl use (kit . head) [rmap]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple subrelmaps.
relmapConfl :: C.RopUse c -> C.RelkitConfl c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . C.ropLex

relmapCopy :: C.RopUse c -> String -> B.Map (C.Relmap c)
relmapCopy = C.RelmapCopy . C.ropLex

relmapWith :: C.RopUse c -> [(B.TermName, String)] -> B.Map (C.Relmap c)
relmapWith = C.RelmapWith . C.ropLex

relmapLink :: C.RopUse c -> String -> C.Relmap c
relmapLink = C.RelmapLink . C.ropLex



-- ----------------------  Specialize

relmapSpecialize :: forall c. C.Global c -> [C.RelmapDef c]
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

              C.RelmapLink lx n ->
                  post lx $ case lookup n with of
                     Just he -> Right (kdef, C.relkitNest n he)
                     Nothing -> case lookup n rdef of
                       Nothing    -> Message.unkRelmap n
                       Just rmap1 -> link n rmap1 (he1, C.relmapLexList rmap1)

              C.RelmapCopy lx n rmap1 ->
                  do let heJust = fromJust he1
                         with' = (n, heJust) : with
                     (kdef2, kit2) <- post lx $ spec with' keys kdef he1 rmap1
                     Right (kdef2, C.relkitCopy n kit2)

              C.RelmapWith lx with1 rmap1 ->
                  do let terms    = map fst with1
                         heJust   = fromJust he1
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



-- ----------------------
-- $AppendRelmaps
--
--  This picture represents calculation
--  of mapping input relation to output relation.
--
--  > input -[ relmap ]- output
--
--  Relmap /A/ maps relation /R1/ to relation /R2/.
--  Another relmap /B/ maps /R2/ to /R3/.
--
--  > R1 -[ A ]- R2
--  > R2 -[ B ]- R3
--
--  Two relmaps /A/ and /B/ are jointed
--  with intermidiate relation /R2/.
--
--  > R1 -[ A ]- R2 -[ B ]- R3
--
--  Or, we can draw a directly jointed picture.
--  This whole structure is also 'RelmapAppend' /A B/.
--
--  > R1 -[ A ]--[ B ]- R3


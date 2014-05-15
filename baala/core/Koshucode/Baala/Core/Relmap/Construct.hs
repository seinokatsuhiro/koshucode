{-# OPTIONS_GHC -Wall #-}

-- | Construct 'C.Lexmap' and 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
( -- * Data type
  RelmapCons (..),
  ConsLexmap,
  RelmapSource,
  ConsLexmapBody,
  ConsRelmap,

  -- * Function
  relmapCons,
  withTerms,

  -- * Construction process
  -- $ConstructionProcess
) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Relmap.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap.Operand   as C
import qualified Koshucode.Baala.Core.Relmap.Operator  as C
import qualified Koshucode.Baala.Core.Relmap.Rodmap    as C
import qualified Koshucode.Baala.Core.Relmap.Slot      as C
import qualified Koshucode.Baala.Core.Message          as Message


-- ----------------------  Constructions

-- | Constructor of lexmap and relmap.
data RelmapCons c = RelmapCons ConsLexmap (ConsRelmap c)

instance Show (RelmapCons c) where
    show _ = "RelmapCons <lex> <full>"

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: C.Global c -> RelmapCons c
relmapCons g = make $ unzip $ map pair $ C.globalRops g where
    make (l, r) = RelmapCons (consLexmap l) (consRelmap g r)
    pair (C.Rop n _ sorter cons _) = ((n, sorter), (n, cons))



-- ----------------------  Lexical relmap

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [B.NamedTrees] -> [RelmapSource] -> ConsLexmapBody

-- | Source of relmap: its name, replacement, and operand editor.
type RelmapSource = B.Named ([B.TokenTree], C.Rodmap)

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TokenTree] -> B.Ab (C.Lexmap, [C.Rody C.Lexmap])

consLexmap :: [B.Named C.RodSorter] -> ConsLexmap
consLexmap sorters gslot derives = lexmap where

    lexmap :: ConsLexmapBody
    lexmap source =
        B.abortableTrees "lexmap" source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TWord _ 0 _) : trees)] -> derived rop trees
           [(B.TreeL rop@(B.TWord _ 3 _) : trees)] -> user C.LexmapWith rop trees
           [[B.TreeB 1 _ trees]] -> lexmap trees
           [[B.TreeB _ _ _]]     -> Message.adlib "bracket"
           [[]]                  -> baseOf "id" []
           [_]                   -> Message.unkRelmap "???"
           trees2                -> baseOf "append" $ map B.treeWrap trees2

    derived :: B.Token -> ConsLexmapBody
    derived rop trees =
        let n = B.tokenContent rop
        in case lookup n derives of
             Just _  -> user C.LexmapDerived rop trees
             Nothing -> base n rop trees

    baseOf n = base n (B.tokenWord n)

    base :: String -> B.Token -> ConsLexmapBody
    base n rop trees =
        case lookup n sorters of
          Nothing     ->  user C.LexmapDerived rop trees
          Just sorter ->  do rod <- sorter trees
                             submap $ cons C.LexmapBase rop rod trees

    user :: C.LexmapType -> B.Token -> ConsLexmapBody
    user C.LexmapWith rop [] = submap $ cons C.LexmapWith rop [] []
    user C.LexmapWith _ _ = Message.extraOperand
    user ty rop trees = do rod <- C.rodBranch trees
                           submap $ cons ty rop rod trees

    cons :: C.LexmapType -> B.Token -> [B.NamedTrees] -> [B.TokenTree] -> C.Lexmap
    cons ty rop rod trees =
        check $ C.Lexmap { C.lexType    = ty
                         , C.lexOpToken = rop
                         , C.lexOperand = (("@operand", trees) : rod)
                         , C.lexSubmap  = []
                         , C.lexMessage = [] }

    check :: B.Map C.Lexmap
    check lx | C.lexType lx == C.LexmapDerived
                 = let n = C.lexOpName lx
                       msg  = "Same name as base relmap operator '" ++ n ++ "'"
                   in case lookup n sorters of
                        Just _  -> C.lexAddMessage msg lx
                        Nothing -> lx
    check lx = lx

    submap :: C.Lexmap -> B.Ab (C.Lexmap, [C.Rody C.Lexmap])
    submap lx@C.Lexmap { C.lexOperand = rod } =
        case lookup "-relmap" $ B.mapFstTo (take 7) rod of
          Nothing    -> do lxs <- slot lx   -- no submaps
                           Right (lx, lxs)
          Just trees -> do ws   <- withVars rod
                           subs <- mapM (lexmap . B.singleton) $ withTrees ws trees
                           Right ( lx { C.lexSubmap = map fst subs }
                                 , concatMap snd subs )

    slot :: C.Lexmap -> B.Ab [C.Rody C.Lexmap]
    slot lx | C.lexType lx /= C.LexmapDerived = Right []
            | otherwise
                = let n   = C.lexOpName  lx
                      rod = C.lexOperand lx
                      sub = C.lexSubmap  lx
                  in B.abortableFrom "slot" lx $ case lookup n derives of
                      Nothing -> B.concatMapM slot sub
                      Just (trees, rodmap) ->
                            do rod2       <- C.rodmapRun rodmap rod
                               trees2     <- C.slotTrees gslot rod2 trees
                               (lx2, lxs) <- lexmap trees2
                               sub2       <- B.concatMapM slot sub
                               Right $ ((n, rod), lx2) : lxs ++ sub2

    withTrees :: [String] -> B.Map [B.TokenTree]
    withTrees ws = map loop where
        loop (B.TreeB t p trees) = B.TreeB t p $ map loop trees
        loop (B.TreeL (B.TWord p 0 w)) | w `elem` ws = B.TreeL (B.TWord p 3 w)
        loop tree = tree

    withVars :: [B.NamedTrees] -> B.Ab [String]
    withVars rod =
        case lookup "-with" rod of
          Nothing -> Right []
          Just ws -> withNames ws

    withNames :: [B.TokenTree] -> B.Ab [String]
    withNames ws = do ts <- withTerms ws
                      Right $ map snd ts

-- | Parse @-with@ operand.
withTerms :: [B.TokenTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ [n]) :
          B.TreeL (B.TWord _ 0 v) : xs)  =  next (n, v) xs
    loop (B.TreeL (B.TTerm _ [n]) : xs)  =  next (n, n) xs
    loop (B.TreeL (B.TWord _ 0 v) : xs)  =  next (v, v) xs
    loop [] = Right []
    loop _  = Message.reqTermName

    next p xs = do xs' <- loop xs
                   Right $ p : xs'


-- ----------------------  Generic relmap

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

consRelmap :: C.Global c -> [B.Named (C.RopCons c)] -> ConsRelmap c
consRelmap global conses = relmap where
    relmap lx =
        let rop  = C.lexOpName  lx
            rod  = C.lexOperand lx
            lxs  = C.lexSubmap  lx
        in case C.lexType lx of
             C.LexmapWith    -> Right $ C.RelmapLink lx rop rod
             C.LexmapDerived -> Right $ C.RelmapLink lx rop rod
             C.LexmapBase    -> case lookup rop conses of
                                 Nothing   -> Message.unkRelmap rop
                                 Just cons -> B.abortableFrom "relmap" lx $
                                              do rmaps <- mapM relmap lxs
                                                 cons $ C.RopUse global lx rmaps


-- ----------------------
-- $ConstructionProcess
--
--  Construction process of lex relmaps from source trees.
--
--  [@\[TokenTree\] -> \[\[TokenTree\]\]@]
--     Divide list of 'B.TokenTree' by vertical bar (@|@).
--
--  [@\[\[TokenTree\]\] -> \[Lexmap\]@]
--     Construct each 'C.Lexmap' from lists of 'B.TokenTree'.
--     When there are submaps in token trees,
--     constructs 'C.Lexmap' recursively.
--
--  [@\[Lexmap\] -> Lexmap@]
--     Wrap list of 'C.Lexmap' into one 'C.Lexmap'
--     that has these relmaps in 'C.lexSubmap'.
--

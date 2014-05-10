{-# OPTIONS_GHC -Wall #-}

-- | Construct 'C.Lexmap' and 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
( -- * Data type
  RelmapCons (..),
  ConsLexmap,
  ConsLexmapBody,
  ConsRelmap,

  -- * Function
  relmapCons,
  slotTrees,
  withTerms,

  -- * Construction process
  -- $ConstructionProcess
) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Relmap.Lexmap    as C
import qualified Koshucode.Baala.Core.Relmap.Operand   as C
import qualified Koshucode.Baala.Core.Relmap.Operator  as C
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
type ConsLexmap = [B.NamedTrees] -> [B.NamedTrees] -> ConsLexmapBody

-- | Construct lexmap and its submaps from source of lexmap
type ConsLexmapBody = [B.TokenTree] -> B.Ab (C.Lexmap, [C.Rody C.Lexmap])

consLexmap :: [B.Named C.RodSorter] -> ConsLexmap
consLexmap sorters gslot tokmaps = lexmap where

    lexmap :: ConsLexmapBody
    lexmap source =
        B.abortableTrees "lexmap" source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TWord _ 0 _) : trees)] -> derived rop trees
           [(B.TreeL rop@(B.TWord _ 3 _) : trees)] -> user C.LexmapWith rop trees
           [[B.TreeB 1 _ trees]] -> lexmap trees
           [[B.TreeB _ _ _]]     -> Message.adlib "bracket"
           [_]                   -> Message.unkRelmap "?"
           trees2                -> derived (B.tokenWord "append") $ map B.treeWrap trees2

    derived :: B.Token -> ConsLexmapBody
    derived rop trees =
        let name = B.tokenContent rop
        in case lookup name tokmaps of
          Just _  -> user C.LexmapDerived rop trees
          Nothing -> base name rop trees

    base :: String -> B.Token -> ConsLexmapBody
    base name rop trees =
        case lookup name sorters of
          Nothing     ->  user C.LexmapDerived rop trees
          Just sorter ->  do rod <- sorter trees
                             submap $ cons C.LexmapBase rop rod trees

    user :: C.LexmapType -> B.Token -> ConsLexmapBody
    user C.LexmapWith rop [] = submap $ cons C.LexmapWith rop [] []
    user C.LexmapWith _ _ = Message.extraOperand
    user ty rop trees = do rod <- C.rodBranch trees
                           submap $ cons ty rop rod trees

    cons :: C.LexmapType -> B.Token -> [B.NamedTrees] -> [B.TokenTree] -> C.Lexmap
    cons ty rop rod trees = C.Lexmap { C.lexType    = ty
                                     , C.lexOpToken = rop
                                     , C.lexOperand = (("@operand", trees) : rod)
                                     , C.lexSubmap  = [] }

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
                 = let n   = C.lexOpText  lx
                       rod = C.lexOperand lx
                       sub = C.lexSubmap  lx
                   in B.abortableFrom "slot" lx $ case lookup n tokmaps of
                        Nothing    -> B.concatMapM slot sub
                        Just trees -> do trees2     <- slotTrees gslot rod trees
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


-- ----------------------  Slot substitution

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



-- ----------------------  Generic relmap

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

consRelmap :: C.Global c -> [B.Named (C.RopCons c)] -> ConsRelmap c
consRelmap global conses = relmap where
    relmap lx =
        let rop  = C.lexOpText  lx
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

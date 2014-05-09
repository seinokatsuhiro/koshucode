{-# OPTIONS_GHC -Wall #-}

-- | Construct 'C.Lexmap' and 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
( -- * Data type
  RelmapCons (..),
  ConsLexmap,
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
import qualified Koshucode.Baala.Core.Message          as Message


-- ----------------------  Constructions

-- | Constructor of lexmap and relmap.
data RelmapCons c = RelmapCons
      { consLexmap :: ConsLexmap
      , consRelmap :: ConsRelmap c
      }

instance Show (RelmapCons c) where
    show _ = "RelmapCons <lex> <full>"

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: C.Global c -> RelmapCons c
relmapCons g = make $ unzip $ map pair $ C.globalRops g where
    make (l, r) = RelmapCons (makeConsLexmap l) (makeConsRelmap g r)
    pair (C.Rop n _ sorter cons _) = ((n, sorter), (n, cons))



-- ----------------------  Lexical relmap

-- | First step of constructing relmap,
--   make lexmap from source of relmap operator.
type ConsLexmap = [B.NamedTrees] -> [String] -> [B.TokenTree] -> B.Ab C.Lexmap

makeConsLexmap :: [B.Named C.RodSorter] -> ConsLexmap
makeConsLexmap sorters tokmaps locals = lexmap where
    lexmap :: [B.TokenTree] -> B.Ab C.Lexmap
    lexmap source =
        B.abortableTrees "lexmap" source $
         case B.divideTreesByBar source of
           [(B.TreeL rop@(B.TWord _ 0 _) : trees)] -> find1 rop trees
           [[B.TreeB 1 _ trees]] -> lexmap trees
           [[B.TreeB _ _ _]]     -> Message.adlib "bracket"
           [_]                   -> Message.unkRelmap "?"
           trees2                -> find1 (B.tokenWord "append") $ map B.treeWrap trees2

    find1 :: B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    find1 rop trees =
        let name = B.tokenContent rop
        in if elem name locals
           then user C.LexmapLocal rop trees
           else find2 name rop trees

    find2 :: String -> B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    find2 name rop trees =
        case lookup name tokmaps of
          Just _  -> user C.LexmapUser rop trees
          Nothing -> find3 name rop trees

    find3 :: String -> B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    find3 name rop trees =
        case lookup name sorters of
          Nothing     ->  user C.LexmapUser rop trees
          Just sorter ->  do rod <- sorter trees
                             submap $ cons C.LexmapSystem rop rod trees

    user :: C.LexmapType -> B.Token -> [B.TokenTree] -> B.Ab C.Lexmap
    user C.LexmapLocal rop [] = submap $ cons C.LexmapLocal rop [] []
    user C.LexmapLocal _ _ = Message.extraOperand
    user ty rop trees = do rod <- C.rodBranch trees
                           submap $ cons ty rop rod trees

    cons :: C.LexmapType -> B.Token -> [B.NamedTrees] -> [B.TokenTree] -> C.Lexmap
    cons ty rop rod trees = C.Lexmap { C.lexType    = ty
                                     , C.lexOpToken = rop
                                     , C.lexOperand = (("@operand", trees) : rod)
                                     , C.lexSubmap  = [] }

    submap :: B.AbMap C.Lexmap
    submap lx@C.Lexmap { C.lexOperand = rod } =
        case lookup "-relmap" $ B.mapFstTo (take 7) rod of
          Nothing    -> Right lx   -- no submaps
          Just trees -> do lexmap2 <- with rod
                           subs    <- mapM (lexmap2 . B.singleton) trees
                           Right $ lx { C.lexSubmap = subs }

    with :: [B.NamedTrees] -> B.Ab ([B.TokenTree] -> B.Ab C.Lexmap)
    with rod =
        case lookup "-with" rod of
          Nothing -> Right lexmap
          Just ws -> do ns <- withNames ws
                        Right $ makeConsLexmap sorters tokmaps $ ns ++ locals

    withNames :: [B.TokenTree] -> B.Ab [String]
    withNames ws =
        do ts <- withTerms ws
           Right $ map snd ts

withTerms :: [B.TokenTree] -> B.Ab [B.Terminal String]
withTerms = loop where
    loop (B.TreeL (B.TTerm _ [n]) : B.TreeL (B.TWord _ 0 v) : xs) = next (n, v) xs
    loop (B.TreeL (B.TTerm _ [n]) : xs)                           = next (n, n) xs
    loop (B.TreeL (B.TWord _ 0 v) : xs)                           = next (v, v) xs
    loop [] = Right []
    loop _  = Message.reqTermName

    next p xs = do xs' <- loop xs
                   Right $ p : xs'


-- ----------------------  Generic relmap

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

makeConsRelmap :: C.Global c -> [B.Named (C.RopCons c)] -> ConsRelmap c
makeConsRelmap global conses = relmap where
    relmap lx =
        let rop  = C.lexOpText  lx
            rod  = C.lexOperand lx
            lxs  = C.lexSubmap  lx
        in case C.lexType lx of
             C.LexmapLocal  -> Right $ C.RelmapLink lx rop rod
             C.LexmapUser   -> Right $ C.RelmapLink lx rop rod
             C.LexmapSystem -> case lookup rop conses of
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

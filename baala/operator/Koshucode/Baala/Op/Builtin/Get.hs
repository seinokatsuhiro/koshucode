{-# OPTIONS_GHC -Wall #-}

-- | Extract suboperand from use of relmap

module Koshucode.Baala.Op.Builtin.Get
( -- * Datatype
  RopGet,
  getMaybe, getOption,
  getTree, getTrees,
  getWordTrees,

  -- * Relmap
  getRelmap,
  getRelmaps,
  getRelmapOption,

  -- * Term
  getTerm,
  getTerms,
  getTermPairs,
  getTermTrees,

  -- * Basic type
  getSwitch,
  getWord,
  getInt,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Op.Builtin.Term as Op
import qualified Koshucode.Baala.Op.Message      as Message



-- ---------------------- Utility

ab :: [B.TokenTree] -> B.Map (B.Ab b)
ab = B.abortableTrees "@operand"

lookupOperand :: String -> C.RopUse c -> Maybe [B.TokenTree]
lookupOperand name = lookup name . C.lexOperand . C.ropLexmap

getAbortable :: ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortable f u name =
    do trees <- getTrees u name
       ab trees $ f trees

getAbortableOption :: b -> ([B.TokenTree] -> B.Ab b) -> RopGet c b
getAbortableOption y f u name =
    do m <- getMaybe getTrees u name
       case m of
         Nothing    -> Right y
         Just trees -> ab trees $ f trees


-- ----------------------  Datatype

type RopGet c b
    = C.RopUse c    -- ^ Use of relmap operator
    -> String       -- ^ Name of suboperand, e.g., @\"-term\"@
    -> B.Ab b       -- ^ Suboperand

getMaybe :: RopGet c b -> RopGet c (Maybe b)
getMaybe get u name =
    case lookupOperand name u of
      Nothing -> Right Nothing
      Just _  -> Right . Just =<< get u name

getOption :: b -> RopGet c b -> RopGet c b
getOption y get u name =
    case lookupOperand name u of
      Nothing -> Right y
      Just _  -> get u name

getTrees :: RopGet c [B.TokenTree]
getTrees u name =
    case lookupOperand name u of
      Just trees -> Right trees
      Nothing    -> Message.noOperand

getTree :: RopGet c B.TokenTree
getTree u name =
    do trees <- getTrees u name
       Right $ B.treeWrap trees

getWordTrees :: RopGet c [B.Named B.TokenTree]
getWordTrees u name =
    case lookupOperand name u of
      Just trees -> wordTrees trees
      Nothing    -> Message.noOperand

wordTrees :: [B.TokenTree] -> B.Ab [B.Named B.TokenTree]
wordTrees []  = Right []
wordTrees [_] = Message.unexpOperand "Require word and tree"
wordTrees (w : tree : xs) =
    do w'  <- word w
       xs' <- wordTrees xs
       Right $ (w', tree) : xs'

word :: B.TokenTree -> B.Ab String
word (B.TreeL (B.TWord _ _ w)) = Right w
word _ = Message.unexpOperand "Require one word"


-- ----------------------  Relmap

-- | Get a relmap from operator use.
--
--   > consMeet :: (Ord c) => RopCons c
--   > consMeet u = do
--   >   m <- getRelmap u
--   >   Right $ relmapMeet u m
getRelmap :: C.RopUse c -> B.Ab (C.Relmap c)
getRelmap u =
    do ms    <- getRelmaps u
       trees <- getTrees   u "-relmap"
       ab trees $ case ms of
         [m] -> Right m
         _   -> Message.unexpOperand "Require one relmap"

-- | Get relmaps from operator use.
getRelmaps :: C.RopUse c -> B.Ab [C.Relmap c]
getRelmaps = Right . C.ropSubrelmap

getRelmapOption :: C.RopUse c -> C.Relmap c -> B.Ab (C.Relmap c)
getRelmapOption u rmapDefault =
    case getRelmap u of
      Right rmap -> Right rmap
      Left _     -> Right rmapDefault


-- ----------------------  Term

-- | Get a term name from named operand.
getTerm :: RopGet c B.TermName
getTerm = getAbortable get where
    get [x] = Op.termName x
    get _   = Message.unexpOperand "Require one term"

-- | Get list of term names from named operand.
getTerms :: RopGet c [B.TermName]
getTerms = getAbortable Op.termNames

-- | Get list of term-name pairs from named operand.
getTermPairs :: RopGet c [(B.TermName, B.TermName)]
getTermPairs = getAbortable Op.termNamePairs

getTermTrees :: RopGet c [B.Named B.TokenTree]
getTermTrees = getAbortable Op.termTreePairs


-- ----------------------  Basic type

getSwitch :: C.RopUse c -> String -> B.Ab Bool
getSwitch u name = getAbortableOption False get u name where
    get [] = Right True
    get _  = Message.unexpOperand $ "Just type only " ++ name

-- | Get word from named operand.
--
--   > consXxx :: RopCons c
--   > consXxx u = do
--   >   sign <- getWord u "-sign"
--   >   ...
getWord :: RopGet c String
getWord = getAbortable get where
    get [B.TreeL (B.TWord _ _ s)] = Right s
    get _ = Message.unexpOperand "Require one word"

getInt :: RopGet c Int
getInt = getAbortable get where
    get [B.TreeL (B.TWord _ _ i)] = Right (read i :: Int)
    get _ = Message.unexpOperand "Require one integer"


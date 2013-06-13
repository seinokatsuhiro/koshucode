{-# OPTIONS_GHC -Wall #-}

-- | Operand patterns

module Koshucode.Baala.Minimal.Relmap.Operand
( relmaps
, OperandPattern (..)
, MinimalOperand (..)
) where
import Koshucode.Baala.Base.Struct

-- | Make relmap implementations.
relmaps
    :: (OperandPattern p)
    => [(String, p, RelmapFullCons v, [String])] -- ^ Operator implementations
    -> [RelmapImplement v] -- ^ Result
relmaps = map f where
    f (op, opd, full, usage) =
        let parser = operandParser opd
        in RelmapImplement op parser full usage

-- | Class for operand pattern.
class OperandPattern p where
    operandParser :: p -> OperandParser



-- ----------------------  Operand patterns

-- | 'OperandPattern' for minimal operators
data MinimalOperand
    = LikeConst   -- ^ No operand
    | LikeMeet    -- ^ Relmap and maybe shared terms
    | LikeNone    -- ^ No operand
    | LikePick    -- ^ List of present terms
    | LikeRename  -- ^ List of new term and present term
    | LikeSource  -- ^ Relsign and list of terms
      deriving (Show, Eq, Enum)

instance OperandPattern MinimalOperand where
    operandParser LikeConst  = terms
    operandParser LikeMeet   = rels
    operandParser LikeNone   = terms
    operandParser LikePick   = terms
    operandParser LikeRename = terms
    operandParser LikeSource = src

terms :: OperandParser
terms xs = [("term", xs)]

rels :: OperandParser
rels xs = [("relmap", xs)]

src :: OperandParser
src (s:xs) = [("sign", [s]), ("term", xs)]
src _      = []

--operandChunks :: [TokenTree] -> [Named [TokenTree]]

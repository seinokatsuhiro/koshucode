{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Operand
( VanillaOperand (..)
) where
import qualified Koshucode.Baala.Minimal as M
import Koshucode.Baala.Base.Struct

-- | 'M.OperandPattern' for relational operations.
data VanillaOperand
    = LikeHold    -- ^ Boolean expression
    | LikeVal     -- ^ List of new term and expression
    | LikeMeet    -- ^ Relmap and maybe shared terms
    | LikeSource  -- ^ Relsign and list of terms
      deriving (Show, Eq, Enum)

instance M.OperandPattern VanillaOperand where
    operandParser LikeHold   = terms
    operandParser LikeVal    = terms
    operandParser LikeMeet   = M.operandParser M.LikeMeet
    operandParser LikeSource = M.operandParser M.LikeSource

terms :: OperandParser
terms xs = [("term", xs)]


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Implement
( -- * Implement
  OpImplement (..),
  OpParser,
  OpParser',

  -- * Constructor
  Relop,
  OpUse (..),

  -- * Relmap basis
  relmapSource,
  relmapConst,
  relmapAlias,
  relmapCalc,
  relmapConfl,

) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Relmap.Relmap
import Koshucode.Baala.Core.Relmap.HalfRelmap



-- ----------------------  Implement

{-| Implementation of relmap operator.
    It consists of
    (1) operator name,
    (2) operand parser,
    (3) constructor of operator, and
    (4) usage of operator. -}
data OpImplement v =
    OpImplement String OpParser (Relop v) [String]
    
{-| Parser for operand of relational operator.
    This parsers docompose operand trees,
    and give a name to suboperand. -}
type OpParser
    =  [TokenTree]          -- ^ Unparsed operand
    -> [Named [TokenTree]]  -- ^ Parsed operand

type OpParser'
    =  [Named [TokenTree]]  -- ^ Partially parsed operand
    -> [Named [TokenTree]]  -- ^ Full parsed operand



-- ----------------------  Constructor

{-| Constructor of relational operator 'Relmap'.
    'Relmap' is constructed from 'HalfRelmap' and subrelmaps in it. -}
type Relop v = OpUse v -> AbortOr (Relmap v)

{-| Use of operator -}
data OpUse v = OpUse {
      opHalf    :: HalfRelmap   -- ^ Syntactic data of operator use
    , opSubmap  :: [Relmap v]   -- ^ Subrelmaps
    } deriving (Show)




-- ----------------------  Relmap

{-| Retrieve relation from dataset. -}
relmapSource
    :: OpUse v     -- ^ Use of operator
    -> String      -- ^ Operator name
    -> [String]    -- ^ List of term names
    -> (Relmap v)  -- ^ Result relmap
relmapSource use = RelmapSource $ opHalf use

{-| Constant relmap. -}
relmapConst
    :: OpUse v     -- ^ Use of operator
    -> String      -- ^ Operator name
    -> Rel v       -- ^ Constant relation
    -> Relmap v    -- ^ Result relmap
relmapConst use = RelmapConst $ opHalf use

{-| Alias relmap. -}
relmapAlias
    :: OpUse v     -- ^ Use of operator
    -> Relmap v    -- ^ 
    -> Relmap v    -- ^ Result relmap
relmapAlias use = RelmapAlias $ opHalf use

{-| Make a non-confluent relmap. -}
relmapCalc
    :: OpUse v      -- ^ Use of operator
    -> String       -- ^ Operator name
    -> RelmapSub v  -- ^ Calculation of operation
    -> Relmap v     -- ^ Result relmap
relmapCalc use op sub = relmapConfl use op sub []

{-| Make a confluent relmap. -}
relmapConfl
    :: OpUse v      -- ^ Use of operator
    -> String       -- ^ Operator name
    -> RelmapSub v  -- ^ Calculation of operation
    -> [Relmap v]   -- ^ Subrelmaps
    -> Relmap v     -- ^ Result relmap
relmapConfl use = RelmapCalc $ opHalf use


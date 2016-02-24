{-# OPTIONS_GHC -Wall #-}

-- | Rops and cops.

module Koshucode.Baala.Cop.Bundle
  ( baalaCops,
    baalaInfix,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as D
import qualified Koshucode.Baala.Cop.Arith  as Cop
import qualified Koshucode.Baala.Cop.List   as Cop
import qualified Koshucode.Baala.Cop.Logic  as Cop
import qualified Koshucode.Baala.Cop.Misc   as Cop
import qualified Koshucode.Baala.Cop.Order  as Cop
import qualified Koshucode.Baala.Cop.Time   as Cop
import qualified Koshucode.Baala.Cop.Type   as Cop

-- | Term-content operators.
baalaCops :: (D.CContent c) => [D.Cop c]
baalaCops = concat [ Cop.copsArith
                   , Cop.copsLogic
                   , Cop.copsList
                   , Cop.copsMisc
                   , Cop.copsOrder
                   , Cop.copsTime
                   , Cop.copsType ]

-- | Height table.
baalaInfix :: [B.Named B.InfixHeight]
baalaInfix = htab where
    h ! name = (name, Right h)

    htab =
        [ 9 ! "then"
        , 9 ! "when"
        , 9 ! "unless"
        , 9 ! "is"
        , 9 ! "of"
        , 9 ! "to"

        , 8 ! "or"

        , 7 ! "and"

        , 6 ! "in"
        , 6 ! "="     -- equal
        , 6 ! "<>"    -- not equal
        , 6 ! "<"     -- less than
        , 6 ! ">"     -- greater than
        , 6 ! "<="    -- less than or equal
        , 6 ! ">="    -- greater than or equal
        , 6 ! "=*"    -- begin with
        , 6 ! "*="    -- end with
        , 6 ! "*=*"   -- contain

        , 2 ! "+"     -- add
        , 2 ! ".+"    -- left add
        , 2 ! "+."    -- right add
        , 2 ! ".+."   -- strict add
        , 2 ! "-"
        , 2 ! "++"
        , 2 ! "intersect"
        , 2 ! "minus"

        , 1 ! "*"
        , 1 ! "/"
        , 1 ! "quo"
        , 1 ! "rem"

        , ("<left-1>"  , Left 1)
        , ("<right-1>" , Right 1)
        ]


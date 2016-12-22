{-# OPTIONS_GHC -Wall #-}

-- | Definition of relmap operators.

module Koshucode.Baala.Rop.Base.Define
  ( rops,
    ropAlias,
    newCheck,
    preCheck,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base.Message  as Msg

-- | Make implementations of relmap operators.
rops :: C.RopGroup     -- ^ Operator group
     -> [(C.RopCons c, [(K.AttrUsage, String)])]
                       -- ^ Constructor and list of usage and layout
     -> [C.Rop c]      -- ^ Operator list
rops group def = make <$> (K.toAttrLayout K.<$$> def) where
    make (cons, layout) =
        let usage  = K.attrUsageString layout
            name   = head $ words usage
            sorter = K.attrParaBy layout
        in C.Rop { C.ropName   = name
                 , C.ropGroup  = group
                 , C.ropAttr   = layout
                 , C.ropParaze = sorter
                 , C.ropCons   = cons }

-- | Add aliases of relmap operator.
ropAlias
    :: [(C.RopName, C.RopName)]  -- ^ Alias and original rop names
    -> K.Map [C.Rop c]           -- ^ Original rop list to add aliases.
ropAlias alias rs = foldr ropAliasAdd rs alias

-- | Add alias of relmap operator.
ropAliasAdd :: (C.RopName, C.RopName) -> K.Map [C.Rop c]
ropAliasAdd (name, orig) = loop where
    loop [] = []
    loop (r : rs) | C.ropName r == orig  = r { C.ropName = name } : r : rs
                  | otherwise            = r : loop rs
    
-- | New term check.
--
--   * Target terms are not present in input terms,
--     e.g., abort if targets are __\/a__ and inputs are __\/a \/b__.
--   * Target terms are not duplicated,
--     e.g., abort if targets are __\/a \/a__ and inputs are __\/a \/b__.
--
newCheck :: K.TermPicker c -> K.MapAb a
newCheck pk body
    | K.preTermsExist pk   = Msg.reqNewTerm pk input
    | K.duplicated target  = Msg.dupTerm target
    | otherwise            = body
    where target = K.pkLNames pk
          input  = K.pkRNames pk

-- | Term presence check.
--
--   * Terget terms are present in input terms,
--     e.g., abort if targets are __\/c__ and inputs are __\/a \/b__.
--   * Target terms are not duplicated,
--     e.g., abort if targets are __\/a \/a__ and inputs are __\/a \/b__.
--
preCheck :: K.TermPicker c -> K.MapAb a
preCheck pk body
    | K.newTermsExist pk   = Msg.newTerm pk input
    | K.duplicated target  = Msg.dupTerm target
    | otherwise            = body
    where target = K.pkLNames pk
          input  = K.pkRNames pk


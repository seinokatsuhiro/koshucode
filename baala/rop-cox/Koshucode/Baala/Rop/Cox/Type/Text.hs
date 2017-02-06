{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for text type.

module Koshucode.Baala.Rop.Cox.Type.Text
  ( ropsTypeText,
    -- * trim
    consTrim, relmapTrim, relkitTrim,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg

-- | Implementation of relational operators.
ropsTypeText :: (K.CContent c) => [C.Rop c]
ropsTypeText = Rop.rops "type"
    [ consTrim
      K.& [ "trim /P ... [-both /P ...] [-begin /P ...] [-end /P ...]"
            K.& "-term* . -both? -begin? -end?" ]
    ]

-- ----------------------  trim

-- | [trim \/P ... -both \/P ... -begin \/P ... -end \/P ...]
--     Trim spaces of text content of term \/P ....
--
consTrim :: (K.CContent c) => C.RopCons c
consTrim med =
    do ns   <- Rop.getTerms med "-term"
       both <- Rop.getOpt [] Rop.getTerms med "-both"
       beg  <- Rop.getOpt [] Rop.getTerms med "-begin"
       end  <- Rop.getOpt [] Rop.getTerms med "-end"
       Right $ relmapTrim med (ns ++ both, beg, end)

-- | Create @trim@ relmap.
relmapTrim :: (K.CContent c) => C.Intmed c -> ([K.TermName], [K.TermName], [K.TermName]) -> C.Relmap c
relmapTrim med = C.relmapFlow med . relkitTrim

-- | Create @trim@ relkit.
relkitTrim :: (K.CContent c) => ([K.TermName], [K.TermName], [K.TermName]) -> C.RelkitFlow c
relkitTrim _ Nothing = C.relkitUnfixed
relkitTrim (both, begin, end) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit
    where
      ns        = both ++ begin ++ end
      pk        = K.termPicker ns he1
      pkBoth    = K.termPicker both  he1
      pkBegin   = K.termPicker begin he1
      pkEnd     = K.termPicker end   he1
      he2       = K.headMap (K.forwardTerms pk) he1
      kit       = C.relkitLine False he2 flow
      flow cs1  = let csBoth  = K.cTrimBoth  <$> K.pickTerms pkBoth  cs1
                      csBegin = K.cTrimBegin <$> K.pickTerms pkBegin cs1
                      csEnd   = K.cTrimEnd   <$> K.pickTerms pkEnd   cs1
                  in csBoth ++ csBegin ++ csEnd ++ K.cutTerms pk cs1


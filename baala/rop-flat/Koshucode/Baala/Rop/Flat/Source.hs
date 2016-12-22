{-# OPTIONS_GHC -Wall #-}

-- | Data source.

module Koshucode.Baala.Rop.Flat.Source
  ( ropsSource,  
    -- * dee & dum
    consDee, consDum,  
    -- * empty
    consEmpty, relmapEmpty,  
    -- * source
    consSource,  
    -- * source-term
    consSourceTerm, relmapSourceTerm,
  ) where

import qualified Koshucode.Baala.DataPlus     as K
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Rop


-- | Implementation of relational operators.
ropsSource :: (K.CContent c) => [C.Rop c]
ropsSource = Rop.rops "source"
    [ consDee         K.& [ "dee"              K.&  "" ]
    , consDum         K.& [ "dum"              K.&  "" ]
    , consEmpty       K.& [ "empty /N ..."     K.&  "-term*" ]
    , consSource      K.& [ "source P /N ..."  K.&  "-pattern -term*" ]
    , consSourceTerm  K.& [ "source-term P R"  K.&  "-pattern -relmap/" ]
    ]


-- ----------------------  empty

-- | __empty \/N ...__
--
--   Output an empty relation which has terms /\/N .../.
--
consEmpty :: C.RopCons c
consEmpty med =
    do ns <- Rop.getTerms med "-term"
       Right $ relmapEmpty med ns

-- | Create @empty@ relmap.
relmapEmpty :: C.Intmed c -> [K.TermName] -> C.Relmap c
relmapEmpty med = C.relmapFlow med . relkitEmpty

-- | Create @empty@ relkit.
relkitEmpty :: [K.TermName] -> C.RelkitFlow c
relkitEmpty ns _ = Right $ C.relkit he2 $ C.RelkitConst [] where
    he2 = Just $ K.headFrom ns


-- ----------------------  source

-- | __source C \/N...__
--
--   Read relation with terms /\/N.../ from judges of /C/.
--
consSource :: C.RopCons c
consSource med =
  do pattern  <- Rop.getWord  med "-pattern"
     terms    <- Rop.getTerms med "-term"
     Right $ C.relmapSource med pattern terms


-- ----------------------  source-term

-- | __source-term C R__
consSourceTerm :: C.RopCons c
consSourceTerm med =
  do pat   <- Rop.getWord   med "-pattern"
     rmap  <- Rop.getRelmap med "-relmap"
     Right $ relmapSourceTerm med pat rmap

-- | Create @source-term@ relmap.
relmapSourceTerm :: C.Intmed c -> String -> K.Map (C.Relmap c)
relmapSourceTerm med pat = C.relmapBinary med $ relkitSourceTerm pat

-- | Create @source-term@ relkit.
relkitSourceTerm :: String -> C.RelkitBinary c
relkitSourceTerm cl (C.RelkitOutput he2 _) _ = Right kit3 where
    kit3   = C.relkitJust he2 $ C.RelkitSource cl ns2
    ns2    = K.getTermNames he2
relkitSourceTerm _  _ _ = Right C.relkitNothing


-- ----------------------  dee & dum

-- | __dee__
--
--   Output the nullary full relation.
--
consDee :: C.RopCons c
consDee med = Right $ C.relmapConst med K.reldee

-- | __dum__
--
--   Output the nullary empty relation.
--
consDum :: C.RopCons c
consDum med = Right $ C.relmapConst med K.reldum


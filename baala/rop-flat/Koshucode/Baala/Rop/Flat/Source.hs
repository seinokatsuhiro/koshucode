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

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Op


-- | Implementation of relational operators.
ropsSource :: (D.CContent c) => [C.Rop c]
ropsSource = Op.ropList "source"
    --        CONSTRUCTOR     USAGE               ATTRIBUTE
    [ Op.def  consDee         "dee"               ""
    , Op.def  consDum         "dum"               ""
    , Op.def  consEmpty       "empty /N ..."      "-term*"
    , Op.def  consSource      "source P /N ..."   "-pattern -term*"
    , Op.def  consSourceTerm  "source-term P R"   "-pattern -relmap/"
    ]


-- ----------------------  empty

-- | __empty \/N ...__
--
--   Output an empty relation which has terms /\/N .../.
--
consEmpty :: C.RopCons c
consEmpty med =
    do ns <- Op.getTerms med "-term"
       Right $ relmapEmpty med ns

-- | Create @empty@ relmap.
relmapEmpty :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapEmpty med = C.relmapFlow med . relkitEmpty

-- | Create @empty@ relkit.
relkitEmpty :: [S.TermName] -> C.RelkitFlow c
relkitEmpty ns _ = Right $ C.relkit he2 $ C.RelkitConst [] where
    he2 = Just $ D.headFrom ns


-- ----------------------  source

-- | __source C \/N...__
--
--   Read relation with terms /\/N.../ from judges of /C/.
--
consSource :: C.RopCons c
consSource med =
  do pattern  <- Op.getWord  med "-pattern"
     terms    <- Op.getTerms med "-term"
     Right $ C.relmapSource med pattern terms


-- ----------------------  source-term

-- | __source-term C R__
consSourceTerm :: C.RopCons c
consSourceTerm med =
  do pat   <- Op.getWord   med "-pattern"
     rmap  <- Op.getRelmap med "-relmap"
     Right $ relmapSourceTerm med pat rmap

-- | Create @source-term@ relmap.
relmapSourceTerm :: C.Intmed c -> String -> O.Map (C.Relmap c)
relmapSourceTerm med pat = C.relmapBinary med $ relkitSourceTerm pat

-- | Create @source-term@ relkit.
relkitSourceTerm :: String -> C.RelkitBinary c
relkitSourceTerm _   (C.Relkit _ Nothing    _) _ = Right C.relkitNothing
relkitSourceTerm pat (C.Relkit _ (Just he2) _) _ = Right kit3 where
    kit3   = C.relkitJust he2 $ C.RelkitSource pat ns2
    ns2    = D.getTermNames he2


-- ----------------------  dee & dum

-- | __dee__
--
--   Output the nullary full relation.
--
consDee :: C.RopCons c
consDee med = Right $ C.relmapConst med D.reldee

-- | __dum__
--
--   Output the nullary empty relation.
--
consDum :: C.RopCons c
consDum med = Right $ C.relmapConst med D.reldum


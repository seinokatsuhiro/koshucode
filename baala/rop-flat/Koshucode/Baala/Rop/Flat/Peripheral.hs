{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Type-specific operators.

module Koshucode.Baala.Rop.Flat.Peripheral
  ( ropsPeripheral,
    -- * RDF
    consRdf,
    -- * tie
    consTie, relmapTie, relkitTie,
    -- * untie
    consUntie, relmapUntie, relkitUntie,
    -- * term-name
    consTermName, relmapTermName, relkitTermName,
    -- * time
    consToday, consNow,
    -- * add term
    consAdd1, relmapAdd1, relkitAdd1,
  ) where

import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Flat.Term      as Rop
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg

-- | Implementation of relational operators.
ropsPeripheral :: (K.CContent c) => [C.Rop c]
ropsPeripheral = Rop.rops "peripheral"
    [ consNow       K.& [ "now /N"                K.& "local : -term"
                        , "now /N -utc"           K.& "utc : -term . -utc"
                        , "now /N -zoned"         K.& "zoned : -term . -zoned" ]
    , consRdf       K.& [ "rdf P /S /O"           K.& "-pattern -term*" ]
    , consTermName  K.& [ "term-name /N"          K.& "-term" ]
    , consTie       K.& [ "tie /P ... -to N"      K.& "-term* . -to" ]
    , consToday     K.& [ "today /N"              K.& "-term" ]
    , consUntie     K.& [ "untie /P -only /P ..." K.& "-from . -only" ]
    ]

-- ----------------------  RDF

-- | __rdf P \/S \/O__
consRdf :: C.RopCons c
consRdf med =
    do sign  <- Rop.getWord  med "-pattern"
       [s,o] <- Rop.getTerms med "-term"
       Right $ C.relmapSource med sign ["/s", "/o"] K.++
               Rop.relmapRename med [(s,"/s"), (o,"/o")]



-- ----------------------  tie

-- | __tie \/P ... -to \/N__
consTie :: (K.CTie c) => C.RopCons c
consTie med =
  do ns <- Rop.getTerms med "-term"
     to <- Rop.getTerm  med "-to"
     Right $ relmapTie med (ns, to)

-- | Create @tie@ relmap.
relmapTie :: (K.CTie c) => C.Intmed c -> ([K.TermName], K.TermName) -> C.Relmap c
relmapTie med = C.relmapFlow med . relkitTie

-- | Create @tie@ relkit.
relkitTie :: (K.CTie c) => ([K.TermName], K.TermName) -> C.RelkitFlow c
relkitTie _ Nothing = Right C.relkitNothing
relkitTie (ns, to) (Just he1) = Right kit2 where
    pick    = K.pickDirect ns he1
    he2     = K.headCons to he1
    kit2    = C.relkitLinear he2 False f
    f cs1   = let tie = K.pTie $ zip ns $ pick cs1
              in tie : cs1


-- ----------------------  untie

-- | __untie \/P -to \/N ...__
consUntie :: (K.CTie c) => C.RopCons c
consUntie med =
  do from <- Rop.getTerm  med "-from"
     ns   <- Rop.getTerms med "-only"
     Right $ relmapUntie med (from, ns)

-- | Create @untie@ relmap.
relmapUntie :: (K.CTie c) => C.Intmed c -> (K.TermName, [K.TermName]) -> C.Relmap c
relmapUntie med = C.relmapFlow med . relkitUntie

-- | Create @untie@ relkit.
relkitUntie :: (K.CTie c) => (K.TermName, [K.TermName]) -> C.RelkitFlow c
relkitUntie _ Nothing = Right C.relkitNothing
relkitUntie (from, ns) (Just he1) = Right kit2 where
    pick      = K.pickDirect [from] he1
    he2       = K.headAppend ns he1
    kit2      = C.relkitAbLinear he2 False flow
    flow cs1  = do let [tie] = pick cs1
                   cs <- tiePick ns $ K.gTie tie
                   Right $ cs ++ cs1

tiePick :: [K.TermName] -> [K.Term c] -> K.Ab [c]
tiePick ns tie = mapM pick ns where
    pick n = case lookup n tie of
               Just c   ->  Right c
               Nothing  ->  Msg.adlib "no term"


-- ----------------------  term-name

-- | __term-name \/N__
consTermName :: (K.CTerm c) => C.RopCons c
consTermName med =
  do n <- Rop.getTerm med "-term"
     Right $ relmapTermName med n

-- | Create @term-name@ relmap.
relmapTermName :: (K.CTerm c) => C.Intmed c -> K.TermName -> C.Relmap c
relmapTermName med n = C.relmapFlow med $ relkitTermName n

-- | Create @term-name@ relkit.
relkitTermName :: (K.CTerm c) => K.TermName -> C.RelkitFlow c
relkitTermName n Nothing    = Msg.noAttr $ K.termNameString n
relkitTermName n (Just he1) = Right kit2 where
    he2       = K.headFrom [n]
    kit2      = C.relkitFull he2 False flow
    flow _    = map term $ K.getTermNames he1
    term t    = [K.pTerm t]


-- ----------------------  today & now

-- | __today \/N__
--
--   Get today's time at term \/N.
--
consToday :: (K.CTime c) => C.RopCons c
consToday med =
  do n <- Rop.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     consAdd1 (n, K.pTime $ K.timeCutClock t) med

-- | [now \/N] Get current local time without time zone at term \/N.
--   [now \/N -zoned] Get current local time with time zone at term \/N.
--   [now \/N -utc] Get current UTC.
--
consNow :: (K.CTime c) => C.RopCons c
consNow med =
  do n <- Rop.getTerm med "-term"
     let tim    = C.globalTime $ C.ropGlobal med
         tag    = (`elem` Rop.getTags med)
         cons f = consAdd1 (n, K.pTime $ f tim) med
     case () of
       _ | tag "local"  -> cons K.timeLocalize
         | tag "zoned"  -> cons id
         | tag "utc"    -> cons $ K.timeAltZone $ const 0
         | otherwise    -> Msg.adlib "unknown tag"


-- ----------------------  Add term

-- | Add single constant term to each tuples.
consAdd1 :: K.Term c -> C.RopCons c
consAdd1 term med = Right $ relmapAdd1 term med

-- | Create term-adding relmap.
relmapAdd1 :: K.Term c -> C.Intmed c -> C.Relmap c
relmapAdd1 term med = C.relmapFlow med $ relkitAdd1 term

-- | Create term-adding relkit.
relkitAdd1 :: K.Term c -> Maybe K.Head -> K.Ab (C.Relkit c)
relkitAdd1 _ Nothing = Right C.relkitNothing
relkitAdd1 (n, c) (Just he1) = Right kit2 where
    he2   = K.headCons n he1
    kit2  = C.relkitLinear he2 False (c :)


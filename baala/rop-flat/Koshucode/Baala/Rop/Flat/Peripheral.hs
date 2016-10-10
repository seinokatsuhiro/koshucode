{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

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
  
    -- * today
    relmapToday, relkitToday,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Flat.Term      as Op
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg

-- | Implementation of relational operators.
ropsPeripheral :: (D.CContent c) => [C.Rop c]
ropsPeripheral = Op.ropList "peripheral"
    --       CONSTRUCTOR       USAGE                      ATTRIBUTE
    [ Op.def consRdf           "rdf P /S /O"              " -pattern -term*"
    , Op.def consTermName      "term-name /N"             "-term"
    , Op.def consTie           "tie /P ... -to N"         "-term* . -to"
    , Op.def consToday         "today /N"                 "-term"
    , Op.def consUntie         "untie /P -only /P ..."    "-from . -only"
    ]


-- ----------------------  RDF

consRdf :: C.RopCons c
consRdf med =
    do sign  <- Op.getWord  med "-pattern"
       [s,o] <- Op.getTerms med "-term"
       Right $ C.relmapSource med sign ["/s", "/o"] B.<>
               Op.relmapRename med [(s,"/s"), (o,"/o")]



-- ----------------------  tie

--    > tie /x /y /z -to /a

consTie :: (D.CTie c) => C.RopCons c
consTie med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapTie med (ns, to)

relmapTie :: (D.CTie c) => C.Intmed c -> ([S.TermName], S.TermName) -> C.Relmap c
relmapTie med = C.relmapFlow med . relkitTie

-- | Create @tie@ relkit.
relkitTie :: (D.CTie c) => ([S.TermName], S.TermName) -> C.RelkitFlow c
relkitTie _ Nothing = Right C.relkitNothing
relkitTie (ns, to) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 ns
    he2       =  D.headCons to he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs1    =  let tie = D.pTie $ zip ns $ pick cs1
                 in tie : cs1


-- ----------------------  untie

--    > untie /a -only /x /y

consUntie :: (D.CTie c) => C.RopCons c
consUntie med =
  do from <- Op.getTerm  med "-from"
     ns   <- Op.getTerms med "-only"
     Right $ relmapUntie med (from, ns)

relmapUntie :: (D.CTie c) => C.Intmed c -> (S.TermName, [S.TermName]) -> C.Relmap c
relmapUntie med = C.relmapFlow med . relkitUntie

-- | Create @untie@ relkit.
relkitUntie :: (D.CTie c) => (S.TermName, [S.TermName]) -> C.RelkitFlow c
relkitUntie _ Nothing = Right C.relkitNothing
relkitUntie (from, ns) (Just he1) = Right kit2 where
    pick      =  Op.picker he1 [from]
    he2       =  D.headAppend ns he1
    kit2      =  C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    f2 _ cs1  =  do let [tie] = pick cs1
                    cs <- tiePick ns $ D.gTie tie
                    Right $ cs ++ cs1

tiePick :: [S.TermName] -> [S.Term c] -> B.Ab [c]
tiePick ns tie = mapM pick ns where
    pick n = case lookup n tie of
               Just c   ->  Right c
               Nothing  ->  Msg.adlib "no term"


-- ----------------------  term-name

consTermName :: (D.CTerm c) => C.RopCons c
consTermName med =
  do n <- Op.getTerm med "-term"
     Right $ relmapTermName med n

relmapTermName :: (D.CTerm c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapTermName med n = C.relmapFlow med $ relkitTermName n

-- | Create @term-name@ relkit.
relkitTermName :: (D.CTerm c) => S.TermName -> C.RelkitFlow c
relkitTermName n Nothing    = Msg.noAttr n
relkitTermName n (Just he1) = Right kit2 where
    he2       = D.headFrom [n]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 _   = map term $ D.getTermNames he1
    term t    = [D.pTerm t]


-- ----------------------  today

--  today /day

consToday :: (D.CTime c) => C.RopCons c
consToday med =
  do n <- Op.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     Right $ relmapToday med (n, t)

relmapToday :: (D.CTime c) => C.Intmed c -> (S.TermName, D.Time) -> C.Relmap c
relmapToday med = C.relmapFlow med . relkitToday

-- | Create @today@ relkit.
relkitToday :: (D.CTime c) => (S.TermName, D.Time) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitToday _ Nothing = Right C.relkitNothing
relkitToday (n, t) (Just he1) = Right kit2 where
    he2   = D.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs = D.pTime t : cs


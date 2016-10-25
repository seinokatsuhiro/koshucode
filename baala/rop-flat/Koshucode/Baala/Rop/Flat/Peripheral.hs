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
  
    -- * today & now
    consToday, consNow, relmapAdd1, relkitAdd1,
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
    [ Op.def consNow           "now /N"                   "-term"
    , Op.def consRdf           "rdf P /S /O"              " -pattern -term*"
    , Op.def consTermName      "term-name /N"             "-term"
    , Op.def consTie           "tie /P ... -to N"         "-term* . -to"
    , Op.def consToday         "today /N"                 "-term"
    , Op.def consUntie         "untie /P -only /P ..."    "-from . -only"
    ]


-- ----------------------  RDF

-- | __rdf P \/S \/O__
consRdf :: C.RopCons c
consRdf med =
    do sign  <- Op.getWord  med "-pattern"
       [s,o] <- Op.getTerms med "-term"
       Right $ C.relmapSource med sign ["/s", "/o"] B.<>
               Op.relmapRename med [(s,"/s"), (o,"/o")]



-- ----------------------  tie

-- | __tie \/P ... -to \/N__
consTie :: (D.CTie c) => C.RopCons c
consTie med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapTie med (ns, to)

-- | Create @tie@ relmap.
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

-- | __untie \/P -to \/N ...__
consUntie :: (D.CTie c) => C.RopCons c
consUntie med =
  do from <- Op.getTerm  med "-from"
     ns   <- Op.getTerms med "-only"
     Right $ relmapUntie med (from, ns)

-- | Create @untie@ relmap.
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

-- | __term-name \/N__
consTermName :: (D.CTerm c) => C.RopCons c
consTermName med =
  do n <- Op.getTerm med "-term"
     Right $ relmapTermName med n

-- | Create @term-name@ relmap.
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

-- | __today \/N__
--
--   Get today's time at term \/N.
--
consToday :: (D.CTime c) => C.RopCons c
consToday med =
  do n <- Op.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     Right $ relmapAdd1 med (n, D.pTime $ D.timeOmitClock t)


-- ----------------------  now

-- | __now \/N__
--
--   Get current time at term \/N.
--
consNow :: (D.CTime c) => C.RopCons c
consNow med =
  do n <- Op.getTerm med "-term"
     let t = C.globalTime $ C.ropGlobal med
     Right $ relmapAdd1 med (n, D.pTime t)

-- | Create @today@ and @now@ relmap.
relmapAdd1 :: C.Intmed c -> (S.TermName, c) -> C.Relmap c
relmapAdd1 med = C.relmapFlow med . relkitAdd1

-- | Create @today@ and @now@ relkit.
relkitAdd1 :: (S.TermName, c) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitAdd1 _ Nothing = Right C.relkitNothing
relkitAdd1 (n, c) (Just he1) = Right kit2 where
    he2   = D.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False f2
    f2 cs = c : cs


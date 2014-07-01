{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Source
( ropsSource,

  -- * const
  consConst, relmapConst, relkitConst,
  -- $const

  -- * dee & dum
  consDee, consDum,
  -- $deedum

  -- * empty
  consEmpty, relmapEmpty,

  -- * source
  consSource,
  -- $source

  -- * source-term
  consSourceTerm, relmapSourceTerm,
  -- $source-term
) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Message  as Message


-- | Implementation of relational operators.
--
--   [@dee@]        Nullary full relation.
--
--   [@dum@]        Nullary empty relation.
--
--   [@empty@]      Make empty relation.
--
--   [@source@]     Read relation from data source.
--
ropsSource :: (C.CContent c) => [C.Rop c]
ropsSource = Op.ropList "source"
    --          CONSTRUCTOR     USAGE               ATTRIBUTE
    [ Op.ropI   consConst       "const R"           "-lit"
    , Op.ropN   consDee         "dee"
    , Op.ropN   consDum         "dum"
    , Op.ropV   consEmpty       "empty /N ..."      "-term"
    , Op.ropIV  consSource      "source P /N ..."   "-pattern -term"
    , Op.ropII  consSourceTerm  "source-term P R"   "-pattern -relmap"
    ]


-- ----------------------  const

-- $const
--
--  Same as relmap @dee@
--  
--    > const {| | |}
--
--  Same as relmap @dum@
--  
--    > const {| |}

consConst :: (C.CContent c) => C.RopCons c
consConst use =
    do tree <- Op.getTree use "-lit"
       lit  <- C.litContent tree
       case C.isRel lit of
         True  -> Right $ relmapConst use $ C.gRel lit
         False -> Message.reqRel

relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst use = C.relmapFlow use . relkitConst

relkitConst :: B.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (B.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo


-- ----------------------  empty

consEmpty :: C.RopCons c
consEmpty use =
    do ns <- Op.getTerms use "-term"
       Right $ relmapEmpty use ns

relmapEmpty :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapEmpty use = C.relmapFlow use . relkitEmpty

relkitEmpty :: [B.TermName] -> C.RelkitFlow c
relkitEmpty ns _ = Right $ C.relkit he2 $ C.RelkitConst [] where
    he2 = Just $ B.headFrom ns


-- ----------------------  source

-- $source
-- 
--  Read relation with term @\/a@ and @\/b@ constructed from judges of @P@.
-- 
--    > source P /a /b

consSource :: C.RopCons c
consSource use =
  do pattern  <- Op.getWord  use "-pattern"
     terms    <- Op.getTerms use "-term"
     Right $ C.relmapSource use pattern terms


-- ----------------------  source-term

-- $source-term
-- 
--  Define relmap @p2@ that has same terms as @p1@.
-- 
--    > p1 : source P1 /a /b
--    > p2 : source-term P2 p2

consSourceTerm :: C.RopCons c
consSourceTerm use =
  do pat   <- Op.getWord  use "-pattern"
     rmap  <- Op.getRelmap use
     Right $ relmapSourceTerm use pat rmap

relmapSourceTerm :: C.RopUse c -> String -> B.Map (C.Relmap c)
relmapSourceTerm use pat = C.relmapBinary use $ relkitSourceTerm pat

relkitSourceTerm :: String -> C.RelkitBinary c
relkitSourceTerm _   (C.Relkit Nothing    _) _ = Right C.relkitNothing
relkitSourceTerm pat (C.Relkit (Just he2) _) _ = Right kit3 where
    kit3   = C.relkitJust he2 $ C.RelkitSource pat ns2
    ns2    = B.headNames he2


-- ----------------------  dee & dum

-- $deedum
--
--  Nullary fullset relation.
--
--    > dee
--
--  Nullary empty relation.
--
--    > dum

consDee :: C.RopCons c
consDee use = Right $ C.relmapConst use B.reldee

consDum :: C.RopCons c
consDum use = Right $ C.relmapConst use B.reldum


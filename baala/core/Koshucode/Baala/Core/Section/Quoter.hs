{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-fields #-}

module Koshucode.Baala.Core.Section.Quoter
( koshuQuoter,
  TH.QuasiQuoter,
) where

import Data.Generics
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap          as C
import qualified Koshucode.Baala.Core.Section.Clause  as C
import qualified Koshucode.Baala.Core.Section.Section as C

{-| Make quasiquoter for @[koshu| ... |]@. -}
koshuQuoter
    :: C.RelmapHalfCons -- ^ Relmap half constructor
    -> TH.ExpQ          -- ^ Quotation expression of 'RelmapFullCons'
    -> TH.QuasiQuoter   -- ^ Quoter that outputs
                        --  'Koshucode.Baala.Core.Section.Section' or
                        --  'Koshucode.Baala.Core.Relmap.Relmap'
koshuQuoter half fullQ = TH.QuasiQuoter { TH.quoteExp = koshuQ half fullQ }

koshuQ :: C.RelmapHalfCons -> TH.ExpQ -> String -> TH.ExpQ
koshuQ half fullQ text =
    dispatch $ B.tokenLines (B.ResourceText text) text
    where
      dispatch src = sectionQ src -- relmapQ src
      sectionQ = consSectionQ fullQ . {- C.consClause half -} undefined
      --relmapQ  = consFullRelmapQ fullQ . half [] . tokenTrees

{- Construct ExpQ of Section
   Tokens like @name in section context and relmap context
   are Haskell variables. -}
consSectionQ
    :: TH.ExpQ      -- ^ Quotation expression of 'RelmapFullCons'
    -> [C.Clause]   -- ^ Materials of section
    -> TH.ExpQ      -- ^ ExpQ of 'Section'
consSectionQ fullQ xs =
    [| either consError id
         (C.consSection $fullQ (B.ResourceText "qq")
               $(TH.dataToExpQ plain xs)) |]

{- construction error -}
consError :: a -> b
consError _ = error "Syntax error in [|koshu ...|]"

plain :: b -> Maybe a
plain _ = Nothing

{- Construct ExpQ of Relmap
   Tokens like @name in relmap context are Haskell variables. -}
consFullRelmapQ
    :: TH.ExpQ        -- ^ Quotation expression of 'RelmapFullCons'
    -> C.HalfRelmap   -- ^ Target relmap operator
    -> TH.ExpQ        -- ^ ExpQ of 'Relmap' v
consFullRelmapQ fullQ = make where
    make = TH.dataToExpQ (plain `extQ` custom)
    custom (C.HalfRelmap _ _ (B.TWord _ 0 ('@':op)) _ _) =
        Just $ TH.varE $ TH.mkName op
    custom h@(C.HalfRelmap _ _ op opd subs) =
        Just $ [| either consError id
                    ($fullQ $(TH.dataToExpQ plain h))
--                     $(dataToExpQ plain opd)   -- [Relmap v]
--                     $(listE (map make subs))) -- [HalfRelmap] -> [Relmap]
                |]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-fields #-}

module Koshucode.Baala.Core.Section.Quoter
( koshuQuoter,
  QuasiQuoter,
) where

import Data.Generics
import Language.Haskell.TH hiding (Clause)
import Language.Haskell.TH.Quote

import Koshucode.Baala.Base
import Koshucode.Baala.Core.Relmap

import Koshucode.Baala.Core.Section.Clause

{-| Make quasiquoter for @[koshu| ... |]@. -}
koshuQuoter
    :: RelmapHalfCons -- ^ Relmap half constructor
    -> ExpQ           -- ^ Quotation expression of 'RelmapFullCons'
    -> QuasiQuoter    -- ^ Quoter that outputs
                      --  'Koshucode.Baala.Core.Section.Section' or
                      --  'Koshucode.Baala.Core.Relmap.Relmap'
koshuQuoter half fullQ = QuasiQuoter { quoteExp = koshuQ half fullQ }

koshuQ :: RelmapHalfCons -> ExpQ -> String -> ExpQ
koshuQ half fullQ = dispatch . sourceLines where
    dispatch src = sectionQ src -- relmapQ src
    sectionQ = consSectionQ fullQ . consClause half
    --relmapQ  = consFullRelmapQ fullQ . half [] . tokenTrees

{- Construct ExpQ of Section
   Tokens like @name in section context and relmap context
   are Haskell variables. -}
consSectionQ
    :: ExpQ      -- ^ Quotation expression of 'RelmapFullCons'
    -> [Clause]  -- ^ Materials of section
    -> ExpQ      -- ^ ExpQ of 'Section'
consSectionQ fullQ xs =
    [| either consError id
         (consSection $fullQ "qq" $(dataToExpQ plain xs)) |]

{- construction error -}
consError :: a -> b
consError _ = error "Syntax error in [|koshu ...|]"

plain :: b -> Maybe a
plain _ = Nothing

{- Construct ExpQ of Relmap
   Tokens like @name in relmap context are Haskell variables. -}
consFullRelmapQ
    :: ExpQ        -- ^ Quotation expression of 'RelmapFullCons'
    -> HalfRelmap  -- ^ Target relmap operator
    -> ExpQ        -- ^ ExpQ of 'Relmap' v
consFullRelmapQ fullQ = make where
    make = dataToExpQ (plain `extQ` custom)
    custom (HalfRelmap _ _ ('@':op) _ _) =
        Just $ varE $ mkName op
    custom h@(HalfRelmap _ _ op opd subs) =
        Just $ [| either consError id
                    ($fullQ $(dataToExpQ plain h))
--                     $(dataToExpQ plain opd)   -- [Relmap v]
--                     $(listE (map make subs))) -- [HalfRelmap] -> [Relmap]
                |]

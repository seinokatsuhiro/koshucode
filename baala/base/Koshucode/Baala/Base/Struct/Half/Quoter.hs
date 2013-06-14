{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fth -fno-warn-missing-fields #-}

module Koshucode.Baala.Base.Struct.Half.Quoter
( koshuQuoter, QuasiQuoter
) where
import Data.Generics
import Koshucode.Baala.Base.Syntax
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.HalfRelmap
import Koshucode.Baala.Base.Struct.Half.Clause
import Language.Haskell.TH hiding (Clause)
import Language.Haskell.TH.Quote

-- | Make quasiquoter for @[koshu| ... |]@.
koshuQuoter
    :: RelmapHalfCons -- ^ Relmap half constructor
    -> ExpQ           -- ^ Quotation expression of 'RelmapFullCons'
    -> QuasiQuoter    -- ^ Quoter that outputs
                      --  'Koshucode.Baala.Base.Struct.Full.Section.Section' or
                      --  'Koshucode.Baala.Base.Struct.Full.Relmap.Relmap'
koshuQuoter half fullQ = QuasiQuoter { quoteExp = koshuQ half fullQ }

koshuQ :: RelmapHalfCons -> ExpQ -> String -> ExpQ
koshuQ half fullQ = dispatch . tokens where
    dispatch toks = case sweepLeft toks of
                    (Word 0 "section" : _) -> sectionQ toks
                    _                      -> relmapQ toks
    sectionQ = consFullSectionQ fullQ . consClause half
    relmapQ  = consFullRelmapQ fullQ . consHalfRelmap half [] . tokenTrees

-- Construct ExpQ of Section
-- Tokens like @name in section context and relmap context
-- are Haskell variables.
consFullSectionQ
    :: ExpQ      -- ^ Quotation expression of 'RelmapFullCons'
    -> [Clause]  -- ^ Materials of section
    -> ExpQ      -- ^ ExpQ of 'Section'
consFullSectionQ fullQ xs =
    [| either consError id
         (consFullSection $fullQ $(dataToExpQ plain xs)) |]

consError :: a -> b
consError _ = error "Syntax error in [|koshu ...|]"

plain :: b -> Maybe a
plain _ = Nothing

-- Construct ExpQ of Relmap
-- Tokens like @name in relmap context are Haskell variables.
consFullRelmapQ
    :: ExpQ        -- ^ Quotation expression of 'RelmapFullCons'
    -> HalfRelmap  -- ^ Target relmap operator
    -> ExpQ        -- ^ ExpQ of 'Relmap' v
consFullRelmapQ fullQ = make where
    make = dataToExpQ (plain `extQ` custom)
    custom (HalfRelmap _ _ ('@':op) _ _) =
        Just $ varE $ mkName op
    custom (HalfRelmap _ _ op opd subs) =
        Just $ [| either consError id
                    ($fullQ
                     $(litE (stringL op))      -- String
                     $(dataToExpQ plain opd)   -- [Relmap v]
                     $(listE (map make subs))) -- [HalfRelmap] -> [Relmap]
                |]

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
    :: C.ConsLexmap     -- ^ Relmap lex constructor
    -> TH.ExpQ          -- ^ Quotation expression of 'ConsRelmap'
    -> TH.QuasiQuoter   -- ^ Quoter that outputs
                        --  'Koshucode.Baala.Core.Section.Section' or
                        --  'Koshucode.Baala.Core.Relmap.Relmap'
koshuQuoter lx fullQ = TH.QuasiQuoter { TH.quoteExp = koshuQ lx fullQ }

koshuQ :: C.ConsLexmap -> TH.ExpQ -> String -> TH.ExpQ
koshuQ lx fullQ text =
    dispatch $ B.tokenLines (B.ResourceText text) text
    where
      dispatch src = sectionQ src -- relmapQ src
      sectionQ = consSectionQ fullQ . {- C.consClause lx -} undefined
      --relmapQ  = consFullRelmapQ fullQ . lx [] . tokenTrees

{- Construct ExpQ of Section
   Tokens like @name in section context and relmap context
   are Haskell variables. -}
consSectionQ
    :: TH.ExpQ      -- ^ Quotation expression of 'ConsRelmap'
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
    :: TH.ExpQ     -- ^ Quotation expression of 'ConsRelmap'
    -> C.Lexmap    -- ^ Target relmap operator
    -> TH.ExpQ     -- ^ ExpQ of 'Relmap' v
consFullRelmapQ fullQ = make where
    make = TH.dataToExpQ (plain `extQ` custom)
    custom (C.Lexmap (B.TWord _ 0 ('@':op)) _ _ _) =
        Just $ TH.varE $ TH.mkName op
    custom h@(C.Lexmap _ op opd subs) =
        Just $ [| either consError id
                    ($fullQ $(TH.dataToExpQ plain h))
--                     $(dataToExpQ plain opd)   -- [Relmap v]
--                     $(listE (map make subs))) -- [Lexmap] -> [Relmap]
                |]

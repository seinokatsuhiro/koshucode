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

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Section.Clause   as C
import qualified Koshucode.Baala.Core.Section.Resource as C

{-| Make quasiquoter for @[koshu| ... |]@. -}
koshuQuoter
    :: C.ConsLexmap     -- ^ Relmap lex constructor
    -> TH.ExpQ          -- ^ Quotation expression of 'ConsRelmap'
    -> TH.QuasiQuoter   -- ^ Quoter that outputs
                        --  'Koshucode.Baala.Core.Section.Resource' or
                        --  'Koshucode.Baala.Core.Relmap.Relmap'
koshuQuoter lx fullQ = TH.QuasiQuoter { TH.quoteExp = koshuQ lx fullQ }

koshuQ :: C.ConsLexmap -> TH.ExpQ -> String -> TH.ExpQ
koshuQ _ fullQ text =
    dispatch $ B.tokenLines (B.sourceOf text) text
    where
      dispatch src = sectionQ src -- relmapQ src
      sectionQ = consResourceQ fullQ . {- C.consClause -} undefined
      --relmapQ  = consFullRelmapQ fullQ . lx [] . tokenTrees

{- Construct ExpQ of Resource
   Tokens like @name in section context and relmap context
   are Haskell variables. -}
consResourceQ
    :: TH.ExpQ      -- ^ Quotation expression of 'ConsRelmap'
    -> [C.Clause]   -- ^ Materials of section
    -> TH.ExpQ      -- ^ ExpQ of 'Resource'
consResourceQ fullQ xs =
    [| either consError id
         (C.consResource C.emptyResource (B.sourceOf "qq")
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
    custom (C.Lexmap _ (B.TText _ B.TextRaw ('@':op)) _ _ _) =
        Just $ TH.varE $ TH.mkName op
    custom h@(C.Lexmap _ op opd subs _) =
        Just $ [| either consError id
                    ($fullQ $(TH.dataToExpQ plain h))
--                     $(dataToExpQ plain opd)   -- [Relmap v]
--                     $(listE (map make subs))) -- [Lexmap] -> [Relmap]
                |]

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-fields #-}

module Koshucode.Baala.Core.Resource.Quoter
  ( koshuQuoter,
    TH.QuasiQuoter,
  ) where

import Data.Generics
import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Quote as TH

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Lexmap             as C
import qualified Koshucode.Baala.Core.Resource.Clause    as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C
import qualified Koshucode.Baala.Core.Resource.Include   as C

{-| Make quasiquoter for @[koshu| ... |]@. -}
koshuQuoter
    :: C.ConsLexmap     -- ^ Relmap lex constructor
    -> TH.ExpQ          -- ^ Quotation expression of 'ConsRelmap'
    -> TH.QuasiQuoter   -- ^ Quoter that outputs
                        --  'Koshucode.Baala.Core.Resource.Resource' or
                        --  'Koshucode.Baala.Core.Relmap.Relmap'
koshuQuoter lx fullQ = TH.QuasiQuoter { TH.quoteExp = koshuQ lx fullQ }

koshuQ :: C.ConsLexmap -> TH.ExpQ -> String -> TH.ExpQ
koshuQ _ fullQ text =
    dispatch $ D.tokenLines (B.codeTextOf text) text
    where
      dispatch src = resQ src -- relmapQ src
      resQ = resIncludeQ fullQ . {- C.consClause -} undefined
      --relmapQ  = consFullRelmapQ fullQ . lx [] . ttrees

{- Construct ExpQ of Resource
   Tokens like @name in resource context and relmap context
   are Haskell variables. -}
resIncludeQ
    :: TH.ExpQ      -- ^ Quotation expression of 'ConsRelmap'
    -> [C.Clause]   -- ^ Materials of resource
    -> TH.ExpQ      -- ^ ExpQ of 'Resource'
resIncludeQ fullQ xs =
    [| either consError id
         (C.resInclude "" C.resEmpty (B.codeTextOf "qq")
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
    custom (C.Lexmap _ (D.TTextRaw _ ('@':op)) _ _ _) =
        Just $ TH.varE $ TH.mkName op
    custom h@(C.Lexmap _ op _ subs _) =
        Just $ [| either consError id
                    ($fullQ $(TH.dataToExpQ plain h))
--                     $(dataToExpQ plain opd)   -- [Relmap v]
--                     $(listE (map make subs))) -- [Lexmap] -> [Relmap]
                |]

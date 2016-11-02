{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Core.Relmap.Message
  ( -- * Abortable
    abOption,
    abRelmap,
    abSpecialize,
    -- * Message
    unkNestVar,
    disabledInputClause,
    disabledOutputClause,
  ) where

import qualified Koshucode.Baala.Overture      as O
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Syntax        as S
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Data.Message  as Msg

-- | Abortable scope for option.
abOption :: S.TTreesTo (O.Map (B.Ab b))
abOption = Msg.abortableTrees "option"

-- | Abortable scope for relmap.
abRelmap :: (B.CodePtr cp) => [cp] -> O.Map (B.Ab b)
abRelmap = B.abortable "relmap"

-- | Abortable scope for specialization.
abSpecialize :: (B.CodePtr cp) => [cp] -> O.Map (B.Ab b)
abSpecialize = B.abortable "specialize"

-- | Unknown nested relation reference.
unkNestVar :: String -> [S.Token] -> [((S.Token, S.Local String), D.Head)] -> B.Ab a
unkNestVar n ls ds = Left $ B.abortLines "Unknown nested relation reference"
                           $ ("search" : map indent dynamic)
                          ++ ("for"    : map indent lexical)
    where lexical = map (text $ S.LocalSymbol n) ls
          dynamic = map f ds
          f ((tk, k), _) = text k tk
          text (S.LocalSymbol k) tk = unwords ["nested relation", quote k, "in", tokenAtPoint tk]
          text (S.LocalNest   k) tk = unwords ["nested relation", term  k, "in", tokenAtPoint tk]
          indent    = ("  " ++)
          term      = ('/' :)

quote :: O.Map String
quote s = "'" ++ s ++ "'"

tokenAtPoint :: S.Token -> String
tokenAtPoint tok = unwords ws where
    ws    = [S.tokenContent tok, "at L" ++ line, "C" ++ col]
    cp    = B.codePt tok
    line  = show $ B.codePtLineNo   cp
    col   = show $ B.codePtColumnNo cp

-- | Disabled feature: input clause
disabledInputClause :: B.Ab b
disabledInputClause = disabledFeature "input clause"

-- | Disabled feature: output clause
disabledOutputClause :: B.Ab b
disabledOutputClause = disabledFeature "output clause"

disabledFeature :: String -> B.Ab b
disabledFeature = Left . B.abortLine "Disabled feature"


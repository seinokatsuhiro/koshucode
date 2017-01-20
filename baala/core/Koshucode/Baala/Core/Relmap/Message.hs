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
import qualified Koshucode.Baala.Type          as T

-- | Abortable scope for option.
abOption :: (B.GetCodePos cp) => B.Abortable cp b
abOption = B.abortable "option"

-- | Abortable scope for relmap.
abRelmap :: (B.GetCodePos cp) => B.Abortable cp b
abRelmap = B.abortable "relmap"

-- | Abortable scope for specialization.
abSpecialize :: (B.GetCodePos cp) => B.Abortable cp b
abSpecialize = B.abortable "specialize"

-- | Unknown nested relation reference.
unkNestVar :: String -> [S.Token] -> [((S.Token, S.LocalRef), T.Head)] -> B.Ab a
unkNestVar n ls ds = left where
    left = B.leftLines "Unknown nested relation reference"
           $ ("search" : map indent dynamic)
          ++ ("for"    : map indent lexical)
    indent  = ("  " ++)
    lexical = map (text $ S.LocalSymbol n) ls
    dynamic = map f ds
    f ((tk, k), _) = text k tk
    text (S.LocalSymbol k) tk = unwords ["nested relation", quote k, "in", tokenAtPoint tk]
    text (S.LocalNest   k) tk = unwords ["nested relation", S.termNameString k, "in", tokenAtPoint tk]

quote :: O.StringMap
quote s = "'" ++ s ++ "'"

tokenAtPoint :: S.Token -> String
tokenAtPoint tok = unwords ws where
    ws    = [S.tokenContent tok, "at L" ++ line, "C" ++ col]
    cp    = B.getCP tok
    line  = show $ B.cpLineNo   cp
    col   = show $ B.cpCharNo cp

-- | Disabled feature: input clause
disabledInputClause :: B.Ab b
disabledInputClause = disabledFeature "input clause"

-- | Disabled feature: output clause
disabledOutputClause :: B.Ab b
disabledOutputClause = disabledFeature "output clause"

disabledFeature :: String -> B.Ab b
disabledFeature = B.leftLine "Disabled feature"


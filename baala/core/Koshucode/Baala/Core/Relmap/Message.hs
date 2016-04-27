{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relmap.Message
  ( -- * Abortable
    abOption,
    abRelmap,
    abSpecialize,
    -- * Message
    unkNestVar,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Syntax        as D
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Data.Message  as Msg

abOption :: D.TTreesTo (B.Map (B.Ab b))
abOption = Msg.abortableTrees "option"

abRelmap :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRelmap = B.abortable "relmap"

abSpecialize :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abSpecialize = B.abortable "specialize"

unkNestVar :: String -> [D.Token] -> [((D.Token, D.Local String), D.Head)] -> B.Ab a
unkNestVar n ls ds = Left $ B.abortLines "Unknown nested relation reference"
                           $ ("search" : map indent dynamic)
                          ++ ("for"    : map indent lexical)
    where lexical = map (text $ D.LocalSymbol n) ls
          dynamic = map f ds
          f ((tk, k), _) = text k tk
          text (D.LocalSymbol k) tk = unwords ["nested relation", quote k, "in", tokenAtPoint tk]
          text (D.LocalNest   k) tk = unwords ["nested relation", term  k, "in", tokenAtPoint tk]
          indent    = ("  " ++)
          term      = ('/' :)

quote :: B.Map String
quote s = "'" ++ s ++ "'"

tokenAtPoint :: D.Token -> String
tokenAtPoint tok = unwords ws where
    ws    = [D.tokenContent tok, "at L" ++ line, "C" ++ col]
    cp    = B.codePt tok
    line  = show $ B.codePtLineNo   cp
    col   = show $ B.codePtColumnNo cp


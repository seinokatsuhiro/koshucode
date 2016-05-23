{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token utilities.

module Koshucode.Baala.Syntax.Token.Utility
  ( -- * Selectors
    tokenContent, untoken,
    tokenDetailTypeString,
    tokenParents,
  
    -- * Type of token
    isBlankToken, sweepToken,
    isShortToken, isTermToken,
    isOpenToken, isCloseToken,
    isOpenTokenOf, isCloseTokenOf,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax.Token.Token  as S


-- ----------------------  Selector

-- | Get the content of token.
--
--   >>> let tok = S.TTermPath B.def ["r", "x"] in tokenContent tok
--   "/r/x"

tokenContent :: S.Token -> String
tokenContent tok =
    case tok of
      S.TText     _ _ s    -> s
      S.TName     _ op     -> B.name op
      S.TShort    _ a b    -> a ++ "." ++ b
      S.TTermN    _ _ n    -> '/' : n
      S.TTerm     _ _ ns   -> concatMap ('/' :) ns
      S.TLocal    _ n _ _  -> S.unlocal n
      S.TSlot     _ _ s    -> s
      S.TOpen     _ s      -> s
      S.TClose    _ s      -> s
      S.TSpace    _ n      -> replicate n ' '
      S.TComment  _ s      -> s

untoken :: S.Token -> String
untoken = dispatch where
    dispatch tok =
        case tok of
          S.TText     _ q s    -> text q s
          S.TName     _ op     -> B.name op
          S.TShort    _ a b    -> a ++ "." ++ b
          S.TTermN    _ _ n    -> '/' : n
          S.TTerm     _ _ ns   -> concatMap ('/' :) ns
          S.TLocal    _ n _ _  -> S.unlocal n
          S.TSlot     _ _ s    -> s
          S.TOpen     _ s      -> s
          S.TClose    _ s      -> s
          S.TSpace    _ n      -> replicate n ' '
          S.TComment  _ s      -> s
    text q s =
        case q of
          S.TextUnk            -> s
          S.TextRaw            -> s
          S.TextQ              -> "'" ++ s
          S.TextQQ             -> "\"" ++ s ++ "\""
          S.TextKey            -> s
          S.TextBar            -> s
          S.TextName           -> s
          S.TextLicense        -> s

-- | Get detail type string of token.
--
--   >>> let tok = S.textToken "flower" in (S.subtypeString tok, tokenDetailTypeString tok)
--   ("text", Just "raw")

tokenDetailTypeString :: S.Token -> Maybe String
tokenDetailTypeString tok =
    case tok of
      S.TText     _ f _    -> Just $ S.subtypeString f
      S.TName     _ b      -> Just $ S.subtypeString b
      S.TShort    _ _ _    -> Nothing
      S.TTermN    _ _ _    -> Nothing
      S.TTerm     _ _ _    -> Nothing
      S.TLocal    _ _ _ _  -> Nothing
      S.TSlot     _ n _    -> Just $ slotTypeText n
      S.TOpen     _ _      -> Nothing
      S.TClose    _ _      -> Nothing
      S.TSpace    _ _      -> Nothing
      S.TComment  _ _      -> Nothing

slotTypeText :: Int -> String
slotTypeText 0   = "positional"
slotTypeText 1   = "named"
slotTypeText 2   = "global"
slotTypeText _   = "unknown"

tokenParents :: S.Token -> [S.Token]
tokenParents (S.TLocal _ _ _ ps) = ps
tokenParents _                   = []


-- ----------------------  Predicate

-- | Test token is blank, i.e., comment or space.
isBlankToken :: B.Pred S.Token
isBlankToken (S.TSpace _ _)       = True
isBlankToken (S.TComment _ _)     = True
isBlankToken _                    = False

-- | Remove blank tokens.
sweepToken :: B.Map [S.Token]
sweepToken = B.omit isBlankToken

-- | Test token is short-type token.
isShortToken :: B.Pred S.Token
isShortToken (S.TShort _ _ _)     = True
isShortToken _                    = False

-- | Test token is term-type token.
isTermToken :: B.Pred S.Token
isTermToken (S.TTermN _ _ _)      = True
isTermToken (S.TTerm _ _ _)       = True
isTermToken _                     = False

-- | Test token is open-type token.
isOpenToken :: B.Pred S.Token
isOpenToken (S.TOpen _ _)         = True
isOpenToken _                     = False

-- | Test token is close-type token.
isCloseToken :: B.Pred S.Token
isCloseToken (S.TClose _ _)       = True
isCloseToken _                    = False

-- | Test token is a open-type of the specific bracket.
--
--   >>> let tok = S.TOpen B.def "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = S.TOpen B.def "{" in isOpenTokenOf "(" tok
--   False

isOpenTokenOf :: String -> B.Pred S.Token
isOpenTokenOf p1 (S.TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _                 = False

-- | Test token is a close-type of the specific bracket.
isCloseTokenOf :: String -> B.Pred S.Token
isCloseTokenOf p1 (S.TClose _ p2) = p1 == p2
isCloseTokenOf _ _                = False


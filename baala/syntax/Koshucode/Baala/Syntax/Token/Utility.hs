{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token utilities.

module Koshucode.Baala.Syntax.Token.Utility
  ( -- * Selectors
    tokenContent, untoken,
    tokenTypeText, tokenSubtypeText,
    tokenParents,
    -- $Selector
  
    -- * Predicates
    isBlankToken, sweepToken,
    isShortToken, isTermToken,
    isOpenToken, isCloseToken,
    isOpenTokenOf, isCloseTokenOf,
    -- $Predicate
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax.Token.Token  as S


-- ----------------------  Selector

-- $Selector
--
--   >>> let tok = TTerm B.def 0 ["r", "x"] in tokenContent tok
--   "/r/x"
--
--   >>> let tok = textToken "flower" in (tokenTypeText tok, tokenSubtypeText tok)
--   ("text", Just "raw")

-- | Get the content of token.
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
untoken tok =
    case tok of
      S.TText     _ q s    -> case q of
                                S.TextUnk     -> s
                                S.TextRaw     -> s
                                S.TextQ       -> "'" ++ s
                                S.TextQQ      -> "\"" ++ s ++ "\""
                                S.TextKey     -> s
                                S.TextBar     -> s
                                S.TextName    -> s
                                S.TextLicense -> s
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

-- | Text of token type, e.g., @\"text\"@, @\"open\"@.
tokenTypeText :: S.Token -> String
tokenTypeText tok =
    case tok of
      S.TText     _ _ _    -> "text"
      S.TName     _ _      -> "name"
      S.TShort    _ _ _    -> "short"
      S.TTermN    _ _ _    -> "term"
      S.TTerm     _ _ _    -> "term"
      S.TLocal    _ _ _ _  -> "local"
      S.TSlot     _ _ _    -> "slot"
      S.TOpen     _ _      -> "open"
      S.TClose    _ _      -> "close"
      S.TSpace    _ _      -> "space"
      S.TComment  _ _      -> "comment"

tokenSubtypeText :: S.Token -> Maybe String
tokenSubtypeText tok =
    case tok of
      S.TText     _ f _    -> Just $ S.textFormTypeText f
      S.TName     _ b      -> Just $ S.blankNameTypeText b
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

-- $Predicate
--
--   >>> let tok = TOpen B.def "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = TOpen B.def "{" in isOpenTokenOf "(" tok
--   False

-- | Test the token is blank, i.e., comment or space.
isBlankToken :: B.Pred S.Token
isBlankToken (S.TSpace _ _)       = True
isBlankToken (S.TComment _ _)     = True
isBlankToken _                    = False

-- | Remove blank tokens.
sweepToken :: B.Map [S.Token]
sweepToken = B.omit isBlankToken

isShortToken :: B.Pred S.Token
isShortToken (S.TShort _ _ _)     = True
isShortToken _                    = False

isTermToken :: B.Pred S.Token
isTermToken (S.TTermN _ _ _)      = True
isTermToken (S.TTerm _ _ _)       = True
isTermToken _                     = False

isOpenToken :: B.Pred S.Token
isOpenToken (S.TOpen _ _)         = True
isOpenToken _                     = False

isCloseToken :: B.Pred S.Token
isCloseToken (S.TClose _ _)       = True
isCloseToken _                    = False

-- | Check token is a 'TOpen' of the specific bracket.
isOpenTokenOf :: String -> B.Pred S.Token
isOpenTokenOf p1 (S.TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _                 = False

-- | Check token is a 'TClose' of the specific bracket.
isCloseTokenOf :: String -> B.Pred S.Token
isCloseTokenOf p1 (S.TClose _ p2) = p1 == p2
isCloseTokenOf _ _                = False


{-# OPTIONS_GHC -Wall #-}

-- | Token utilities.

module Koshucode.Baala.Syntax.Token.Utility
  ( -- * Selectors
    tokenContent,
    tokenDetailTypeString,
    tokenParents,
    prepareTokens,
    sweepToken,
  
    -- * Type of token
    isBlankToken,
    isShortToken, isTermToken,
    isOpenToken, isCloseToken,
    --isOpenTokenOf, isCloseTokenOf,
    isBracketTokenOf,
    isUnknownToken,
  ) where

import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Syntax.Symbol       as S
import qualified Koshucode.Baala.Syntax.Token.Token  as S


-- ----------------------  Selector

-- | Get the content of token.
--
--   >>> tokenContent $ S.TOpen B.def "("
--   "("
--
tokenContent :: S.Token -> String
tokenContent tok =
    case tok of
      S.TText     _ _ s    -> s
      S.TShort    _ a b    -> a O.++ "." O.++ b
      S.TTerm     _ n      -> S.enslash n
      S.TLocal    _ n _ _  -> B.name n
      S.TSlot     _ _ s    -> s
      S.TOpen     _ s      -> s
      S.TClose    _ s      -> s
      S.TSpace    _ n      -> replicate n ' '
      S.TComment  _ s      -> s
      S.TName     _ op     -> B.name op
      S.TUnknown  _ s _    -> s

-- | Get detail type string of token.
--
--   >>> let tok = S.rawTextToken "flower" in (S.subtypeName tok, tokenDetailTypeString tok)
--   ("text", Just "raw")
--
tokenDetailTypeString :: S.TToken t -> Maybe String
tokenDetailTypeString tok =
    case tok of
      S.TText     _ f _    -> Just $ S.subtypeName f
      S.TShort    _ _ _    -> Nothing
      S.TTerm     _ _      -> Nothing
      S.TLocal    _ _ _ _  -> Nothing
      S.TSlot     _ n _    -> Just $ slotTypeText n
      S.TOpen     _ _      -> Nothing
      S.TClose    _ _      -> Nothing
      S.TSpace    _ _      -> Nothing
      S.TComment  _ _      -> Nothing
      S.TName     _ b      -> Just $ S.subtypeName b
      S.TUnknown  _ _ _    -> Nothing

slotTypeText :: Int -> String
slotTypeText 0   = "positional"
slotTypeText 1   = "named"
slotTypeText 2   = "global"
slotTypeText _   = "unknown"

-- | Get token parents from local token.
tokenParents :: S.Token -> [S.Token]
tokenParents (S.TLocal _ _ _ ps) = ps
tokenParents _                   = []

-- | Prepare token list for parsing.
--
--   1. Remove spaces.
--   2. Remove comments.
--   3. Join fragmented texts.
--
prepareTokens :: (O.Textual t) => O.Map [S.TToken t]
prepareTokens = pre where
    pre ((S.TText cp f1 s1) : (S.TText _ f2 s2) : ts)
        | isQqKey f1 && isQqKey f2
                          = let t' = S.TText cp S.TextQQ (s1 O.++ s2)
                            in pre (t' : ts)
    pre ((S.TText cp f1 s1) : ts)
        | isQqKey f1      = (S.TText cp S.TextQQ s1) : pre ts
    pre (t : ts)
        | isBlankToken t  = pre ts
        | otherwise       = t : pre ts
    pre []                = []

-- | Test double-quoted or related text.
isQqKey :: O.Test S.TextForm
isQqKey f = f == S.TextQQ || f == S.TextKey

-- | Remove blank tokens.
sweepToken :: O.Map [S.TToken t]
sweepToken = B.omit isBlankToken


-- ----------------------  Predicate

-- | Test token is blank, i.e., comment or space.
isBlankToken :: O.Test (S.TToken t)
isBlankToken (S.TSpace _ _)       = True
isBlankToken (S.TComment _ _)     = True
isBlankToken _                    = False

-- | Test token is unknown.
isUnknownToken :: O.Test (S.TToken t)
isUnknownToken (S.TUnknown _ _ _) = True
isUnknownToken _                  = False

-- | Test token is short-type token.
isShortToken :: O.Test (S.TToken t)
isShortToken (S.TShort _ _ _)     = True
isShortToken _                    = False

-- | Test token is term-type token.
isTermToken :: O.Test (S.TToken t)
isTermToken (S.TTerm  _ _)        = True
isTermToken _                     = False

-- | Test token is open-type token.
isOpenToken :: O.Test (S.TToken t)
isOpenToken (S.TOpen _ _)         = True
isOpenToken _                     = False

-- | Test token is close-type token.
isCloseToken :: O.Test (S.TToken t)
isCloseToken (S.TClose _ _)       = True
isCloseToken _                    = False

-- | Test token is a open-type of the specific bracket.
--
--   >>> let tok = S.TOpen B.def "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = S.TOpen B.def "{" in isOpenTokenOf "(" tok
--   False
--
isOpenTokenOf :: (Eq t) => t -> O.Test (S.TToken t)
isOpenTokenOf p1 (S.TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _                 = False

-- | Test token is a close-type of the specific bracket.
isCloseTokenOf :: (Eq t) => t -> O.Test (S.TToken t)
isCloseTokenOf p1 (S.TClose _ p2) = p1 == p2
isCloseTokenOf _ _                = False

-- | Create bracket testers.
isBracketTokenOf :: (Eq t) => (t, t) -> (O.Test (S.TToken t), O.Test (S.TToken t))
isBracketTokenOf (a, b) = (isOpenTokenOf a, isCloseTokenOf b)

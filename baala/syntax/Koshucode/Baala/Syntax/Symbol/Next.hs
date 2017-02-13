{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Next character sequence.

module Koshucode.Baala.Syntax.Symbol.Next
  ( -- * Next
    Next, AbNext,
    nextSpace,
    nextQQ,
    nextBefore,
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax.Symbol.Message  as Msg

-- | Split next character sequence from input text.
type Next t a = t -> (t, a)

-- | Split next character sequence from input text.
type AbNext t a = t -> B.Ab (t, a)

-- | Test double punctuation character.
isQQ :: O.Test Char
isQQ = (== '"')

-- | Get next spaces.
--   Space character is decided by 'Ch.isSpace'.
--
--   >>> nextSpace " abc"
--   ("abc", 1)
--
--   >>> nextSpace " \tabc"
--   ("abc", 2)
--
nextSpace :: (O.Textual t) => Next t Int
nextSpace = loop 0 where
    loop n (O.cut -> O.Jp c cs)
        | Ch.isSpace c   = loop (n + 1) cs
    loop n cs            = (cs, n)

{-| Get next double quoted text.
    A single double quote ends text.
    A double double quote does not end text,
    but it is converted to double quote.

    >>> nextQQ "abc\" def"
    Right (" def","abc")

    >>> nextQQ "abc\"\"def\" ghi"
    Right (" ghi","abc\"def")
    -}
nextQQ :: (O.Textual t) => AbNext t t
nextQQ = text where
    text t = case O.span (not . isQQ) t of
                (cnt, t1) -> do (t2, cont2) <- qq t1
                                case O.tIsEmpty cont2 of
                                  True  -> Right (t2, cnt)
                                  False -> Right (t2, cnt O.++ cont2)
    qq (O.cut -> O.Jp q t)
        | isQQ q = case O.cut t of
                     O.Jp q' t1 | isQQ q' -> do (t2, cnt) <- text t1
                                                Right (t2, q O.<:> cnt)
                     _ -> Right (t, O.tEmpty)
    qq _ = Msg.quotNotEnd

-- | Get next text before given delimiter.
--
--   >>> nextBefore "''" "abc'' def"
--   Right (" def","abc")
--
nextBefore :: (O.Textual t) => t -> AbNext t t
nextBefore (O.cut -> O.Jp t0 to) cs0 = loop O.zero cs0 where
    loop n (O.cut -> O.Jp c cs)
        | c == t0     = case O.tDropPrefix to cs of
                          Just cs' -> Right (cs', O.tTake n cs0)
                          Nothing  -> loop (n + 1) cs
        | otherwise   = loop (n + 1) cs
    loop _ _          = Msg.quotNotEnd
nextBefore _ _ = B.bug "nextBefore"


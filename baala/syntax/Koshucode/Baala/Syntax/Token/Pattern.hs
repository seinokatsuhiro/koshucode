{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token pattern.

module Koshucode.Baala.Syntax.Token.Pattern
  ( -- * Text token
    pattern T,
    pattern TUnk,
    pattern TRaw,
    pattern TSection,
    pattern TQ,
    pattern TQq,
    pattern TKey,
    pattern TBar,
    pattern TLicense,

    -- * Other token
    pattern Term,
    pattern TermOrd,
  ) where

import qualified Koshucode.Baala.Syntax.Token.Token  as S

-- | Text token.
pattern T f w <- S.TText _ f w

-- | Unknown text.
pattern TUnk w <- T S.TextUnk w

-- | Raw text.
pattern TRaw w <- T S.TextRaw w

-- | Section sign: @"==="@
pattern TSection <- TRaw "==="

-- | Single-quoted text.
pattern TQ w <- T S.TextQ w

-- | Double-quoted text.
pattern TQq w <- T S.TextQQ w

-- | Keyword text.
pattern TKey w <- T S.TextKey w

-- | Bar-enclosed text.
pattern TBar w <- T S.TextBar w

-- | License text.
pattern TLicense w <- T S.TextLicense w

-- | Term token.
pattern Term w <- S.TTerm _ _ w

-- | Term token with ordering.
pattern TermOrd o w <- S.TTerm _ o w


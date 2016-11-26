{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token pattern.

module Koshucode.Baala.Syntax.Token.Pattern
  ( pattern TText,
    pattern TUnk,
    pattern TRaw,
    pattern TSection,
    pattern TQ,
    pattern TQq,
    pattern TKey,
    pattern TBar,
    pattern TLicense,
    pattern TTerm,
  ) where

import qualified Koshucode.Baala.Syntax.Token.Token  as S

-- | Text token.
pattern TText f w <- S.TText _ f w

-- | Unknown text token.
pattern TUnk w <- TText S.TextUnk w

-- | Raw text token.
pattern TRaw w <- TText S.TextRaw w

-- | Section sign.
pattern TSection <- TRaw "==="

-- | Quoted text token.
pattern TQ w <- TText S.TextQ w

-- | Dobule-quoted text token.
pattern TQq w <- TText S.TextQQ w

-- | Keyword token.
pattern TKey w <- TText S.TextKey w

-- | Bar-enclosed token.
pattern TBar w <- TText S.TextBar w

-- | License text token.
pattern TLicense w <- TText S.TextLicense w

-- | Term token.
pattern TTerm w <- S.TTerm _ w


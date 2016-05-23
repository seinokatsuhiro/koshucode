{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Token pattern.

module Koshucode.Baala.Syntax.Token.Pattern
  ( -- * Text token
    pattern TTextUnk,
    pattern TTextRaw,
    pattern TTextQ,
    pattern TTextQQ,
    pattern TTextKey,
    pattern TTextBar,
    pattern TTextName,
    pattern TTextLicense,
    pattern TTextSect,

    -- * Term token
    pattern TTermPath,
    pattern TTermQ,
  ) where

import qualified Koshucode.Baala.Syntax.Token.Token  as S


-- ----------------------  Text token

-- | Unknown text token.
pattern TTextUnk cp w = S.TText cp S.TextUnk  w

-- | Raw text token.
--
--   >>> S.TTextRaw B.def "a"   -- a
pattern TTextRaw cp w = S.TText cp S.TextRaw  w

-- | Quoted text token.
--
--   >>> S.TTextQ B.def "a"   -- 'a
pattern TTextQ cp w = S.TText cp S.TextQ    w

-- | Dobule-quoted text token.
--
--   >>> S.TTextQQ B.def "a"   -- "a"
pattern TTextQQ cp w = S.TText cp S.TextQQ   w

-- | Keyword token.
--
--   >>> S.TTextKey B.def "a"   -- <a>
pattern TTextKey cp w = S.TText cp S.TextKey  w

-- | Bar-enclosed token.
--
--   >>> S.TTextBar B.def "a"   -- |a|
pattern TTextBar cp w = S.TText cp S.TextBar  w

pattern TTextName cp w = S.TText cp S.TextName w

pattern TTextLicense cp w = S.TText cp S.TextLicense w

pattern TTextSect cp = TTextRaw cp "==="


-- ----------------------  Term token

-- | Term path token.
--
--   >>> TTermPath B.def ["a", "b"]   -- /a/b
pattern TTermPath cp ws = S.TTerm cp S.TermTypePath ws

-- | Quoted term path token.
--
--   >>> TTermQ B.def ["a"]   -- '/a
pattern TTermQ cp ws = S.TTerm cp S.TermTypeQuoted ws

{-# OPTIONS_GHC -Wall #-}

-- | Runtime structure for calculations written in Koshucode.
--   'Section' is bundle of calculation stuff.
-- 
--   Dependencies of significant data types.
-- 
--   * 'Section' uses 'Section', 'Assert', 'Relmap',
--     'Koshucode.Baala.Base.Data.Judge.Judge' and 'RelmapCons'.
-- 
--   * 'Assert' uses 'Relmap'.
-- 
--   * 'Relmap' uses 'Relmap' and
--     'Koshucode.Baala.Base.Data.Rel.Rel'.
--     
--   * 'RelmapCons' uses 'RelmapHalfCons' and 'RelmapFullCons'.
-- 
--   * 'RelmapHalfCons' uses 'HalfRelmap' and
--     'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
--   * 'RelmapFullCons' uses 'Relmap' and
--     'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.
-- 
--   * 'Clause' uses 'HalfRelmap', 'Clause', and
--     'Koshucode.Baala.Base.Syntax.Token.Token'.
-- 
--   * 'HalfRelmap' uses 'HalfRelmap' and
--     'Koshucode.Baala.Base.Syntax.TokenTree.TokenTree'.

module Koshucode.Baala.Base.Section
( module Koshucode.Baala.Base.Section.Run
, module Koshucode.Baala.Base.Section.Section
, module Koshucode.Baala.Base.Section.Clause
, module Koshucode.Baala.Base.Section.Quoter
, module Koshucode.Baala.Base.Section.SectionIO
) where

import Koshucode.Baala.Base.Section.Run
import Koshucode.Baala.Base.Section.Section
import Koshucode.Baala.Base.Section.Clause
import Koshucode.Baala.Base.Section.Quoter
import Koshucode.Baala.Base.Section.SectionIO


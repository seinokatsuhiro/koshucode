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

module Koshucode.Baala.Base.Struct
( module Koshucode.Baala.Base.Struct.Full.Assert
, module Koshucode.Baala.Base.Struct.Full.HalfRelmap
, module Koshucode.Baala.Base.Struct.Full.Relmap
, module Koshucode.Baala.Base.Struct.Full.Run
, module Koshucode.Baala.Base.Struct.Full.Section
, module Koshucode.Baala.Base.Struct.Half.Clause
, module Koshucode.Baala.Base.Struct.Half.RelmapCons
, module Koshucode.Baala.Base.Struct.Half.Implement
, module Koshucode.Baala.Base.Struct.Half.Quoter
, module Koshucode.Baala.Base.Struct.SectionIO
) where

import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.HalfRelmap
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Full.Run
import Koshucode.Baala.Base.Struct.Full.Section
import Koshucode.Baala.Base.Struct.Half.Clause
import Koshucode.Baala.Base.Struct.Half.RelmapCons
import Koshucode.Baala.Base.Struct.Half.Implement
import Koshucode.Baala.Base.Struct.Half.Quoter
import Koshucode.Baala.Base.Struct.SectionIO


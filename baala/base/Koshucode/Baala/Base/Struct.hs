{-# OPTIONS_GHC -Wall #-}

-- | Runtime structure for calculations written in Koshucode.
--   'Module' is bundle of calculation stuff.
-- 
--   Dependencies of significant data types.
-- 
--   * 'Module' uses 'Module', 'Assert', 'Relmap',
--     'Koshucode.Baala.Base.Data.Judge.Judge' and 'ConsRelmap'.
-- 
--   * 'Assert' uses 'Relmap'.
-- 
--   * 'Relmap' uses 'Relmap' and
--     'Koshucode.Baala.Base.Data.Rel.Rel'.
--     
--   * 'ConsRelmap' uses 'ConsHalfRelmap' and 'RelmapFullCons'.
-- 
--   * 'ConsHalfRelmap' uses 'HalfRelmap' and
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
, module Koshucode.Baala.Base.Struct.Full.Module
, module Koshucode.Baala.Base.Struct.Full.Relmap
, module Koshucode.Baala.Base.Struct.Half.Clause
, module Koshucode.Baala.Base.Struct.Half.HalfRelmap
, module Koshucode.Baala.Base.Struct.Half.Quoter
, module Koshucode.Baala.Base.Struct.ModuleIO
) where
import Koshucode.Baala.Base.Struct.Full.Assert
import Koshucode.Baala.Base.Struct.Full.Module
import Koshucode.Baala.Base.Struct.Full.Relmap
import Koshucode.Baala.Base.Struct.Half.Clause
import Koshucode.Baala.Base.Struct.Half.HalfRelmap
import Koshucode.Baala.Base.Struct.Half.Quoter
import Koshucode.Baala.Base.Struct.ModuleIO

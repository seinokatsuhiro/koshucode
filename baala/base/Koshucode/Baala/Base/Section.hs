{-# OPTIONS_GHC -Wall #-}

{-| Runtime structure for calculations written in Koshucode.
    'Section' is bundle of calculation stuff.
  
    Dependencies of significant data types.
  
    * 'Section' uses
      'Section',
      'Koshucode.Baala.Base.Relmap.Assert',
      'Koshucode.Baala.Base.Relmap.Relmap',
      'Koshucode.Baala.Base.Data.Judge.Judge' and
      'Koshucode.Baala.Base.Relmap.Construct'.
  
    * 'Clause' uses
      'Clause',
      'Koshucode.Baala.Base.Relmap.HalfRelmap', and
      'Koshucode.Baala.Base.Syntax.Token.Token'.      -}

module Koshucode.Baala.Base.Section
( module Koshucode.Baala.Base.Section.Section
, module Koshucode.Baala.Base.Section.Clause
, module Koshucode.Baala.Base.Section.Quoter
, module Koshucode.Baala.Base.Section.SectionIO
, module Koshucode.Baala.Base.Section.SectionUnion
) where

import Koshucode.Baala.Base.Section.Section
import Koshucode.Baala.Base.Section.Clause
import Koshucode.Baala.Base.Section.Quoter
import Koshucode.Baala.Base.Section.SectionIO
import Koshucode.Baala.Base.Section.SectionUnion


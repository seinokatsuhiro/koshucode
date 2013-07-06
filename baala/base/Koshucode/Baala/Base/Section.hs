{-# OPTIONS_GHC -Wall #-}

{-| Runtime structure for calculations written in Koshucode.
    'Section' is bundle of calculation stuff.  
  -}

module Koshucode.Baala.Base.Section
(
-- * Modules
 module Koshucode.Baala.Base.Section.Section
, module Koshucode.Baala.Base.Section.Clause
, module Koshucode.Baala.Base.Section.Clausify
, module Koshucode.Baala.Base.Section.Quoter
, module Koshucode.Baala.Base.Section.SectionIO
, module Koshucode.Baala.Base.Section.SectionUnion
, module Koshucode.Baala.Base.Section.Utility

-- * Module dependencies
-- $ModuleDependency

-- * Data dependencies
-- $DataDependency
) where

import Koshucode.Baala.Base.Section.Section
import Koshucode.Baala.Base.Section.Clause
import Koshucode.Baala.Base.Section.Clausify
import Koshucode.Baala.Base.Section.Quoter
import Koshucode.Baala.Base.Section.SectionIO
import Koshucode.Baala.Base.Section.SectionUnion
import Koshucode.Baala.Base.Section.Utility

-- ----------------------
{- $ModuleDependency

* @Section@ module uses
  @Relmap@,
  @Data@,
  @Abort@,
  @Syntax@, and
  @Prelude@ modules.

* @Relmap@ module uses
  @Data@,
  @Abort@,
  @Syntax@, and
  @Prelude@ modules.

* @Abort@ module uses
  @Syntax@ and
  @Prelude@ modules.

* @Syntax@ module uses
  @Prelude@ module.

* @Data@ module uses
  @Prelude@ module.

* @Prelude@ module uses
  no modules in @Koshucode@.

Following picture shows reduced dependencies.

@
      Section
        |
      Relmap
     /      \\
  Data     Abort
    \\        |
     \\     Syntax
      \\     /
      Prelude
@
-}



-- ----------------------
{- $DataDependency

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
  'Koshucode.Baala.Base.Syntax.Token.Token'.
-}


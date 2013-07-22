{-# OPTIONS_GHC -Wall #-}

{-| Runtime structure for calculations written in Koshucode.
    'Section' is bundle of calculation stuff.  
  -}

module Koshucode.Baala.Core.Section
(
-- * Modules
 module Koshucode.Baala.Core.Section.Section
, module Koshucode.Baala.Core.Section.Clause
, module Koshucode.Baala.Core.Section.Clausify
, module Koshucode.Baala.Core.Section.Quoter
, module Koshucode.Baala.Core.Section.SectionIO
, module Koshucode.Baala.Core.Section.SectionUnion

-- * Module dependencies
-- $ModuleDependency

-- * Data dependencies
-- $DataDependency
) where

import Koshucode.Baala.Core.Section.Section
import Koshucode.Baala.Core.Section.Clause
import Koshucode.Baala.Core.Section.Clausify
import Koshucode.Baala.Core.Section.Quoter
import Koshucode.Baala.Core.Section.SectionIO
import Koshucode.Baala.Core.Section.SectionUnion

-- ----------------------
{- $ModuleDependency

* @Section@ module uses
  @Relmap@,
  @Data@,
  @Abort@,
  @Syntax@, and
  @Prelude@ modules.

* @Relmap@ module uses
  @Content@,
  @Data@,
  @Abort@,
  @Syntax@, and
  @Prelude@ modules.

* @Content@ module uses
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
        |
      Content
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
  'Koshucode.Baala.Core.Relmap.Assert',
  'Koshucode.Baala.Core.Relmap.Relmap',
  'Koshucode.Baala.Base.Data.Judge.Judge' and
  'Koshucode.Baala.Core.Relmap.Construct'.

* 'Clause' uses
  'Clause',
  'Koshucode.Baala.Core.Relmap.HalfRelmap', and
  'Koshucode.Baala.Base.Syntax.Token.Token'.
-}


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core
(
-- * Modules
module Koshucode.Baala.Core.Assert,
module Koshucode.Baala.Core.Content,
module Koshucode.Baala.Core.Relmap,
module Koshucode.Baala.Core.Section,

-- * Dependencies
-- $ModuleDependency
) where

import Koshucode.Baala.Core.Assert
import Koshucode.Baala.Core.Content
import Koshucode.Baala.Core.Relmap
import Koshucode.Baala.Core.Section


-- ----------------------
{- $ModuleDependency

* @[Core]@ module is a union of
  @Section@,
  @Assert@,
  @Relmap@, and
  @Content@ modules.

* @Section@ module uses
  @Assert@,
  @Relmap@,
  @Content@, and
  @[Base]@ modules.

* @Assert@ module uses
  @Relmap@,
  @Content@, and
  @[Base]@ modules.

* @Relmap@ module uses
  @Content@, and
  @[Base]@ modules.

* @Content@ module uses
  @Data@, and
  @[Base]@ modules.

* @[Base]@ module is a union of
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
  no modules in Koshucode.

Following picture shows reduced dependencies.

@
      [Core]             [Base]
        |               /     \\
      Section        Data     Abort
        |              \\        |
      Assert            \\     Syntax
        |                \\     /
      Relmap             Prelude
        |
      Content
        |
      [Base]
@
-}


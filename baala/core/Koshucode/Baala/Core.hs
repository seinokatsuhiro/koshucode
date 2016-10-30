{-# OPTIONS_GHC -Wall #-}

-- | Bundle of core modules.

module Koshucode.Baala.Core
  ( -- * Modules
    module Koshucode.Baala.Core.Assert,
    module Koshucode.Baala.Core.Lexmap,
    module Koshucode.Baala.Core.Relkit,
    module Koshucode.Baala.Core.Relmap,
    module Koshucode.Baala.Core.Resource,
  
    -- * Dependencies
    -- $ModuleDependency
  ) where

import Koshucode.Baala.Core.Assert
import Koshucode.Baala.Core.Lexmap
import Koshucode.Baala.Core.Relkit
import Koshucode.Baala.Core.Relmap
import Koshucode.Baala.Core.Resource


-- ----------------------
-- $ModuleDependency
--
-- * @[Core]@ module is a union of
--   @Resource@,
--   @Assert@,
--   @Relmap@, and
--   @Content@ modules.
--
-- * @Resource@ module uses
--   @Assert@,
--   @Relmap@,
--   @Content@, and
--   @[Base]@ modules.
--
-- * @Assert@ module uses
--   @Relmap@,
--   @Content@, and
--   @[Base]@ modules.
--
-- * @Relmap@ module uses
--   @Content@, and
--   @[Base]@ modules.
--
-- * @Content@ module uses
--   @Data@, and
--   @[Base]@ modules.
--
-- * @[Base]@ module is a union of
--   @Data@,
--   @Abort@,
--   @Syntax@, and
--   @Prelude@ modules.
--
-- * @Abort@ module uses
--   @Syntax@ and
--   @Prelude@ modules.
--
-- * @Syntax@ module uses
--   @Prelude@ module.
--
-- * @Data@ module uses
--   @Prelude@ module.
--
-- * @Prelude@ module uses
--   no modules in Koshucode.
--
-- Following picture shows reduced dependencies.
--
-- @
--      [Core]             [Base]
--        |                /    \\
--     Resource           /     Abort
--        |              /        |
--      Assert          Data    Token
--        |              \\        |
--      Relmap            \\     Syntax
--        |                \\     /
--      Content             Prelude
--        |
--      [Base]
-- @
--


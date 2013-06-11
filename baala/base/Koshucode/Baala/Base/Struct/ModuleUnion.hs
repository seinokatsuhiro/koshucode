{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Struct.ModuleUnion
( moduleUnion
, moduleUse
) where

import Koshucode.Baala.Base.Struct.Full.Module

moduleUnion :: Module v -> Module v -> Module v
moduleUnion m1 m2 =
    m1 { moduleName   = Nothing
       , moduleImport = []
       , moduleExport = union moduleExport
       , moduleAssert = union moduleAssert
       , moduleRelmap = union moduleRelmap
       , moduleJudge  = union moduleJudge
       } where union f = f m1 ++ f m2

moduleUse :: Module v -> Module v -> Module v
moduleUse m1 m2 =
    m1 { moduleName   = Nothing
       , moduleImport = []
       , moduleRelmap = union moduleRelmap
       } where union f = f m1 ++ f m2
        


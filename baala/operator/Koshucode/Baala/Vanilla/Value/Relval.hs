{-# OPTIONS_GHC -Wall #-}

-- | Relation on type 'Val'

module Koshucode.Baala.Vanilla.Value.Relval
( terms,
  unionUpTerm,
  module Koshucode.Baala.Vanilla.Value.Val,
  module Koshucode.Baala.Base.Prelude,
) where

import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)

import Koshucode.Baala.Vanilla.Value.Val

terms :: [String] -> [Relterm]
terms = map Term

unionUpTerm :: (Name a) => [String] -> [a] -> [Relterm]
unionUpTerm ns ts = map Term $ unionUp ns $ names ts


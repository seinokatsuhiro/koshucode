{-# OPTIONS_GHC -Wall #-}

-- | Relation on type 'Val'

module Koshucode.Baala.Vanilla.Type.Relval
( terms,
  unionUpTerm,
  module Koshucode.Baala.Vanilla.Type.Content,
) where

import Koshucode.Baala.Base hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Vanilla.Type.Content

terms :: [String] -> [Relterm]
terms = map Term

unionUpTerm :: (Name a) => [String] -> [a] -> [Relterm]
unionUpTerm ns ts = map Term $ unionUp ns $ names ts


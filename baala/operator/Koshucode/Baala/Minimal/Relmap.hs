{-# OPTIONS_GHC -Wall #-}

{-| Minimal relational operators. -}

module Koshucode.Baala.Minimal.Relmap
( -- * Utility
  module Koshucode.Baala.Minimal.Relmap.Get
, module Koshucode.Baala.Minimal.Relmap.Pattern
, module Koshucode.Baala.Minimal.Relmap.Operand

  -- * Implementation
, module Koshucode.Baala.Minimal.Relmap.Tropashko
, module Koshucode.Baala.Minimal.Relmap.Unary
, module Koshucode.Baala.Minimal.Relmap.Restrict
, module Koshucode.Baala.Minimal.Relmap.Implement

  -- * Naming conventions
  -- $NamingConventions
) where

import Koshucode.Baala.Minimal.Relmap.Get
import Koshucode.Baala.Minimal.Relmap.Pattern
import Koshucode.Baala.Minimal.Relmap.Operand

import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Implement



-- ----------------------
-- $NamingConventions
--
-- Functions for relational mapping are
-- named under some conventions.
-- Relational mapping is a kind of functions
-- that calculate relations from relations.
--
-- [@relXxx@]
-- Functions from 'Rel' to 'Rel'.
-- These are basic functions for relational operators.
--
-- [@relmapXxx@]
-- Functions from 'Relmap' to 'Relmap'.
-- 
-- [@consXxx@]
-- Functions of 'Koshucode.Baala.Core.Relmap.HalfRelmap.Relop'.
-- These functions construct 'Relmap' from operator usages,
-- but constructions are failed if operators are misused.
--
-- [@likeXxx@]
-- Operand parser.
--
-- [@LikeXxx@]
-- Operand pattern.


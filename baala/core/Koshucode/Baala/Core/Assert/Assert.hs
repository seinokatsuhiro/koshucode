{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

{-| Data structure for mapping relation to judges -}

module Koshucode.Baala.Core.Assert.Assert
( -- * Assert
  Assert (..),
  AssertOption,
  assertMap,

  -- * AssertType
  AssertType (..),
  assertQuality,

  -- * Constructor
  affirm,
  deny,
) where

import qualified Data.Generics as G
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core.Relmap as C



-- ----------------------  Assert

{-| Assertion of affirming or denying relation.
    It consists of logical quality, relsign, and relmap.

    See also 'B.Judge' -}
data Assert c = Assert
    { assertType    :: AssertType      -- ^ Logical quality
    , assertPattern :: B.JudgePattern  -- ^ Pattern of judgement
    , assertOption  :: AssertOption    -- ^ Assert option
    , assertRelmap  :: C.Relmap c      -- ^ Relmap
    , assertSource  :: [B.Token]       -- ^ Source code information
    } deriving (Show)

{-| Option for assertions. -}
type AssertOption = [B.Named [B.TokenTree]]

instance B.Pretty (Assert c) where
    doc (Assert q pat _ r _) =
        let qs = B.doch [assertText q, pat]
        in B.docHang qs 2 (B.doc r)

{-| Apply function to relamp in assert. -}
assertMap :: B.Map (C.Relmap c) -> B.Map (Assert c)
assertMap f (Assert q pat opt r src) = Assert q pat opt (f r) src



-- ----------------------  AssertType

data AssertType
    = AssertAffirm    -- ^ @|==@ /pattern/ @:@ /relmap/
    | AssertDeny      -- ^ @|=X@ /pattern/ @:@ /relmap/
    | AssertViolate   -- ^ @|=V@ /pattern/ @:@ /relmap/
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

assertQuality :: AssertType -> Bool
assertQuality AssertAffirm   = True
assertQuality AssertDeny     = False
assertQuality AssertViolate  = True

assertText :: AssertType -> String
assertText AssertAffirm   = "affirm"
assertText AssertDeny     = "deny"
assertText AssertViolate  = "violate"



-- ----------------------  Constructor

affirm, deny :: B.JudgePattern -> AssertOption
             -> C.Relmap c -> [B.Token] -> Assert c

{-| Make affirmed assertion. -}
affirm = Assert AssertAffirm

{-| Make denied assertion. -}
deny   = Assert AssertDeny


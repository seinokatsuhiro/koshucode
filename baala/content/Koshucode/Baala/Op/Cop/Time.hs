{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Time
  ( copsTime
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Msg



-- ----------------------
-- $Operators
--
--  [@mjd@]    Modified Jurian Day of time.
--

copsTime :: (C.CTime c, C.CDec c) => [C.Cop c]
copsTime =
    [ C.CopCalc  (C.copNormal "mjd")    copMJD
    ]

copMJD :: (C.CTime c, C.CDec c) => C.CopCalc c
copMJD [Right c] | C.isTime c = Right $ C.pDecFromInteger $ B.timeMJD $ C.gTime c
copMJD _ = Msg.unexpAttr "mjd"


{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Unary
( 
  -- * size
  relopSize, relmapSize, relSize,
  -- * conf
  relopConf, relmapConf, relConf,
  -- * enclose
  relopEnclose, relmapEnclose, relEnclose
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  size

relopSize :: Kit.Relop VContent
relopSize use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapSize use n

relmapSize :: (CInt v) => OpUse v -> String -> Relmap v
relmapSize use n = Kit.relmapCalc use "size" sub where
    sub _ = relSize n

{-| Change terms names -}
relSize
    :: (CInt v)
    => String          -- ^ List of term name (/to/, /from/)
    -> AbMap (Rel v)   -- ^ Relation to relation
relSize n (Rel _ b1) = Right $ Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[putInt $ length b1]]



-- ----------------------  conf

relopConf :: Kit.Relop VContent
relopConf use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapConf use n

relmapConf :: (CString v) => OpUse v -> String -> Relmap v
relmapConf use n = Kit.relmapCalc use "conf" sub where
    sub _ = relConf n

{-| Change terms names -}
relConf
    :: (CString v)
    => String          -- ^ Term name
    -> AbMap (Rel v)   -- ^ Relation to relation
relConf n (Rel h1 _) = Right $ Rel h2 b2 where
    h2 = Kit.headFrom [n]
    b2 = [[putString $ show s]]
    s  = show $ docParen $ doc h1



-- ----------------------  enclose

relopEnclose :: Kit.Relop VContent
relopEnclose use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapEnclose use n

relmapEnclose :: (CRel v) => OpUse v -> String -> Relmap v
relmapEnclose use n = Kit.relmapCalc use "enclose" sub where
    sub _ = relEnclose n

{-| Enclose the current relation in a term. -}
relEnclose
    :: (CRel v)
    => String          -- ^ Term name
    -> AbMap (Rel v)   -- ^ Relation to relation
relEnclose n r@(Rel h1 _) = Right $ Rel h2 b2 where
    h2 = Relhead [Nest n $ headTerms h1]
    b2 = [[putRel r]]


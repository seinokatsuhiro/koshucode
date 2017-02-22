{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for dec type.

module Koshucode.Baala.Rop.Cox.Type.Dec
  ( ropsTypeDec,
    -- * of-dec
    consOfDec, relmapOfDec, relkitOfDec,
    -- * alt-dec
    consAltDec, relmapAltDec, relkitAltDec,
    -- * to-dec
    consToDec, relmapToDec, relkitToDec,
    relmapToDecReplace, relkitToDecReplace,
  ) where

import qualified Data.Ratio                             as R
import qualified Koshucode.Baala.DataPlus               as K
import qualified Koshucode.Baala.Core                   as C
import qualified Koshucode.Baala.Rop.Base               as Rop
import qualified Koshucode.Baala.Rop.Base.Message       as Msg
import qualified Koshucode.Baala.Rop.Cox.Type.Utility   as Rop

-- | Implementation of relational operators.
ropsTypeDec :: (K.CContent c) => [C.Rop c]
ropsTypeDec = Rop.rops "type"
    [ consOfDec
      K.& [ "of-dec E [-sign /N] [-fracle /N] [-num /N] [-denom /N] [-int /N] [-frac /N]"
            K.& "-content . -sign? -fracle? -num? -denom? -int? -frac? -let?" ]
    , consAltDec
      K.& [ "alt-dec /P [-fracle F]"
            K.& "-term . -fracle? -let?" ]
    , consToDec
      K.& [ "to-dec /P ... [-replace E]"
            K.& "-term* . -replace?" ]
    ]


-- ----------------------  of-dec

-- | [of-dec /E/ -fracle \/N]  Add term \/N as fracle (fraction length) of decimal number /E/.
--   [of-dec /E/ -num \/N]     Add term \/N as numerator of decimal number /E/.
--   [of-dec /E/ -denom \/N]   Add term \/N as denominator of decimal number /E/.
--   [of-dec /E/ -int \/N]     Add term \/N as integral part of decimal number /E/.
--   [of-dec /E/ -frac \/N]    Add term \/N as fractional part of decimal number /E/.
--   [of-dec /E/ -sign \/N]    Add term \/N as sign of decimal number /E/, i.e., -1, 0, or 1.
--
consOfDec :: (K.CContent c) => C.RopCons c
consOfDec med =
  do cops     <- Rop.getLetR      med
     content  <- Rop.getCox       med "-content"
     sign     <- Rop.getMaybeTerm med "-sign"
     fracle   <- Rop.getMaybeTerm med "-fracle"
     num      <- Rop.getMaybeTerm med "-num"
     denom    <- Rop.getMaybeTerm med "-denom"
     int      <- Rop.getMaybeTerm med "-int"
     frac     <- Rop.getMaybeTerm med "-frac"
     Right $ relmapOfDec med (cops, content, [sign, fracle, num, denom, int, frac])

-- | Create @of-dect@ relmap.
relmapOfDec :: (K.CContent c) => C.Intmed c -> Rop.OfParam c -> C.Relmap c
relmapOfDec med = C.relmapFlow med . relkitOfDec

-- | Create @of-dec@ relkit.
relkitOfDec :: (K.CContent c) => Rop.OfParam c -> C.RelkitFlow c
relkitOfDec = Rop.relkitOfX K.getDec decContents

decContents :: (K.CContent c) => K.Ab K.Decimal -> [c]
decContents (Left _) = [K.empty, K.empty, K.empty, K.empty]
decContents (Right dec) = [sign, fracle, num, denom, int, frac] where
    sign    = K.pDec $ signum dec
    fracle  = K.pInt l
    num     = K.pInteger $ abs $ R.numerator ratio
    denom   = K.pInteger $ abs $ R.denominator ratio
    int     = K.pDec $ K.realDecimal (min 0 l) i
    frac    = K.pDec $ K.realDecimal l f

    (i, f)  = properFraction ratio :: (K.DecimalInteger, K.DecimalRatio)
    l       = K.decimalFracle dec
    ratio   = K.decimalRatio dec


-- ----------------------  alt-dec

-- | [alt-dec \/P -fracle /F/]    Alter fracle of decimal number to /F/ in term \/P.
--
consAltDec :: (K.CContent c) => C.RopCons c
consAltDec med =
    do cops    <- Rop.getLetR     med
       term    <- Rop.getTerm     med "-term"
       fracle  <- Rop.getMaybeCox med "-fracle"
       Right $ relmapAltDec med (cops, term, fracle)

-- | Create @alt-dec@ relmap.
relmapAltDec :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, K.TermName, K.MaybeCox c) -> C.Relmap c
relmapAltDec med = C.relmapFlow med . relkitAltDec

-- | Create @alt-dec@ relkit.
relkitAltDec :: (K.CContent c)
  => (K.CopSet c, K.TermName, K.MaybeCox c)
  -> C.RelkitFlow c
relkitAltDec _ Nothing = C.relkitUnfixed
relkitAltDec (cops, n, fracle) (Just he1) = Right kit2 where
      ns1       = K.getTermNames he1
      ind       = [n] `K.selectIndex` ns1
      pick      = K.selectElems    ind
      cut       = K.selectOthers   ind
      fore      = K.permuteForward ind
      he2       = K.headMap fore he1
      kit2      = C.relkitLineAb False he2 flow
      flow cs1  = do let run    = K.calcCox cops he1 cs1
                         [c]    = pick cs1
                         dec    = K.gDec c
                     f' <- getMaybe (getInt . run) fracle
                     let dec' = case f' of
                                  Nothing -> dec
                                  Just f  -> dec { K.decimalFracle = f }
                     Right $ K.pDec dec' : cut cs1

getMaybe :: (c -> K.Ab a) -> Maybe c -> K.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (K.CContent c) => K.Ab c -> K.Ab Int
getInt c = do d <- K.getDec c
              return $ fromInteger $ K.decimalNum d


-- ----------------------  to-dec

-- | [to-dec /\/P/ ...]
--      Convert content of term /\/P/ ... to decimal number.
--   [to-dec /\/P/ ... -replace /E/]
--      Convert content of term /\/P/ ... to decimal number,
--      or replace inconvertible content to /E/.
--
consToDec :: (K.CContent c) => C.RopCons c
consToDec med =
    do cops <- Rop.getLetR     med
       ns   <- Rop.getTerms    med "-term"
       rep' <- Rop.getMaybeCox med "-replace"
       Right $ case rep' of
         Nothing  -> relmapToDec med ns
         Just rep -> relmapToDecReplace med (cops, ns, rep)

-- | Create @to-dec@ relmap.
relmapToDec :: (K.CContent c) => C.Intmed c -> [K.TermName] -> C.Relmap c
relmapToDec med = C.relmapFlow med . relkitToDec

-- | Create @to-dec@ relkit.
relkitToDec :: (K.CContent c) => [K.TermName] -> C.RelkitFlow c
relkitToDec _ Nothing = C.relkitUnfixed
relkitToDec ns (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit
    where
      pk        = K.termPicker ns he1
      he2       = K.headMap (K.forwardTerms pk) he1
      kit       = C.relkitLine False he2 flow
      flow cs1  = let cs = K.toDec <$> K.pickTerms pk cs1
                  in cs ++ K.cutTerms pk cs1

-- | Create @to-dec@ relmap with @-replace@ option.
relmapToDecReplace :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.TermName], K.Cox c) -> C.Relmap c
relmapToDecReplace med = C.relmapFlow med . relkitToDecReplace

-- | Create @to-dec@ relkit with @-replace@ option.
relkitToDecReplace :: (K.CContent c) => (K.CopSet c, [K.TermName], K.Cox c) -> C.RelkitFlow c
relkitToDecReplace _ Nothing = C.relkitUnfixed
relkitToDecReplace (cops, ns, rep) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit
    where
      pk        = K.termPicker ns he1
      he2       = K.headMap (K.forwardTerms pk) he1
      kit       = C.relkitLineAb False he2 flow
      flow cs1  = do let run = K.calcCox cops he1 cs1
                     cRep <- run rep
                     let cs = K.toDecReplace cRep <$> K.pickTerms pk cs1
                     Right $ cs ++ K.cutTerms pk cs1


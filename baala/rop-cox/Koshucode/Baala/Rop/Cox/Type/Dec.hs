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
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg

-- | Implementation of relational operators.
ropsTypeDec :: (K.CContent c) => [C.Rop c]
ropsTypeDec = Rop.rops "type"
    [ consOfDec
      K.& [ "of-dec E [-sign /N] [-fracle /N] [-num /N] [-denom /N]"
            K.& "-content . -sign? -fracle? -num? -denom?" ]
    , consAltDec
      K.& [ "alt-dec /P [-fracle F]"
            K.& "-term . -fracle?" ]
    , consToDec
      K.& [ "to-dec /P ..."
            K.& "-term*" ]
    ]


-- ----------------------  of-dec

-- | [of-dec /E/ -fracle \/N]  Add term \/N as fracle (fraction length) of decimal number /E/.
--   [of-dec /E/ -num \/N]     Add term \/N as numerator of decimal number /E/.
--   [of-dec /E/ -denom \/N]   Add term \/N as denominator of decimal number /E/.
--   [of-dec /E/ -sign \/N]    Add term \/N as sign of decimal number /E/, i.e., -1, 0, or 1.
--
consOfDec :: (K.CContent c) => C.RopCons c
consOfDec med =
  do cops     <- Rop.getWhere   med "-where"
     content  <- Rop.getCox     med "-content"
     sign     <- Rop.getTermOpt med "-sign"
     fracle   <- Rop.getTermOpt med "-fracle"
     num      <- Rop.getTermOpt med "-num"
     denom    <- Rop.getTermOpt med "-denom"
     Right $ relmapOfDec med (cops, content, [sign, fracle, num, denom])

-- | Create @of-dect@ relmap.
relmapOfDec :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, K.Cox c, [Maybe K.TermName]) -> C.Relmap c
relmapOfDec med = C.relmapFlow med . relkitOfDec

-- | Create @of-dec@ relkit.
relkitOfDec :: (K.CContent c) =>
  (K.CopSet c, K.Cox c, [Maybe K.TermName]) -> C.RelkitFlow c
relkitOfDec _ Nothing = Right C.relkitNothing
relkitOfDec (cops, cox, ns) (Just he1) = Right kit2 where
      he2       = K.catMaybes ns `K.headAppend` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do dec <- K.getDec $ K.coxRunCox cops he1 cs1 cox
                     let cs2 = K.zipMaybe2 ns $ decContents dec
                     Right $ cs2 ++ cs1

decContents :: (K.CDec c) => K.Decimal -> [c]
decContents dec = [sign, fracle, num, denom] where
    sign    = K.pDec $ signum dec
    fracle  = K.pInt $ K.decimalFracle dec
    num     = K.pInteger $ abs $ R.numerator ratio
    denom   = K.pInteger $ abs $ R.denominator ratio
    ratio   = K.decimalRatio dec


-- ----------------------  alt-dec

-- | [alt-dec \/P -fracle /F/]    Alter fracle of decimal number to /F/ in term \/P.
--
consAltDec :: (K.CContent c) => C.RopCons c
consAltDec med =
    do cops    <- Rop.getWhere    med "-where"
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
relkitAltDec _ Nothing = Right C.relkitNothing
relkitAltDec (cops, n, fracle) (Just he1) = Right kit2 where
      ns1       = K.getTermNames he1
      ind       = [n] `K.selectIndex` ns1
      pick      = K.selectElems    ind
      cut       = K.selectOthers   ind
      fore      = K.permuteForward ind
      he2       = K.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do let run    = K.coxRunCox cops he1 cs1
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

-- | [to-dec \/P ...]    Convert content of term \/P ... to decimal number.
--
consToDec :: (K.CContent c) => C.RopCons c
consToDec med =
    do ns <- Rop.getTerms med "-term"
       Right $ relmapToDec med ns

-- | Create @to-dec@ relmap.
relmapToDec :: (K.CContent c) => C.Intmed c -> [K.TermName] -> C.Relmap c
relmapToDec med = C.relmapFlow med . relkitToDec

-- | Create @to-dec@ relkit.
relkitToDec :: (K.CContent c) => [K.TermName] -> C.RelkitFlow c
relkitToDec _ Nothing = Right C.relkitNothing
relkitToDec ns (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit2
    where
      pk     = K.termPicker ns he1
      he2    = K.headMap (K.forwardTerms pk) he1
      kit2   = C.relkitLinear he2 False f
      f cs1  = let cs = K.toDec <$> K.pickTerms pk cs1
               in cs ++ K.cutTerms pk cs1


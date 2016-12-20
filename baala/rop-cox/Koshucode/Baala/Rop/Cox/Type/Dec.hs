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

import qualified Data.Ratio                   as R
import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Rop
import qualified Koshucode.Baala.Rop.Cox.Get  as Rop

-- | Implementation of relational operators.
ropsTypeDec :: (D.CContent c) => [C.Rop c]
ropsTypeDec = Rop.rops "type"
    [ consOfDec
      O.& [ "of-dec E [-sign /N] [-fracle /N] [-num /N] [-denom /N]"
            O.& "-content . -sign? -fracle? -num? -denom?" ]
    , consAltDec
      O.& [ "alt-dec /P [-fracle F]"
            O.& "-term . -fracle?" ]
    , consToDec
      O.& [ "to-dec /P ..."
            O.& "-term*" ]
    ]


-- ----------------------  of-dec

-- | [of-dec /E/ -fracle \/N]  Add term \/N as fracle (fraction length) of decimal number /E/.
--   [of-dec /E/ -num \/N]     Add term \/N as numerator of decimal number /E/.
--   [of-dec /E/ -denom \/N]   Add term \/N as denominator of decimal number /E/.
--   [of-dec /E/ -sign \/N]    Add term \/N as sign of decimal number /E/, i.e., -1, 0, or 1.
--
consOfDec :: (D.CContent c) => C.RopCons c
consOfDec med =
  do cops     <- Rop.getWhere   med "-where"
     content  <- Rop.getCox     med "-content"
     sign     <- Rop.getTermOpt med "-sign"
     fracle   <- Rop.getTermOpt med "-fracle"
     num      <- Rop.getTermOpt med "-num"
     denom    <- Rop.getTermOpt med "-denom"
     Right $ relmapOfDec med (cops, content, [sign, fracle, num, denom])

-- | Create @of-dect@ relmap.
relmapOfDec :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.Relmap c
relmapOfDec med = C.relmapFlow med . relkitOfDec

-- | Create @of-dec@ relkit.
relkitOfDec :: (D.CContent c) =>
  (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.RelkitFlow c
relkitOfDec _ Nothing = Right C.relkitNothing
relkitOfDec (cops, cox, ns) (Just he1) = Right kit2 where
      he2       = B.catMaybes ns `D.headAppend` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do dec <- D.getDec $ D.coxRunCox cops he1 cs1 cox
                     let cs2 = B.zipMaybe2 ns $ decContents dec
                     Right $ cs2 ++ cs1

decContents :: (D.CDec c) => D.Decimal -> [c]
decContents dec = [sign, fracle, num, denom] where
    sign    = D.pDec $ signum dec
    fracle  = D.pInt $ D.decimalFracle dec
    num     = D.pInteger $ abs $ R.numerator ratio
    denom   = D.pInteger $ abs $ R.denominator ratio
    ratio   = D.decimalRatio dec


-- ----------------------  alt-dec

-- | [alt-dec \/P -fracle /F/]    Alter fracle of decimal number to /F/ in term \/P.
--
consAltDec :: (D.CContent c) => C.RopCons c
consAltDec med =
    do cops    <- Rop.getWhere    med "-where"
       term    <- Rop.getTerm     med "-term"
       fracle  <- Rop.getMaybeCox med "-fracle"
       Right $ relmapAltDec med (cops, term, fracle)

-- | Create @alt-dec@ relmap.
relmapAltDec :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, D.MaybeCox c) -> C.Relmap c
relmapAltDec med = C.relmapFlow med . relkitAltDec

-- | Create @alt-dec@ relkit.
relkitAltDec :: (D.CContent c)
  => (D.CopSet c, S.TermName, D.MaybeCox c)
  -> C.RelkitFlow c
relkitAltDec _ Nothing = Right C.relkitNothing
relkitAltDec (cops, n, fracle) (Just he1) = Right kit2 where
      ns1       = D.getTermNames he1
      ind       = [n] `B.selectIndex` ns1
      pick      = B.selectElems    ind
      cut       = B.selectOthers   ind
      fore      = B.permuteForward ind
      he2       = D.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do let run    = D.coxRunCox cops he1 cs1
                         [c]    = pick cs1
                         dec    = D.gDec c
                     f' <- getMaybe (getInt . run) fracle
                     let dec' = case f' of
                                  Nothing -> dec
                                  Just f  -> dec { D.decimalFracle = f }
                     Right $ D.pDec dec' : cut cs1

getMaybe :: (c -> B.Ab a) -> Maybe c -> B.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (D.CContent c) => B.Ab c -> B.Ab Int
getInt c = do d <- D.getDec c
              return $ fromInteger $ D.decimalNum d


-- ----------------------  to-dec

-- | [to-dec \/P ...]    Convert content of term \/P ... to decimal number.
--
consToDec :: (D.CContent c) => C.RopCons c
consToDec med =
    do ns <- Rop.getTerms med "-term"
       Right $ relmapToDec med ns

-- | Create @to-dec@ relmap.
relmapToDec :: (D.CContent c) => C.Intmed c -> [S.TermName] -> C.Relmap c
relmapToDec med = C.relmapFlow med . relkitToDec

-- | Create @to-dec@ relkit.
relkitToDec :: (D.CContent c) => [S.TermName] -> C.RelkitFlow c
relkitToDec _ Nothing = Right C.relkitNothing
relkitToDec ns (Just he1) = Right kit2 where
      ns1       = D.getTermNames he1
      ind       = B.selectIndex ns ns1
      pick      = B.selectElems    ind
      cut       = B.selectOthers   ind
      fore      = B.permuteForward ind
      he2       = D.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitLinear False f2
      f2 cs1    = let cs = D.toDec <$> pick cs1
                  in cs ++ cut cs1


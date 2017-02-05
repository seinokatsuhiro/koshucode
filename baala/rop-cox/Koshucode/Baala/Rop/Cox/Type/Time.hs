{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for time type.

module Koshucode.Baala.Rop.Cox.Type.Time
  ( ropsTypeTime,
    -- * add-time
    consAddTime, relmapAddTime, relkitAddTime,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop

-- | Implementation of relational operators.
ropsTypeTime :: (K.CContent c) => [C.Rop c]
ropsTypeTime = Rop.rops "type"
    [ consAddTime
      K.& [ "add-time /N -year E -month E [-day E] [-clock E]"
            K.& "-time . -year -month -day? -clock?" ]
    ]


-- ----------------------  time

-- | [add-time \/c -year /Y/ -month /M/ -day /D/]
--     Add term \/c as a time of /Y/-/M/-/D/.

consAddTime :: (K.CContent c) => C.RopCons c
consAddTime med =
    do cops   <- Rop.getLetR     med
       n      <- Rop.getTerm     med "-time"
       year   <- Rop.getCox      med "-year"
       month  <- Rop.getMaybeCox med "-month"
       day    <- Rop.getMaybeCox med "-day"
       clock  <- Rop.getMaybeCox med "-clock"
       Right $ relmapAddTime med (cops, n, (year, month, day, clock))

-- | Create @time@ relmap.
relmapAddTime :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, K.TermName, (K.Cox c, K.MaybeCox c, K.MaybeCox c, K.MaybeCox c)) -> C.Relmap c
relmapAddTime med = C.relmapFlow med . relkitAddTime

-- | Create @time@ relkit.
relkitAddTime :: (K.CContent c)
  => (K.CopSet c, K.TermName, (K.Cox c, K.MaybeCox c, K.MaybeCox c, K.MaybeCox c))
  -> C.RelkitFlow c
relkitAddTime _ Nothing = C.relkitUnfixed
relkitAddTime (cops, n, (year, month, day, clock)) (Just he1) = kit where
    pk     = K.termPicker [n] he1
    he2    = n `K.headCons` he1
    kit    = Rop.newCheck pk $ Right $ C.relkitLineAb False he2 f
    f cs1  = do let run = K.calcCox cops he1 cs1
                y <- getInteger $ run year
                m <- getMaybe (getInt . run) month
                d <- getMaybe (getInt . run) day
                c <- getMaybe (K.getClock . run) clock
                time <- timeFrom y m d c
                Right $ K.pTime time : cs1

timeFrom :: Integer -> Maybe Int -> Maybe Int -> Maybe K.Clock -> K.Ab K.Time
timeFrom y (Just m) (Just d) (Just c)    = do date <- K.ymdDate y m d
                                              Right $ K.timeSetClock c $ K.dTime date
timeFrom y (Just m) (Just d) (Nothing)   = K.dTime <$> K.ymdDate y m d
timeFrom y (Just m) (Nothing) (Nothing)  = K.dTime <$> K.ymDate y m
timeFrom _ _ _ _ = K.bug "timeFrom"

getMaybe :: (c -> K.Ab a) -> Maybe c -> K.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (K.CContent c) => K.Ab c -> K.Ab Int
getInt c = do d <- K.getDec c
              return $ fromInteger $ K.decimalNum d

getInteger :: (K.CContent c) => K.Ab c -> K.Ab Integer
getInteger c = do i <- getInt c
                  return $ toInteger i


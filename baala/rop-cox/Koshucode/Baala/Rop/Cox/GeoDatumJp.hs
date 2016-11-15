{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Conversion geographic point
--   from the plane rectangular coordinates @\/n@ @\/x@ @\/y@
--   to the geographic coordinates @\/latitude@ @\/longtitude@.
--   This program is based on the method described in
--   /Kazushige KAWASE, 2011,/
--   /A More Concise Method of Calculation for the Coordinate/
--   /Conversion between Geographic and Plane Rectangular/
--   /Coordinates on the Gauss-Krüger Projection,/
--   /Bulletin of the Geospatial Information Authority of Japan, 121/.
--
module Koshucode.Baala.Rop.Cox.GeoDatumJp
 ( GeoPoint,
   convDegree, convRadian,
 ) where

-- | Point in geographic coordinate system.
type GeoPoint = (Double, Double)

theLat, theLong :: GeoPoint -> Double
theLat  = fst
theLong = snd

-- | Convert point in plane rectangular coordinates
--   to degrees of latitude and longitude.
--
--   >>> convDegree 8 (0.0, 0.0)
--   (35.99999999999999, 138.5)
--
convDegree :: Int -> GeoPoint -> GeoPoint
convDegree coord (x, y) =
    case convRadian coord (x, y) of
      (lat, long) -> (lat / s2r / 3600, long / 3600)

-- | Convert point in plane rectangular coordinates
--   to geographic latitude and longitude.
--
--   >>> convRadian 8 (0.0, 0.0)
--   (0.6283185307179585, 498600.0)
--
convRadian :: Int -> GeoPoint -> GeoPoint
convRadian coord (x, y) = (lat, long) where

    lat = chi + s where
        chi = asin (sin xi' / cosh eta')
        s = sum $ map f $ zip [1..] deltaList
        f (j, delta) = delta * sin (2 * j * chi)

    long = let a2 = atan2 (sinh eta') (cos xi')
           in 60 * (theLong $ origin !! coord) + a2 / s2r

    eta' = eta - s where
        s = sum $ map f $ zip [1..] betaList
        f (j, beta) = let x1 = 2 * j * xi
                          x2 = 2 * j * eta
                      in beta * cos x1 * sinh x2

    xi' = xi - s where
        s = sum $ map f $ zip [1..] betaList
        f (j, beta) = let x1 = 2 * j * xi
                          x2 = 2 * j * eta
                      in beta * sin x1 * cosh x2

    eta = y / ra

    xi  = let l = theLat $ origin !! coord
          in (x + m0 * meridian (2 * l * 3600 * s2r)) / ra

meridian :: Double -> Double
meridian phi2 = mer where
    mer            = anh * (phi2 + s)
    s              = sum $ zipWith3 p3 c1 c1 c2
    p3 x y z       = x * y * z

    c1             = reverse $ opList (*) 1 eList1
    c2             = map c2Calc $ c3
    c3             = opList2 (/) (*) 2 `map` [e 0, e 1, e 2, e 3, e 4]

    c2Calc c3sub   = phi2 + (sum $ zipWith (*) c3sub tList)

    tList          = map tCalc $ zip [1..10] sList
    tCalc (l, sl)  = (1 / l - 4 * l) * sl

    sList          = tail $ sCalc 0 (sin phi2)
    sCalc x y      = x : sCalc y (dc * y - x)
    dc             = 2 * cos phi2

    e k            = alternate (drop k eList1') ((reverse $ take k eList1') ++ eList2)
    eList1'        = reverse eList1
    eList1         = map eCalc1 [1..5]
    eList2         = map eCalc2 [1..5]
    eCalc1 k       = 1.5 * n1 / k - n1
    eCalc2 k       = 1.5 * n1 / (k + 5) - n1


-- --------------------------------------------  Parameter

betaList :: [Double]
betaList =
    [ (1/2 + (-2/3 + (37/96 + (-1/360 - 81/512 * n1) * n1) * n1) * n1) * n1
    , (1/48 + (1/15 + (-437/1440 + 46/105 * n1) * n1) * n1) * n2
    , (17 / 480 + (-37 / 840 - 209 / 4480 * n1) * n1) * n3
    , (4397 / 161280 - 11 / 504 * n1) * n4
    , 4583 / 161280 * n5 ]

deltaList :: [Double]
deltaList =
    [ (2 + (-2/3 + (-2 + (116/45 + (26/45 - 2854/675 * n1) * n1) * n1) * n1) * n1) * n1
    , (7/3 + (-8/5 + (-227/45 + (2704/315 + 2323/945 * n1) * n1) * n1) * n1) * n2
    , (56/15 + (-136/35 + (-1262/105 + 73814/2835 * n1) * n1) * n1) * n3
    , (4279/630 + (-332/35 - 399572/14175 * n1) * n1) * n4
    , (4174/315 - 144838/6237 * n1) * n5
    , 601676/22275 * n6 ]

origin :: [GeoPoint]
origin = [ (0, 0)
         , (33, 7770), (33, 7860), (36, 7930), (33, 8010), (36, 8060)
         , (36, 8160), (36, 8230), (36, 8310), (36, 8390), (40, 8450)
         , (44, 8415), (44, 8535), (44, 8655), (26, 8520), (26, 7650)
         , (26, 7440), (26, 7860), (20, 8160), (26, 9240) ]

rf, n1, n2, n3, n4, n5, n6 :: Double
rf   = 298.257222101
n1   = 0.5 / (rf - 0.5)
n2   = n1 * n1
n3   = n1 * n2
n4   = n1 * n3
n5   = n1 * n4
n6   = n1 * n5

a, m0, s2r, anh, ra :: Double
a    = 6378137
m0   = 0.9999
s2r  = pi / 648000
anh  = 0.5 * a / (1 + n1)
ra   = 2 * anh * m0 * (1 + n2 / 4 + n4 / 64)


-- --------------------------------------------  Utility

type Op a = a -> a -> a

opList :: Op a -> a -> [a] -> [a]
opList op = loop where
    loop p (x:xs) = let px = p `op` x in px : loop px xs
    loop _ [] = []

opList2 :: Op a -> Op a -> a -> [a] -> [a]
opList2 op1 op2 = loop1 where
    loop1 p (x:xs) = let px = p `op1` x in px : loop2 px xs
    loop1 _ [] = []

    loop2 p (x:xs) = let px = p `op2` x in px : loop1 px xs
    loop2 _ [] = []

alternate :: [a] -> [a] -> [a]
alternate (x:xs) (y:ys) = x : y : alternate xs ys
alternate _ _ = []

{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.TermPos
( 
  -- * Position
  TermPos (..),
  posPoss,
  csPick,
  csCut,
  termsInner,
  termsOuter,
) where

import Koshucode.Baala.Base.Prelude



-- ----------------------  Term position

{-| Term position -}
data TermPos = TermPos
    { posName   :: String    -- ^ Term name
    , posIndex  :: Int       -- ^ Position
    } deriving (Show, Eq, Ord)

instance Name TermPos where
    name (TermPos n _) = n

{-| Indicies -}
posPoss  :: [TermPos] -> [Int]
posPoss  = map posIndex

{-| Pick an inner part. -}
termsInner :: [TermPos] -> [[String]]
termsInner = termsFilter isInner

{-| Pick an outer part. -}
termsOuter :: [TermPos] -> [[String]]
termsOuter = termsFilter isOuter

termsFilter :: (TermPos -> Bool) -> [TermPos] -> [[String]]
termsFilter p = map (singleton . name) . filter p

isInner, isOuter :: TermPos -> Bool
isInner (TermPos _ i)  =  i >= 0
isOuter (TermPos _ i)  =  i <  0

{-| Pick contents by positions. -}
csPick :: [TermPos] -> Map [c]
csPick  =  indexPick . map posIndex

{-| Cut contents by positions. -}
csCut  :: [TermPos] -> Map [c]
csCut   =  indexCut . map posIndex


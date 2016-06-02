{-# OPTIONS_GHC -Wall #-}

-- | Text table

module Koshucode.Baala.Base.Text.TextTable
  ( -- * Position of contents
    Position (..),
    textPos,
  
    -- * Constructing cells
    Cell (..),
  
    textTable,
    textTableWithHead,
    textCell,
    textRuleCell,
    textBlockCell,
    textBlockCellPlus,
  
    -- * Rendering tables
    alignTable,
    renderTable,
  ) where

import qualified Data.Char                      as C
import qualified Data.List                      as L
import qualified Koshucode.Baala.Base.List      as B
import qualified Koshucode.Baala.Base.Prelude   as B


-- ---------------------------------  Position

data Position
    = Front | Middle | Rear
      deriving (Show, Eq, Ord)

type Measure a = [a] -> Int

textPos :: String -> [Position]
textPos = map p where
    p c | c `elem` "<^" = Front
        | c `elem` ">v" = Rear
        | otherwise     = Middle

position :: Measure a -> a -> Position -> Int -> [a] -> [a]
position measure x pos size xs = p pos where
    p Front  =         xs ++ pad
    p Middle = left ++ xs ++ right
    p Rear   = pad  ++ xs

    pad      = padding measure x size xs
    (l, r)   = divideBy2 $ paddingSize measure size xs
    left     = replicate l x
    right    = replicate r x

padding :: Measure a -> a -> Int -> [a] -> [a]
padding measure x size xs = replicate n x where
    n = paddingSize measure size xs

paddingSize :: Measure a -> Int -> [a] -> Int
paddingSize measure size xs = max 0 $ size - measure xs

divideBy2 :: Int -> (Int, Int)
divideBy2 x
    | x `rem` 2 == 0  = (y, y)
    | otherwise       = (y, y + 1)
    where y = x `div` 2


-- ---------------------------------  Cell

data Cell = Cell {
      cellText   :: [String]
    , cellWidth  :: Int
    , cellHeight :: Int
    , cellPos    :: Position
    , cellPad    :: Char
    } deriving (Show, Eq, Ord)

emptyCell :: Cell
emptyCell = Cell [] 0 0 Front ' '

-- constructor / text to cell

textTable :: [Position] -> [[String]] -> [[Cell]]
textTable ps = map (textRow ps)

textTableWithHead :: [Position] -> Char -> [String] -> [[String]] -> [[Cell]]
textTableWithHead ps pad label body = h ++ r ++ b where
    h = textTable ps [label]
    r = [replicate (length label) $ textRuleCell pad]
    b = textTable ps body

textRow :: [Position] -> [String] -> [Cell]
textRow ps xs = map f $ zip ps xs where
    f (pos, text) = textCell pos text

textCell :: Position -> String -> Cell
textCell pos s =
    Cell { cellText   = [s]
         , cellWidth  = displaySize s
         , cellHeight = 1
         , cellPos    = pos
         , cellPad    = ' ' }

textRuleCell :: Char -> Cell
textRuleCell c = cell { cellPad = c } where
    cell = textCell Front [c]

textBlockCell :: Position -> [String] -> Cell
textBlockCell = textBlockCellPlus 0

textBlockCellPlus :: Int -> Position -> [String] -> Cell
textBlockCellPlus n pos xs =
    Cell { cellText   = xs
         , cellWidth  = maximum $ map displaySize xs
         , cellHeight = n + length xs
         , cellPos    = pos
         , cellPad    = ' ' }


-- ---------------------------------  Alignment

-- alignment / cell to cell

alignTable :: [[Cell]] -> [[Cell]]
alignTable =
    withTranspose (map alignWidth)
     . map alignHeight
     . transposable emptyCell

alignWidth :: [Cell] -> [Cell]
alignWidth cs = map rewidth cs where
    rewidth c = c { cellWidth = maxWidth }
    maxWidth  = maximum $ map cellWidth cs

alignHeight :: [Cell] -> [Cell]
alignHeight cs = map reheight cs where
    reheight c = c { cellHeight = maxHeight }
    maxHeight  = maximum $ map cellHeight cs

-- render / cell to text

renderTable :: String -> [[Cell]] -> [String]
renderTable vrule = concatMap (renderRow vrule)

renderRow :: String -> [Cell] -> [String]
renderRow vrule =
    map (L.intercalate vrule)
     . L.transpose
     . B.mapWithLast renderCell (renderCell . unpad)

renderCell :: Cell -> [String]
renderCell (Cell texts wd ht pos pad) = map width $ height texts where
    width  = position displaySize pad pos wd
    height = position length "" Front ht

unpad :: B.Map Cell
unpad cell
    | cellPad cell == ' ' = cell { cellWidth = 0 }
    | otherwise           = cell


-- ---------------------------------  Utility

withTranspose :: ([[a]] -> [[b]]) -> [[a]] -> [[b]]
withTranspose f = L.transpose . f . L.transpose

transposable :: a -> [[a]] -> [[a]]
transposable pad tab = map p tab where
    p = position length pad Front n
    n = maximum $ map length tab

displaySize :: Measure Char
displaySize = sum . map displaySizeChar      

displaySizeChar :: Char -> Int
displaySizeChar c
    | n < 4096  = 1
    | otherwise = 2
    where n = C.ord c


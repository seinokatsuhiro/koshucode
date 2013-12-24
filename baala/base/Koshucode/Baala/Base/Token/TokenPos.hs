{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.TokenPos
(
  -- * Token position
  TokenPos (..),
  tokenPosLineNumber,
  tokenPosLineText,
  tokenPosColumn,
  tokenPosDisplay,
  tokenPosZero,
) where

import Data.Generics (Data, Typeable)
import qualified Data.Monoid                   as M
import qualified Koshucode.Baala.Base.Syntax   as B


data TokenPos = TokenPos
      { tokenPosLine    :: B.NumberedLine    -- ^ Line number and content
      , tokenPosText    :: String            -- ^ Text at which begins token
      , tokenPosNumber  :: B.TokenNumber
      } deriving (Show, Eq, Data, Typeable)

instance Ord TokenPos where
    compare p1 p2
        = (tokenPosLineNumber p1 `compare` tokenPosLineNumber p2)
          `M.mappend` (tokenPosTextLength p2 `compare` tokenPosTextLength p1)

tokenPosLineNumber :: TokenPos -> Int
tokenPosLineNumber = fst . tokenPosLine

tokenPosLineText   :: TokenPos -> String
tokenPosLineText   = snd . tokenPosLine

tokenPosTextLength :: TokenPos -> Int
tokenPosTextLength = length . tokenPosText

tokenPosColumn :: TokenPos -> Int
tokenPosColumn TokenPos { tokenPosLine = (_, line), tokenPosText = subline }
    = length line - length subline

tokenPosDisplay :: TokenPos -> String
tokenPosDisplay p = show lno ++ " " ++ show cno ++ " " ++ text where
    lno  = tokenPosLineNumber p
    cno  = tokenPosColumn p
    text = tokenPosText p

tokenPosZero :: TokenPos
tokenPosZero = TokenPos (0, "") "" 0



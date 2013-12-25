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

  -- * Resource
  Resource (..),
  resourceType,
  resourceName,
) where

import qualified Data.Generics                 as G
import qualified Data.Monoid                   as M
import qualified Koshucode.Baala.Base.Syntax   as B


-- ----------------------  TokenPos

data TokenPos = TokenPos
      { tokenPosResource :: Resource
      , tokenPosLine     :: B.NumberedLine  -- ^ Line number and content
      , tokenPosText     :: String          -- ^ Text at which begins token
      } deriving (Show, Eq, G.Data, G.Typeable)

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

tokenPosDisplay :: TokenPos -> [String]
tokenPosDisplay p = [pos, "> " ++ text] where
    pos   = show lno ++ " " ++ show cno ++ " " ++ res
    lno   = tokenPosLineNumber p
    cno   = tokenPosColumn p
    res   = resourceName $ tokenPosResource p
    text  = tokenPosText p

tokenPosZero :: TokenPos
tokenPosZero = TokenPos (ResourceText "") (0, "") ""


-- ----------------------  Resource

data Resource
    = ResourceFile String
    | ResourceText String
    | ResourceURL  String
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

resourceType :: Resource -> String
resourceType (ResourceFile _) = "file"
resourceType (ResourceText _) = "text"
resourceType (ResourceURL _)  = "url"

resourceName :: Resource -> String
resourceName (ResourceFile path) = path
resourceName (ResourceText text) = text
resourceName (ResourceURL url)   = url



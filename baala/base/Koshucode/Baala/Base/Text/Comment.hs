{-# OPTIONS_GHC -Wall #-}

-- | Simple document used in comments.

module Koshucode.Baala.Base.Text.Comment
  ( Texts (..),
    CommentDoc (..),
    CommentSec (..),
    emacsModeComment,
    commentLine,
    putCommentLines, hPutCommentLines,
  ) where

import qualified System.IO                 as IO
import qualified Koshucode.Baala.Overture  as O

-- | Something that can become a string list.
class Texts a where
    texts :: a -> [String]

-- | Simple document in comment.
--
--   >>> texts $ CommentDoc [CommentSec "SAMPLE" ["This is a sample section."]]
--   [ "**"
--   , "**  SAMPLE"
--   , "**    This is a sample section."
--   , "**"
--   ]
--
data CommentDoc =
    CommentDoc [CommentSec]
    deriving (Show, Eq, Ord)

instance Texts CommentDoc where
    texts (CommentDoc ss) =
        let ls = "" : concatMap texts ss
        in map (prepend "**" "  ") ls

-- | Section title and its contents.
data CommentSec =
    CommentSec String [String]
    deriving (Show, Eq, Ord)

instance Texts CommentSec where
    texts (CommentSec title xs) =
        title : map (prepend "" "  ") (xs ++ [""])

prepend :: [a] -> [a] -> [a] -> [a]
prepend a _ [] = a
prepend a b xs = a ++ b ++ xs

-- | Comment string @-*- koshu -*-@ for The Emacs editor.
emacsModeComment :: String
emacsModeComment = "** -*- koshu -*-"

-- | Wrap string in comment.
--
--   >>> commentLine "Hello Koshu"
--   "**  Hello Koshu"
--
commentLine :: O.StringMap
commentLine "" = "**"
commentLine s  = "**  " ++ s

-- | Print strings as comment lines.
--
--   >>> putCommentLines ["Hello", "Koshu"]
--   **  Hello
--   **  Koshu
--
putCommentLines :: [String] -> IO ()
putCommentLines = O.putLines . map commentLine

-- | Print strings as comment lines.
hPutCommentLines :: IO.Handle -> [String] -> IO ()
hPutCommentLines h = O.hPutLines h . map commentLine

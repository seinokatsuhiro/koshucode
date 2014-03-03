{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Comment
( Texts (..),
  CommentDoc (..),
  CommentSec (..),
  emacsModeComment,
) where

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

emacsModeComment :: String
emacsModeComment = "** -*- koshu -*-"


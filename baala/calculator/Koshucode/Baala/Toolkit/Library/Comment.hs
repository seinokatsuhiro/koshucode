{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Toolkit.Library.Comment
( CommentDoc (..)
, CommentSec (..)
, Texts (..)
, emacsModeComment
) where

data CommentDoc =
    CommentDoc [CommentSec]
    deriving (Show, Eq, Ord)

data CommentSec =
    CommentSec String [String]
    deriving (Show, Eq, Ord)

class Texts a where
    texts :: a -> [String]

instance Texts CommentDoc where
    texts (CommentDoc ss) =
        let ls = "" : concatMap texts ss
        in map ("**  " ++) ls

instance Texts CommentSec where
    texts (CommentSec title xs) =
        title : map ("  " ++) (xs ++ [""])

emacsModeComment :: String
emacsModeComment = "** -*- koshu -*-"


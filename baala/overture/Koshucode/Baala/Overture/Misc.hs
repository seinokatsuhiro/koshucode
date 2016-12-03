{-# OPTIONS_GHC -Wall #-}

-- | Miscellaneous functions.

module Koshucode.Baala.Overture.Misc
 ( printList
 ) where

-- | Print list.
--
--   >>> printList ["foo", "bar", "baz"]
--   "foo"
--   "bar"
--   "baz"
--
printList :: (Show a) => [a] -> IO ()
printList = mapM_ print

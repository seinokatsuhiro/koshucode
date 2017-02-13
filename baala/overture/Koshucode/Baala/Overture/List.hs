{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

{-| List class. -}

module Koshucode.Baala.Overture.List
 ( -- * List class
   List (..),
   (<:>),
   cut2, cut3,
   -- * List-like class.
   L.ListLike (..),
 ) where

import qualified Data.ListLike                        as L
import qualified Data.Text                            as Tx
import qualified Data.Text.Lazy                       as Tz
import qualified Koshucode.Baala.Overture.Infix       as O
import qualified Koshucode.Baala.Overture.Shorthand   as O

{-| List class. -}
class (L.ListLike es e) => List es e | es -> e where

    {-| Split first element and rest of list.

        >>> cut "foo"
        Just ('f',"oo")

        >>> cut ""
        Nothing
        -}
    cut :: es -> Maybe (e, es)
    cut = L.uncons

instance List [e] e where

instance List O.Tx Char where
    cut = Tx.uncons

instance List O.Tz Char where
    cut = Tz.uncons

infixr 5 <:>

{-| Add element to list.

    >>> 'b' <:> "ar" :: String
    "bar"
    -}
(<:>) :: (List es e) => e -> es -> es
(<:>) = L.cons

{-| Split first two elements and rest of list.

    >>> cut2 "bar"
    Just ('b', Just ('a', "r"))
    -}
cut2 :: (List es e) => es -> Maybe (e, Maybe (e, es))
cut2 es = cut O.<$$> cut es

{-| Split first three elements and rest of list.
    
    >>> cut3 "bar"
    Just ('b', Just ('a', Just ('r', "")))

    >>> cut3 "b"
    Just ('b', Nothing)
    -}
cut3 :: (List es e) => es -> Maybe (e, Maybe (e, Maybe (e, es)))
cut3 es = cut2 O.<$$> cut es

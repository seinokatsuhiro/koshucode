{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

{-| List class. -}

module Koshucode.Baala.Overture.List
 ( List,
   (<:>),
   cut, cut2, cut3,
 ) where

import qualified Data.ListLike                        as L
import qualified Koshucode.Baala.Overture.Infix       as O

{-| List class as alias of 'L.ListLike'. -}
type List es e = L.ListLike es e

infixr 5 <:>

{-| Add element to list.

    >>> 'b' <:> "ar" :: String
    "bar"
    -}
(<:>) :: (List es e) => e -> es -> es
(<:>) = L.cons

{-| Split first element and rest of list.

    >>> cut "foo"
    Just ('f',"oo")

    >>> cut ""
    Nothing
    -}
cut :: (List es e) => es -> Maybe (e, es)
cut = L.uncons

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

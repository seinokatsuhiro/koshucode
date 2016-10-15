{-# OPTIONS_GHC -Wall #-}

-- | Named function.

module Koshucode.Baala.Overture.Fn
 ( O.Name,
   Fn (..),
   Fn2 (..),
 ) where

import qualified Koshucode.Baala.Overture.Name.String as O


-- ----------------------  Fn

-- | Named function.
data Fn a b = Fn O.Name (a -> b)

-- | Get function name.
instance O.GetName (Fn a b) where
    getName (Fn n _) = n

-- | Show function name.
instance Show (Fn a b) where
    show f = "Fn " ++ show (O.getName f)

-- | Equality by function name.
instance Eq (Fn a b) where
    f1 == f2 = O.getName f1 == O.getName f2

-- | Ordering by function name.
instance Ord (Fn a b) where
    compare f1 f2 = compare (O.getName f1) (O.getName f2)


-- ----------------------  Fn2

-- | Two-argument named function.
data Fn2 a b c = Fn2 O.Name (a -> b -> c)

-- | Get function name.
instance O.GetName (Fn2 a b c) where
    getName (Fn2 n _) = n

-- | Show function name.
instance Show (Fn2 a b c) where
    show f = "Fn2 " ++ show (O.getName f)

-- | Equality by function name.
instance Eq (Fn2 a b c) where
    f1 == f2 = O.getName f1 == O.getName f2

-- | Ordering by function name.
instance Ord (Fn2 a b c) where
    compare f1 f2 = compare (O.getName f1) (O.getName f2)


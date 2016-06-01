{-# OPTIONS_GHC -Wall #-}

-- | Singleton content type.

module Koshucode.Baala.Data.Content.Singleton
  ( -- * Type
    CTypeOf (..),
    getAbAb,
  
    -- * Empty and End
    CEmpty (..), maybeEmpty, omitEmpty,
    CEnd (..),
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Type            as D
import qualified Koshucode.Baala.Data.Content.Message as Msg


-- --------------------------------------------  Type

class (Show c, B.Write c) => CTypeOf c where
    typeOf :: c -> D.Type

getAbAb :: (CTypeOf c) => (c -> Bool) -> (c -> b) -> B.Ab c -> B.Ab b
getAbAb _ _ (Left reason) =  Left reason
getAbAb is get (Right x)
    | is x = Right $ get x
    | otherwise = Msg.unmatchType $ show $ B.doc $ typeOf x


-- --------------------------------------------  Empty and End

-- | Empty: the minimum content.
class (CTypeOf c) => CEmpty c where
    isEmpty     ::          c -> Bool
    empty       ::          c

maybeEmpty :: (CEmpty c) => (a -> c) -> Maybe a -> c
maybeEmpty f (Just a)   = f a
maybeEmpty _ (Nothing)  = empty

omitEmpty :: (CEmpty c) => B.Map [(a, c)]
omitEmpty = B.omit (isEmpty . snd)

-- | End of everything: the maximum content.
class (CTypeOf c) => CEnd c where
    isEnd       ::          c -> Bool
    end         ::          c


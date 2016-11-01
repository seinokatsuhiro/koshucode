{-# OPTIONS_GHC -Wall #-}

-- | Singleton content type.

module Koshucode.Baala.Data.Class.Singleton
  ( -- * Type
    CTypeOf (..),
    getContent,
  
    -- * Empty and End
    CEmpty (..), maybeEmpty, omitEmpty, contMaximum,
    CEnd (..), contMinimum, 
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data.Type            as D
import qualified Koshucode.Baala.Data.Class.Message   as Msg


-- --------------------------------------------  Type

-- | Classifiable into 'D.Type'.
class CTypeOf c where
    -- | Get type of content.
    typeOf :: c -> D.Type

-- | Get content which may be aborted.
getContent :: (CTypeOf c) => O.Test c -> (c -> b) -> B.Ab c -> B.Ab b
getContent _ _    (Left a) = Left a
getContent is get (Right c)
    | is c      = Right $ get c
    | otherwise = let s = B.mixToFlatString $ B.mixEncode $ typeOf c
                  in Msg.unmatchType s


-- --------------------------------------------  Empty and End

-- | Empty: the minimum content.
class (CTypeOf c) => CEmpty c where
    isEmpty     :: c -> Bool
    empty       :: c

-- | Create empty or non-empty content.
maybeEmpty :: (CEmpty c) => (a -> c) -> Maybe a -> c
maybeEmpty f (Just a)   = f a
maybeEmpty _ (Nothing)  = empty

-- | Cut empty terms.
omitEmpty :: (CEmpty c) => O.Map [S.Term c]
omitEmpty = B.omit (isEmpty . snd)

-- | Maximum content of contents list.
contMaximum :: (Ord c, CEmpty c) => [c] -> c
contMaximum = B.maximumNull empty

-- | End of everything: the maximum content.
class (CTypeOf c) => CEnd c where
    isEnd       :: c -> Bool
    end         :: c

-- | Minimum content of contents list.
contMinimum :: (Ord c, CEnd c) => [c] -> c
contMinimum = B.minimumNull end


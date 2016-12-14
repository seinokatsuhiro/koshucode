{-# OPTIONS_GHC -Wall #-}

-- | Edge content type.

module Koshucode.Baala.Data.Class.Edge
  ( -- * Type
    CTypeOf (..),
    GetContent,
    getContent,
  
    -- * Empty
    CEmpty (..), maybeEmpty, omitEmpty, contMaximum,
    -- * End
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

    -- | Test content is a some type.
    isA :: O.Test2 c D.Type
    isA c t = typeOf c == t

    -- | Test two contents are same type.
    sameType :: O.Test2 c c
    sameType c1 c2 = typeOf c1 == typeOf c2

-- | Get content in calculation.
type GetContent b c = B.Ab c -> B.Ab b

-- | Get content which may be aborted.
getContent :: (CTypeOf c) => O.Test c -> (c -> b) -> GetContent b c
getContent is get (Right c)
    | is c      = Right $ get c
    | otherwise = Msg.unmatchType $ B.encode $ typeOf c
getContent _ _ (Left a) = Left a


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


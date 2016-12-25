{-# OPTIONS_GHC -Wall #-}

-- | Edge content types.

module Koshucode.Baala.Data.Class.Edge
  ( -- * Basis
    Basis (..),
    GetContent,
    getContent,
  
    -- * Empty
    CEmpty (..), maybeEmpty, omitEmpty, contMaximum,
    empties,
    -- * End
    CEnd (..), contMinimum, 
  ) where

import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Type                 as T
import qualified Koshucode.Baala.Data.Class.Message   as Msg


-- --------------------------------------------  Basis

-- | Basis of contents.
class (Eq c, Ord c, B.MixEncode c) => Basis c where
    -- | Get type of content.
    typeOf :: c -> T.Type

    -- | Test content is a some type.
    isA :: O.Test2 c T.Type
    isA c t = typeOf c == t

    -- | Test two contents are same type.
    sameType :: O.Test2 c c
    sameType c1 c2 = typeOf c1 == typeOf c2

-- | Get content in calculation.
type GetContent b c = B.Ab c -> B.Ab b

-- | Get content which may be aborted.
getContent :: (Basis c) => O.Test c -> (c -> b) -> GetContent b c
getContent test get (Right c) | test c     = Right $ get c
                              | otherwise  = Msg.typeUnmatched c
getContent _ _ (Left a)                    = Left a


-- --------------------------------------------  Empty

-- | Empty: the minimum content.
class (Basis c) => CEmpty c where
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
contMaximum :: (CEmpty c) => [c] -> c
contMaximum = B.maximumNull empty

-- | List of empties.
--
--   >>> empties 4 :: [Content]
--   [ContentEmpty, ContentEmpty, ContentEmpty, ContentEmpty]
--
empties :: (CEmpty c) => Int -> [c]
empties n = replicate n empty


-- --------------------------------------------  End

-- | End of everything: the maximum content.
class (Basis c) => CEnd c where
    isEnd       :: c -> Bool
    end         :: c

-- | Minimum content of contents list.
contMinimum :: (CEnd c) => [c] -> c
contMinimum = B.minimumNull end


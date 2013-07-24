{-# OPTIONS_GHC -Wall #-}

{-| Literalizer: Make literal contents from token tree. -}

module Koshucode.Baala.Core.Content.Extension
(
  -- * Library
  litContent,
  litOperators,
  litJudge,

) where

import Koshucode.Baala.Base

import Koshucode.Baala.Core.Content.Class
import Koshucode.Baala.Core.Content.Literalize


-- ----------------------  Operator

litContent :: (CContent c) => LitTree c
litContent = litContentBy litOperators

litOperators :: (CContent c) => LitOperators c
litOperators =
    [ o "q"          litText
    , o "text"       litText
    , o "n"          litInt
    , o "int"        litInt
    , o "<list>"     litInt
    , o "<termset>"  litInt
    , o "<relation>" litInt
    ] where o = (,)

litText :: (CContent c) => LitTree c -> LitTrees c
litText lit xs = Right . joinContent =<< mapM lit xs



-- ----------------------  Simple data

-- litBool
-- litString
-- litNil

litInt :: (CInt c) => LitTree c -> LitTrees c
litInt _ [TreeL (TWord _ 0 digits)] = 
    Right . putInt =<< readInt digits
litInt _ xs = Left $ AbortNotNumber (show xs)

readInt :: LitString Int
readInt s =
    case reads s of
      [(n, "")] -> Right n
      _         -> Left $ AbortNotNumber s


-- ----------------------  Judge

litJudge
    :: (CContent c)
    => Bool                -- ^ Logical quality
    -> Relsign             -- ^ Judgement pattern
    -> LitTrees (Judge c)  -- ^ Convertor into judge
litJudge = litJudgeBy litOperators

{-| Construct judge from token trees.
    Judges itself are not content type.
    It can be only used in the top-level of sections. -}
litJudgeBy
    :: (CContent c)
    => [Named (LitTree c -> LitTrees c)]
    -> Bool                -- ^ Logical quality
    -> Relsign             -- ^ Judgement pattern
    -> LitTrees (Judge c)  -- ^ Convertor into judge
litJudgeBy ops q p xs =
  do xs' <- litTermset (litContentBy ops) xs
     Right $ Judge q p xs'



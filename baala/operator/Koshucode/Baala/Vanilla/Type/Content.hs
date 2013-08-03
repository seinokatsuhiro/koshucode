{-# OPTIONS_GHC -Wall #-}

{-| Term content. -}

module Koshucode.Baala.Vanilla.Type.Content
( VContent (..),
  toString,
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core

{-| Vanilla type -}

data VContent
    = VText    String            -- ^ String type
    | VDec     Decimal           -- ^ Decimal number type
    | VBool    Bool              -- ^ Boolean type
    | VList    [VContent]        -- ^ List type
    | VSet     [VContent]        -- ^ Set type
    | VTermset [Named VContent]  -- ^ List of terms
    | VRel     (Rel VContent)    -- ^ Relation type
    | VNil                       -- ^ Sign of no ordinary type
      deriving (Show, Eq, Ord)

instance PrimContent VContent where        



-- ----------------------  haskell data

instance CBool VContent where
    putBool                  =  VBool
    getBool (VBool x)        =  x
    getBool _                =  bug
    isBool  (VBool _)        =  True
    isBool  _                =  False

instance CDec VContent where
    putDec                   =  VDec
    getDec (VDec x)          =  x
    getDec _                 =  bug
    isDec  (VDec _)          =  True
    isDec  _                 =  False

instance CText VContent where
    putText                  = VText
    getText (VText s)        =  s
    getText _                =  bug
    isText  (VText _)        =  True
    isText  _                =  False

instance CList VContent where
    putList                  =  VList
    getList (VList xs)       =  xs
    getList _                =  []
    isList (VList _)         =  True
    isList _                 =  False



-- ----------------------  koshu data

instance CNil VContent where
    nil                      = VNil
    isNil VNil               = True
    isNil _                  = False

instance CSet VContent where
    putSet                   =  VSet . nonNilFilter . unique
    getSet (VSet x)          =  x
    getSet _                 =  bug
    isSet  (VSet _)          =  True
    isSet  _                 =  False

instance CTermset VContent where
    putTermset               = VTermset
    getTermset (VTermset x)  =  x
    getTermset _             =  bug
    isTermset  (VTermset _)  =  True
    isTermset  _             =  False

instance CRel VContent where
    putRel                   = VRel
    getRel (VRel r)          =  r
    getRel _                 =  bug
    isRel  (VRel _)          =  True
    isRel  _                 =  False



-- ----------------------

instance Pretty VContent where
    doc (VText s)       =  text $ "'" ++ hashString s
    doc (VDec n)        =  text $ decimalString n
    doc (VBool b)
        | b             =  text "#true"
        | otherwise     =  text "#false"
    doc (VNil)          =  text "()"
    doc (VList xs)      =  docBracket  $ hsep (docColonList xs)
    doc (VSet xs)       =  docBrace    $ hsep (docColonList xs)
    doc (VTermset xs)   =  docAngleBar $ hsep (map docTerms xs)
    doc (VRel r)        =  doc r

docTerms :: (Pretty a) => Named a -> Doc
docTerms (n, x) = text n <+> doc x

instance CContent VContent where
    appendContent (VNil) x = x
    appendContent x (VNil) = x
    appendContent (VText s1) (VText s2) = VText $ s1 ++ s2
    appendContent _ _ = nil



-- ----------------------  

-- binv :: (Int -> Int -> Int) -> VContent -> Map VContent
-- binv op (VDec x) (VDec y) = VDec $ op x y
-- binv _ _ _ = VNil

-- valRangeMinMax :: VContent -> VContent -> [VContent]
-- valRangeMinMax (VDec a) (VDec b) = map VDec [a .. b]
-- valRangeMinMax _ _ = undefined

-- toDec :: VContent -> Decimal
-- toDec (VText x) = read x
-- toDec (VDec  x) = x
-- toDec x = error $ "not integer: " ++ show x

toString :: VContent -> String
toString (VText x) = x
toString (VDec  x) = show x
toString _ = undefined



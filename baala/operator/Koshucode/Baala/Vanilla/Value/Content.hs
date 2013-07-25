{-# OPTIONS_GHC -Wall #-}

{-| Term content. -}

module Koshucode.Baala.Vanilla.Value.Content
( VContent (..),
  binv, toInt, toString,
  valRangeMinMax,
  module Koshucode.Baala.Base.Data,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Core.Content

{-| Type for values. -}

data VContent
    = VString  String            -- ^ String type
    | VInt     Int               -- ^ Integer type
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

instance CInt VContent where
    putInt                   =  VInt
    getInt (VInt x)          =  x
    getInt _                 =  bug
    isInt  (VInt _)          =  True
    isInt  _                 =  False

instance CString VContent where
    putString                = VString
    getString (VString s)    =  s
    getString _              =  bug
    isString  (VString _)    =  True
    isString  _              =  False

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
    doc (VString s)     =  text $ "'" ++ hashString s
    doc (VInt n)        =  int n
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
    appendContent (VString s1) (VString s2) = VString $ s1 ++ s2
    appendContent _ _ = nil



-- ----------------------  

binv :: (Int -> Int -> Int) -> VContent -> Map VContent
binv op (VInt x) (VInt y) = VInt $ op x y
binv _ _ _ = VNil

valRangeMinMax :: VContent -> VContent -> [VContent]
valRangeMinMax (VInt a) (VInt b) = map VInt [a .. b]
valRangeMinMax _ _ = undefined

toInt :: VContent -> Int
toInt (VString x) = read x
toInt (VInt    x) = x
toInt x = error $ "not integer: " ++ show x

toString :: VContent -> String
toString (VString x) = x
toString (VInt    x) = show x
toString _ = undefined


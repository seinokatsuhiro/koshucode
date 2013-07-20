{-# OPTIONS_GHC -Wall #-}

{-| Term content. -}

module Koshucode.Baala.Vanilla.Value.Content
( VContent (..),
  binv, toInt, toString,
  valRangeMinMax,
  module Koshucode.Baala.Base.Data,
) where

import qualified Data.List as L

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content

{-| Type for values. -}

data VContent
    = VString  String           -- ^ String type
    | VInt     Int              -- ^ Integer type
    | VBool    Bool             -- ^ Boolean type
    | VList    [VContent]       -- ^ List type
    | VSet     [VContent]       -- ^ Set type
    | VTermset [Named VContent] -- ^ List of terms
    | VRel     (Rel VContent)   -- ^ Relation type
    | VNil                      -- ^ Sign of no ordinary type
      deriving (Show, Eq, Ord)

instance CBool VContent where
    putBool = VBool
    isBool (VBool _) = True
    isBool _         = False

instance PrimContent VContent where

instance CInt VContent where
    putInt = VInt
    isInt (VInt _) = True
    isInt _        = False

instance CString VContent where
    putString = VString
    isString (VString _) = True
    isString _           = False
    getString (VString s) = s
    getString _           = ""

instance CList VContent where
    putList = VList
    isList (VList _)   = True
    isList _           = False
    getList (VList xs) = xs
    getList _          = []

instance CSet VContent where
    putSet = VSet . unique
    isSet (VSet _)   = True
    isSet _          = False

instance CTermset VContent where
    putTermset = VTermset
    isTermset (VTermset _) = True
    isTermset _            = False

instance CRel VContent where
    putRel = VRel
    getRel (VRel r) = r
    getRel _        = undefined
    isRel  (VRel _) = True
    isRel  _        = False

instance CNil VContent where
    nil = VNil
    isNil VNil = True
    isNil _   = False

instance Pretty VContent where
    doc (VString s)   = text $ escape s
    doc (VInt n)      = text "int" <+> int n
    doc (VBool b)
        | b           = text "#true"
        | otherwise   = text "#false"
    doc (VNil)         = text "()"
    doc (VList xs)    = text "[" <+> hsep (map doc xs) <+> text "]"
    doc (VSet xs)     = text "{" <+> hsep (map doc xs) <+> text "}"
    doc (VTermset xs) = text "{|" <+> hsep (map docTerms xs) <+> text "|}"
    doc (VRel r)      = doc r

docTerms :: (Pretty a) => Named a -> Doc
docTerms (n, x) = text n <+> doc x

instance CContent VContent where
    appendContent (VNil) x = x
    appendContent x (VNil) = x
    appendContent (VString s1) (VString s2) = VString $ s1 ++ s2
    appendContent _ _ = nil

escape :: String -> String
escape = join . hashSplit where
    join :: [String] -> String
    join [] = ""
    join [x] = x
    join xs = L.intercalate " " $ "text" : xs



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


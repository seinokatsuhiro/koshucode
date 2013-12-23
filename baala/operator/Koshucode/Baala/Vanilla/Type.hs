{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Type
( VContent (..),
  VCop,
  isMember,
) where

import qualified Data.Set as Set
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------  vanilla type

{-| Vanilla type -}

data VContent
    = VBool    Bool               -- ^ Boolean type
    | VText    String             -- ^ String type
    | VDec     B.Decimal          -- ^ Decimal number type
    | VNil                        -- ^ Sign of no ordinary type
    | VList    [VContent]         -- ^ List type (objective collection)
    | VSet     [VContent]         -- ^ Set type (informative collection)
    | VTermset [B.Named VContent] -- ^ Termset type (set of terms)
    | VRel     (B.Rel VContent)   -- ^ Relation type
      deriving (Show)

instance Eq VContent where
    x == y  =  compare x y == EQ
    x /= y  =  compare x y /= EQ

instance Ord VContent where
    compare (VBool    x) (VBool    y)  =  compare x y
    compare (VText    x) (VText    y)  =  compare x y
    compare (VDec     x) (VDec     y)  =  compare x y
    compare (VNil      ) (VNil      )  =  EQ
    compare (VList    x) (VList    y)  =  compare x y
    compare (VSet     x) (VSet     y)  =  compareAsSet x y
    compare (VTermset x) (VTermset y)  =  compareAsSet x y
    compare (VRel     x) (VRel     y)  =  compare x y

    compare (VBool    _) _             =  LT
    compare (VText    _) _             =  LT
    compare (VDec     _) _             =  LT
    compare (VNil      ) _             =  LT
    compare (VList    _) _             =  LT
    compare (VSet     _) _             =  LT
    compare (VTermset _) _             =  LT
    compare (VRel     _) _             =  LT

compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

instance C.PrimContent VContent where        
    typename (VBool    _)  =  "boolean"
    typename (VText    _)  =  "text"
    typename (VDec     _)  =  "decimal"
    typename (VNil)        =  "nil"
    typename (VList    _)  =  "list"
    typename (VSet     _)  =  "set"
    typename (VTermset _)  =  "termset"
    typename (VRel     _)  =  "rel"

instance C.CContent VContent where
    appendContent (VNil) x = Right x
    appendContent x (VNil) = Right x
    appendContent (VText x) (VText y) = Right . VText $ x ++ y
    appendContent x y = Left $ B.AbortCalc [] $ B.ACUnmatchType (show (x, y))

{-| >>> B.doc $ VText "abc"
    'abc  -}
instance B.Pretty VContent where
    doc (VText s)
        | s == ""           =  B.doc $ "#empty"
        | B.isSimpleWord s  =  B.doc $ '\'' : s
        | otherwise         =  B.doc $ C.hashWord s
    doc (VDec  n)           =  B.doc $ B.decimalString n
    doc (VBool b)           =  B.doc b
    doc (VNil)              =  B.doc "()"
    doc (VList    xs)       =  B.docWraps "["   "]" $ B.docColon xs
    doc (VSet     xs)       =  B.docWraps "{"   "}" $ B.docColon xs
    doc (VTermset xs)       =  B.docWraps "<|" "|>" $ B.doch xs
    doc (VRel r)            =  B.doc r

type VCop = C.CopEagerF VContent



-- ----------------------  haskell data

instance C.CBool VContent where
    putBool                  =  VBool
    getBool (VBool x)        =  x
    getBool _                =  B.bug
    isBool  (VBool _)        =  True
    isBool  _                =  False

instance C.CDec VContent where
    putDec                   =  VDec
    getDec (VDec x)          =  x
    getDec _                 =  B.bug
    isDec  (VDec _)          =  True
    isDec  _                 =  False

instance C.CText VContent where
    putText                  =  VText
    getText (VText s)        =  s
    getText _                =  B.bug
    isText  (VText _)        =  True
    isText  _                =  False

instance C.CList VContent where
    putList                  =  VList
    getList (VList xs)       =  xs
    getList _                =  []
    isList (VList _)         =  True
    isList _                 =  False



-- ----------------------  koshu data

instance C.CNil VContent where
    nil                      =  VNil
    isNil VNil               =  True
    isNil _                  =  False

instance C.CSet VContent where
    putSet                   =  VSet . C.nonNilFilter . B.unique
    getSet (VSet x)          =  x
    getSet _                 =  B.bug
    isSet  (VSet _)          =  True
    isSet  _                 =  False

instance C.CTermset VContent where
    putTermset               =  VTermset
    getTermset (VTermset x)  =  x
    getTermset _             =  B.bug
    isTermset  (VTermset _)  =  True
    isTermset  _             =  False

instance C.CRel VContent where
    putRel                   =  VRel
    getRel (VRel r)          =  r
    getRel _                 =  B.bug
    isRel  (VRel _)          =  True
    isRel  _                 =  False



-- ----------------------

isMember :: VContent -> VContent -> Bool
isMember x (VSet  xs) = x `elem` xs
isMember x (VList xs) = x `elem` xs
isMember _ _ = False


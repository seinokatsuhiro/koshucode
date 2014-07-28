{-# OPTIONS_GHC -Wall #-}

-- | Content formula.

module Koshucode.Baala.Op.Type
( VContent (..),
  VCop,
) where

import qualified Data.Set                   as Set
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Message



-- ----------------------  vanilla type

-- | Vanilla type

data VContent
    = VBool    Bool               -- ^ Boolean type
    | VText    String             -- ^ String type
    | VDec     B.Decimal          -- ^ Decimal number type
    | VEmpty                        -- ^ Sign of no ordinary type
    | VList    [VContent]         -- ^ List type (objective collection)
    | VSet     [VContent]         -- ^ Set type (informative collection)
    | VAssn    [B.Named VContent] -- ^ Assn type (set of terms)
    | VRel     (B.Rel VContent)   -- ^ Relation type
      deriving (Show)

instance Eq VContent where
    x == y  =  compare x y == EQ
    x /= y  =  compare x y /= EQ

instance Ord VContent where
    compare (VBool    x) (VBool    y)  =  compare x y
    compare (VText    x) (VText    y)  =  compare x y
    compare (VDec     x) (VDec     y)  =  compare x y
    compare (VEmpty    ) (VEmpty    )  =  EQ
    compare (VList    x) (VList    y)  =  compare x y
    compare (VSet     x) (VSet     y)  =  compareAsSet x y
    compare (VAssn    x) (VAssn    y)  =  compareAsSet x y
    compare (VRel     x) (VRel     y)  =  compare x y

    compare (VBool    _) _             =  LT
    compare (VText    _) _             =  LT
    compare (VDec     _) _             =  LT
    compare (VEmpty    ) _             =  LT
    compare (VList    _) _             =  LT
    compare (VSet     _) _             =  LT
    compare (VAssn    _) _             =  LT
    compare (VRel     _) _             =  LT

compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

instance C.PrimContent VContent where        
    typename (VBool    _)  =  "boolean"
    typename (VText    _)  =  "text"
    typename (VDec     _)  =  "decimal"
    typename (VEmpty    )  =  "empty"
    typename (VList    _)  =  "list"
    typename (VSet     _)  =  "set"
    typename (VAssn    _)  =  "assn"
    typename (VRel     _)  =  "rel"

instance C.CContent VContent where
    appendContent (VEmpty) x = Right x
    appendContent x (VEmpty) = Right x
    appendContent (VText x) (VText y) = Right . VText $ x ++ y
    appendContent x y = Message.unmatchType (show (x, y))

instance B.Write VContent where
    write sh a = case a of
        VText s      ->  B.doc $ sh s
        VDec  n      ->  B.doc $ B.decimalString n
        VBool b      ->  B.doc b
        VEmpty       ->  B.doc "()"
        VList    xs  ->  B.docWraps "["   "]" $ B.writeColon sh xs
        VSet     xs  ->  B.docWraps "{"   "}" $ B.writeColon sh xs
        VAssn    xs  ->  B.docWraps "<<" ">>" $ B.writeH     sh xs
        VRel r       ->  B.write sh r

type VCop = C.CopFun VContent



-- ----------------------  haskell data

instance C.CBool VContent where
    pBool                    =  VBool
    gBool (VBool x)          =  x
    gBool _                  =  B.bug "gBool"
    isBool  (VBool _)        =  True
    isBool  _                =  False

instance C.CDec VContent where
    pDec                     =  VDec
    gDec (VDec x)            =  x
    gDec _                   =  B.bug "gDec"
    isDec  (VDec _)          =  True
    isDec  _                 =  False

instance C.CText VContent where
    pText                    =  VText
    gText (VText s)          =  s
    gText _                  =  B.bug "gText"
    isText  (VText _)        =  True
    isText  _                =  False

instance C.CList VContent where
    pList                    =  VList
    gList (VList xs)         =  xs
    gList _                  =  []
    isList (VList _)         =  True
    isList _                 =  False



-- ----------------------  koshu data

instance C.CEmpty VContent where
    empty                    =  VEmpty
    isEmpty VEmpty           =  True
    isEmpty _                =  False

instance C.CSet VContent where
    pSet                     =  VSet . B.omit C.isEmpty . B.unique
    gSet (VSet x)            =  x
    gSet _                   =  B.bug "gSet"
    isSet  (VSet _)          =  True
    isSet  _                 =  False

instance C.CAssn VContent where
    pAssn                    =  VAssn
    gAssn (VAssn x)          =  x
    gAssn _                  =  B.bug "gAssn"
    isAssn  (VAssn _)        =  True
    isAssn  _                =  False

instance C.CRel VContent where
    pRel                     =  VRel
    gRel (VRel r)            =  r
    gRel _                   =  B.bug "gRel"
    isRel  (VRel _)          =  True
    isRel  _                 =  False


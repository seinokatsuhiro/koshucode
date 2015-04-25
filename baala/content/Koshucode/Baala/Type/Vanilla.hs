{-# OPTIONS_GHC -Wall #-}

-- | Vanilla type.

module Koshucode.Baala.Type.Vanilla
  ( VContent (..),
  ) where

import qualified Data.Set                   as Set
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Msg



-- ----------------------  vanilla type

-- | Vanilla type

data VContent
    = VBool    Bool               -- ^ Boolean type
    | VText    String             -- ^ String type
    | VTerm    String             -- ^ Term name type
    | VDec     B.Decimal          -- ^ Decimal number type
    | VClock   B.Clock            -- ^ Clock type
    | VTime    B.Time             -- ^ Time type
    | VEmpty                      -- ^ Sign of no ordinary type
    | VInterp  B.Interp           -- ^ Interpretation type
    | VType    B.Type             -- ^ Type for type

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
    compare (VTerm    x) (VTerm    y)  =  compare x y
    compare (VDec     x) (VDec     y)  =  compare x y
    compare (VClock   x) (VClock   y)  =  compare x y
    compare (VTime    x) (VTime    y)  =  compare x y
    compare (VEmpty    ) (VEmpty    )  =  EQ
    compare (VInterp  x) (VInterp  y)  =  compare x y
    compare (VType    x) (VType    y)  =  compare x y
    compare (VList    x) (VList    y)  =  compare x y
    compare (VSet     x) (VSet     y)  =  compareAsSet x y
    compare (VAssn    x) (VAssn    y)  =  compareAsSet x y
    compare (VRel     x) (VRel     y)  =  compare x y
    compare x y                        =  typeOrder x `compare` typeOrder y

typeOrder :: VContent -> Int
typeOrder (VBool    _)  =  1
typeOrder (VText    _)  =  2
typeOrder (VTerm    _)  =  3
typeOrder (VDec     _)  =  4
typeOrder (VClock   _)  =  6
typeOrder (VTime    _)  =  7
typeOrder (VEmpty    )  =  8
typeOrder (VInterp  _)  =  9
typeOrder (VType    _)  =  10
typeOrder (VList    _)  =  11
typeOrder (VSet     _)  =  13
typeOrder (VAssn    _)  =  14
typeOrder (VRel     _)  =  15

compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

instance C.CTypeOf VContent where
    typeOf (VBool    _)  =  B.TypeBool
    typeOf (VText    _)  =  B.TypeText
    typeOf (VTerm    _)  =  B.TypeTerm
    typeOf (VDec     _)  =  B.TypeDec
    typeOf (VClock   c)  =  B.TypeClock $ Just $ B.clockPrecision c
    typeOf (VTime    t)  =  B.TypeTime  $ Just $ B.timePrecision t
    typeOf (VEmpty    )  =  B.TypeEmpty
    typeOf (VInterp  _)  =  B.TypeInterp
    typeOf (VType    _)  =  B.TypeType
    typeOf (VList   cs)  =  B.TypeList $ typeSum cs
    typeOf (VSet    cs)  =  B.TypeSet  $ typeSum cs
    typeOf (VAssn   cs)  =  B.TypeAssn $ B.mapSndTo C.typeOf cs
    typeOf (VRel     _)  =  B.TypeRel []

typeSum :: C.CTypeOf c => [c] -> B.Type
typeSum cs = case B.unique $ map C.typeOf cs of
               [t] -> t
               ts  -> B.TypeSum ts

instance C.CContent VContent where
    appendContent (VEmpty) x = Right x
    appendContent x (VEmpty) = Right x
    appendContent (VText x) (VText y) = Right . VText $ x ++ y
    appendContent x y = Msg.unmatchType (show (x, y))

instance B.Write VContent where
    write sh a = case a of
        VText s      ->  B.doc $ sh s
        VTerm s      ->  B.doc $ "'/" ++ s
        VDec  n      ->  B.doc $ B.decimalString n
        VClock c     ->  B.doc c
        VTime t      ->  B.doc t
        VBool b      ->  B.doc b
        VEmpty       ->  B.doc "()"
        VInterp i    ->  B.write sh i
        VType t      ->  B.docWraps "[-" "-]" $ B.write    sh t
        VList xs     ->  B.docWraps "["   "]" $ B.writeBar sh xs
        VSet  xs     ->  B.docWraps "{"   "}" $ B.writeBar sh xs
        VAssn xs     ->  B.docWraps "<<" ">>" $ B.writeH   sh xs
        VRel r       ->  B.write sh r


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

instance C.CClock VContent where
    pClock                    =  VClock
    gClock  (VClock x)        =  x
    gClock  _                 =  B.bug "gClock"
    isClock (VClock _)        =  True
    isClock _                 =  False

instance C.CTime VContent where
    pTime                    =  VTime
    gTime  (VTime x)         =  x
    gTime  _                 =  B.bug "gTime"
    isTime (VTime _)         =  True
    isTime _                 =  False

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

instance C.CTerm VContent where
    pTerm                    =  VTerm
    gTerm (VTerm s)          =  s
    gTerm _                  =  B.bug "gTerm"
    isTerm  (VTerm _)        =  True
    isTerm  _                =  False

instance C.CInterp VContent where
    pInterp                     =  VInterp
    gInterp (VInterp r)         =  r
    gInterp _                   =  B.bug "gInterp"
    isInterp  (VInterp _)       =  True
    isInterp  _                 =  False

instance C.CType VContent where
    pType                     =  VType
    gType (VType r)           =  r
    gType _                   =  B.bug "gType"
    isType  (VType _)         =  True
    isType  _                 =  False

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


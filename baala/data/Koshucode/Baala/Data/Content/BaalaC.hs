{-# OPTIONS_GHC -Wall #-}

-- | Baala content type.

module Koshucode.Baala.Data.Content.BaalaC
  ( BaalaC (..),
  ) where

import qualified Data.Set                              as Set
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Data.Type             as D
import qualified Koshucode.Baala.Data.Content.Class    as D
import qualified Koshucode.Baala.Data.Content.Message  as Msg


-- ----------------------  Content type

-- | Vanilla content type
data BaalaC
    = VBool    Bool               -- ^ Boolean type
    | VText    String             -- ^ String type
    | VTerm    String             -- ^ Term name type
    | VDec     D.Decimal          -- ^ Decimal number type
    | VClock   D.Clock            -- ^ Clock type
    | VTime    D.Time             -- ^ Time type
    | VEmpty                      -- ^ Sign of no ordinary type
    | VInterp  D.Interp           -- ^ Interpretation type
    | VType    D.Type             -- ^ Type for type
    | VList    [BaalaC]           -- ^ List type (objective collection)
    | VSet     [BaalaC]           -- ^ Set type (informative collection)
    | VAssn    [B.Named BaalaC]   -- ^ Assn type (set of terms)
    | VRel     (D.Rel BaalaC)     -- ^ Relation type
    deriving (Show)

instance Eq BaalaC where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord BaalaC where
    compare (VBool    x) (VBool    y)  = compare x y
    compare (VText    x) (VText    y)  = compare x y
    compare (VTerm    x) (VTerm    y)  = compare x y
    compare (VDec     x) (VDec     y)  = compare x y
    compare (VClock   x) (VClock   y)  = compare x y
    compare (VTime    x) (VTime    y)  = compare x y
    compare (VEmpty    ) (VEmpty    )  = EQ
    compare (VInterp  x) (VInterp  y)  = compare x y
    compare (VType    x) (VType    y)  = compare x y
    compare (VList    x) (VList    y)  = compare x y
    compare (VSet     x) (VSet     y)  = compareAsSet x y
    compare (VAssn    x) (VAssn    y)  = compareAsSet x y
    compare (VRel     x) (VRel     y)  = compare x y
    compare x y                        = typeOrder x `compare` typeOrder y

typeOrder :: BaalaC -> Int
typeOrder (VBool    _)  = 1
typeOrder (VText    _)  = 2
typeOrder (VTerm    _)  = 3
typeOrder (VDec     _)  = 4
typeOrder (VClock   _)  = 6
typeOrder (VTime    _)  = 7
typeOrder (VEmpty    )  = 8
typeOrder (VInterp  _)  = 9
typeOrder (VType    _)  = 10
typeOrder (VList    _)  = 11
typeOrder (VSet     _)  = 13
typeOrder (VAssn    _)  = 14
typeOrder (VRel     _)  = 15

compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

instance D.CTypeOf BaalaC where
    typeOf (VBool    _)  = D.TypeBool
    typeOf (VText    _)  = D.TypeText
    typeOf (VTerm    _)  = D.TypeTerm
    typeOf (VDec     _)  = D.TypeDec
    typeOf (VClock   c)  = D.TypeClock $ Just $ D.clockPrecision c
    typeOf (VTime    t)  = D.TypeTime  $ Just $ D.timePrecision t
    typeOf (VEmpty    )  = D.TypeEmpty
    typeOf (VInterp  _)  = D.TypeInterp
    typeOf (VType    _)  = D.TypeType
    typeOf (VList   cs)  = D.TypeList $ typeSum cs
    typeOf (VSet    cs)  = D.TypeSet  $ typeSum cs
    typeOf (VAssn   cs)  = D.TypeAssn $ B.mapSndTo D.typeOf cs
    typeOf (VRel     _)  = D.TypeRel []

typeSum :: D.CTypeOf c => [c] -> D.Type
typeSum cs = case B.unique $ map D.typeOf cs of
               [t] -> t
               ts  -> D.TypeSum ts

instance D.CContent BaalaC where
    appendContent (VEmpty) x           = Right x
    appendContent x (VEmpty)           = Right x
    appendContent (VText x) (VText y)  = Right . VText $ x ++ y
    appendContent x y                  = Msg.unmatchType (show (x, y))

instance B.Write BaalaC where
    writeDocWith sh c = case c of
        VText s      -> B.doc $ sh s
        VTerm s      -> B.doc $ "'/" ++ s
        VDec  n      -> B.doc $ D.decimalString n
        VClock t     -> B.doc t
        VTime t      -> B.doc t
        VBool b      -> B.doc b
        VEmpty       -> B.doc "()"
        VInterp i    -> B.writeDocWith sh i
        VType t      -> B.docWraps "[-" "-]" $ B.writeDocWith    sh t
        VList xs     -> B.docWraps "["   "]" $ B.writeBar sh xs
        VSet  xs     -> B.docWraps "{"   "}" $ B.writeBar sh xs
        VAssn xs     -> B.docWraps "<<" ">>" $ B.writeH   sh xs
        VRel r       -> B.writeDocWith sh r

    writeHtmlWith sh c = case c of
        VRel r       -> B.writeHtmlWith sh r
        _            -> B.toHtml $ B.writeStringWith sh c


-- ----------------------  haskell data

instance D.CBool BaalaC where
    pBool                    = VBool
    gBool (VBool x)          = x
    gBool _                  = B.bug "gBool"
    isBool  (VBool _)        = True
    isBool  _                = False

instance D.CDec BaalaC where
    pDec                     = VDec
    gDec (VDec x)            = x
    gDec _                   = B.bug "gDec"
    isDec  (VDec _)          = True
    isDec  _                 = False

instance D.CClock BaalaC where
    pClock                   = VClock
    gClock  (VClock x)       = x
    gClock  _                = B.bug "gClock"
    isClock (VClock _)       = True
    isClock _                = False

instance D.CTime BaalaC where
    pTime                    = VTime
    gTime  (VTime x)         = x
    gTime  _                 = B.bug "gTime"
    isTime (VTime _)         = True
    isTime _                 = False

instance D.CText BaalaC where
    pText                    = VText
    gText (VText s)          = s
    gText _                  = B.bug "gText"
    isText  (VText _)        = True
    isText  _                = False

instance D.CList BaalaC where
    pList                    = VList
    gList (VList xs)         = xs
    gList _                  = []
    isList (VList _)         = True
    isList _                 = False



-- ----------------------  koshu data

instance D.CEmpty BaalaC where
    empty                    = VEmpty
    isEmpty VEmpty           = True
    isEmpty _                = False

instance D.CTerm BaalaC where
    pTerm                    = VTerm
    gTerm (VTerm s)          = s
    gTerm _                  = B.bug "gTerm"
    isTerm  (VTerm _)        = True
    isTerm  _                = False

instance D.CInterp BaalaC where
    pInterp                  = VInterp
    gInterp (VInterp r)      = r
    gInterp _                = B.bug "gInterp"
    isInterp  (VInterp _)    = True
    isInterp  _              = False

instance D.CType BaalaC where
    pType                    = VType
    gType (VType r)          = r
    gType _                  = B.bug "gType"
    isType  (VType _)        = True
    isType  _                = False

instance D.CSet BaalaC where
    pSet                     = VSet . B.omit D.isEmpty . B.unique
    gSet (VSet x)            = x
    gSet _                   = B.bug "gSet"
    isSet  (VSet _)          = True
    isSet  _                 = False

instance D.CAssn BaalaC where
    pAssn                    = VAssn
    gAssn (VAssn x)          = x
    gAssn _                  = B.bug "gAssn"
    isAssn  (VAssn _)        = True
    isAssn  _                = False

instance D.CRel BaalaC where
    pRel                     = VRel
    gRel (VRel r)            = r
    gRel _                   = B.bug "gRel"
    isRel  (VRel _)          = True
    isRel  _                 = False

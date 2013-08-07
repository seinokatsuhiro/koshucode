{-# OPTIONS_GHC -Wall #-}

{-| Content formula. -}

module Koshucode.Baala.Vanilla.Type
( VContent (..),
  VCop,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



-- ----------------------  vanilla type

{-| Vanilla type -}

data VContent
    = VBool    Bool               -- ^ Boolean type
    | VText    String             -- ^ String type
    | VDec     C.Decimal          -- ^ Decimal number type
    | VNil                        -- ^ Sign of no ordinary type
    | VList    [VContent]         -- ^ List type (objective collection)
    | VSet     [VContent]         -- ^ Set type (informative collection)
    | VTermset [B.Named VContent] -- ^ Termset type (set of terms)
    | VRel     (B.Rel VContent)   -- ^ Relation type
      deriving (Show, Eq, Ord)

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
    appendContent (VNil) x = x
    appendContent x (VNil) = x
    appendContent (VText s1) (VText s2) = VText $ s1 ++ s2
    appendContent _ _ = C.nil

instance B.Pretty VContent where
    doc (VText s)       =  B.text $ "'" ++ B.hashString s
    doc (VDec n)        =  B.text $ C.decimalString n
    doc (VBool b)
        | b             =  B.text "#true"
        | otherwise     =  B.text "#false"
    doc (VNil)          =  B.text "()"
    doc (VList xs)      =  B.docBracket  $ B.hsep (B.docColonList xs)
    doc (VSet xs)       =  B.docBrace    $ B.hsep (B.docColonList xs)
    doc (VTermset xs)   =  B.docAngleBar $ B.hsep (map docTerms xs)
    doc (VRel r)        =  B.doc r

docTerms :: (B.Pretty a) => B.Named a -> B.Doc
docTerms (n, x) = B.text n B.<+> B.doc x

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


{-# OPTIONS_GHC -Wall #-}

-- | The Baala content type.

module Koshucode.Baala.Data.Content
  ( -- * Type
    Content (..),

    -- * Dispatcher
    DispatchEdge,
    DispatchSimple,
    DispatchComplex,
    dispatchEdge,
    dispatchSimple,
    dispatchComplex,
    dispatchContent,

    -- * Concrete type
    BaalaC,
    TermC, JudgeC, RelC,
    the, stringC,
  ) where

import qualified Data.Set                                as Set
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data.Type               as D
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Decode             as D
import qualified Koshucode.Baala.Data.Class.Message      as Msg


-- ----------------------  Content type

-- | The Baala content type.
{-# DEPRECATED BaalaC "Use 'Content' instead" #-}
type BaalaC = Content

{-# WARNING VEmpty   "Use 'D.empty' instead." #-}
{-# WARNING VBool    "Use 'D.pBool' instead." #-}
{-# WARNING VDec     "Use 'D.pDec' instead." #-}
{-# WARNING VClock   "Use 'D.pClock' instead." #-}
{-# WARNING VTime    "Use 'D.pTime' instead." #-}
{-# WARNING VCode    "Use 'D.pCode' instead." #-}
{-# WARNING VTerm    "Use 'D.pTerm' instead." #-}
{-# WARNING VText    "Use 'D.pText' instead." #-}
{-# WARNING VList    "Use 'D.pList' instead." #-}
{-# WARNING VSet     "Use 'D.pSet' instead." #-}
{-# WARNING VTie     "Use 'D.pTie' instead." #-}
{-# WARNING VRel     "Use 'D.pRel' instead." #-}
{-# WARNING VInterp  "Use 'D.pInterp' instead." #-}
{-# WARNING VType    "Use 'D.pType' instead." #-}
{-# WARNING VEnd     "Use 'D.end' instead." #-}

-- | The Baala content type.
data Content
    = VEmpty                      -- ^ /Singleton:/   Sign of no ordinary type
    | VBool    Bool               -- ^ /Numeric:/     Boolean type
    | VDec     D.Decimal          -- ^ /Numeric:/     Decimal number type
    | VClock   D.Clock            -- ^ /Numeric:/     Clock type
    | VTime    D.Time             -- ^ /Numeric:/     Time type
    | VCode    String             -- ^ /Textual:/     Code type
    | VTerm    String             -- ^ /Textual:/     Term name type
    | VText    String             -- ^ /Textual:/     Text type
    | VList    [Content]           -- ^ /Collective:/  List type
    | VSet     [Content]           -- ^ /Collective:/  Set type
    | VTie     [B.Named Content]   -- ^ /Relational:/  Tie type (set of terms)
    | VRel     (D.Rel Content)     -- ^ /Relational:/  Relation type
    | VInterp  D.Interp           -- ^ /Relational:/  Interpretation type
    | VType    D.Type             -- ^ /Meta:/        Type for type
    | VEnd                        -- ^ /Singleton:/   The end of everything
    deriving (Show)

instance Eq Content where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord Content where
    -- simple
    compare (VBool    x) (VBool    y)  = compare x y
    compare (VDec     x) (VDec     y)  = compare x y
    compare (VClock   x) (VClock   y)  = compare x y
    compare (VTime    x) (VTime    y)  = compare x y
    compare (VCode    x) (VCode    y)  = compare x y
    compare (VTerm    x) (VTerm    y)  = compare x y
    compare (VText    x) (VText    y)  = compare x y

    -- complex
    compare (VList    x) (VList    y)  = compare x y
    compare (VSet     x) (VSet     y)  = compareAsSet x y
    compare (VTie     x) (VTie     y)  = compareAsSet x y
    compare (VRel     x) (VRel     y)  = compare x y
    compare (VInterp  x) (VInterp  y)  = compare x y
    compare (VType    x) (VType    y)  = compare x y

    -- others
    compare x y  = D.typeOrder x `compare` D.typeOrder y

-- | Compare two lists as two sets.
compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

instance D.CTypeOf Content where
    typeOf (VEmpty    )  = D.TypeEmpty
    typeOf (VBool    _)  = D.TypeBool
    typeOf (VDec     _)  = D.TypeDec
    typeOf (VClock   c)  = D.TypeClock $ Just $ D.clockPrecision c
    typeOf (VTime    t)  = D.TypeTime  $ Just $ D.timePrecision t
    typeOf (VCode    _)  = D.TypeCode
    typeOf (VTerm    _)  = D.TypeTerm
    typeOf (VText    _)  = D.TypeText
    typeOf (VList   cs)  = D.TypeList $ typeSum cs
    typeOf (VSet    cs)  = D.TypeSet  $ typeSum cs
    typeOf (VTie    cs)  = D.TypeTie  $ B.mapSndTo D.typeOf cs
    typeOf (VRel     _)  = D.TypeRel []
    typeOf (VInterp  _)  = D.TypeInterp
    typeOf (VType    _)  = D.TypeType
    typeOf (VEnd      )  = D.TypeEnd

typeSum :: D.CTypeOf c => [c] -> D.Type
typeSum cs = case B.unique $ map D.typeOf cs of
               [t] -> t
               ts  -> D.TypeSum ts

instance D.CContent Content where
    appendContent (VEmpty) x           = Right x
    appendContent x (VEmpty)           = Right x
    appendContent (VText x) (VText y)  = Right . VText $ x ++ y
    appendContent x y                  = Msg.unmatchType (show (x, y))

instance B.MixShortEncode Content where
    mixShortEncode sh c =
        case c of
          VCode  s   -> B.mixString $ quote  (sh s) s
          VText  s   -> B.mixString $ qquote (sh s) s
          VTerm  s   -> B.mixString $ "'/" ++ s
          VDec   n   -> B.mixString $ D.encodeDecimal n
          VClock t   -> B.mixEncode t
          VTime  t   -> B.mixEncode t
          VBool  b   -> B.mixEncode b
          VEmpty     -> B.mixString "()"
          VEnd       -> B.mixString "(/)"

          VList cs   -> D.mixBracketList $ mixBar cs
          VSet  cs   -> D.mixBracketSet  $ mixBar cs
          VTie  ts   -> B.mixBracketS S.tieOpen  S.tieClose  $ D.termsToMix1 sh ts
          VRel  r    -> B.mixShortEncode sh r
          VInterp i  -> B.mixEncode i
          VType t    -> B.mixBracketS S.typeOpen S.typeClose $ B.mixEncode t
        where
          mixBar cs   = B.mixJoinBar $ map (B.mixShortEncode sh) cs

quote :: Maybe String -> O.StringMap
quote (Nothing) s   = "'" ++ s
quote (Just s)  _   = s

qquote :: Maybe String -> O.StringMap
qquote (Nothing) "" = "\"\""
qquote (Nothing) s  = S.angleQuote s
qquote (Just s)  _  = s


-- ----------------------  Simple

instance D.CEmpty Content where
    empty                    = VEmpty
    isEmpty VEmpty           = True
    isEmpty _                = False

instance D.CEnd Content where
    end                      = VEnd
    isEnd VEnd               = True
    isEnd _                  = False

instance D.CBool Content where
    pBool                    = VBool
    gBool (VBool x)          = x
    gBool _                  = B.bug "gBool"
    isBool  (VBool _)        = True
    isBool  _                = False

instance D.CDec Content where
    pDec                     = VDec
    gDec (VDec x)            = x
    gDec _                   = B.bug "gDec"
    isDec  (VDec _)          = True
    isDec  _                 = False

instance D.CClock Content where
    pClock                   = VClock
    gClock  (VClock x)       = x
    gClock  _                = B.bug "gClock"
    isClock (VClock _)       = True
    isClock _                = False

instance D.CTime Content where
    pTime                    = VTime
    gTime  (VTime x)         = x
    gTime  _                 = B.bug "gTime"
    isTime (VTime _)         = True
    isTime _                 = False

instance D.CCode Content where
    pCode                    = VCode
    gCode (VCode s)          = s
    gCode _                  = B.bug "gCode"
    isCode  (VCode _)        = True
    isCode  _                = False

instance D.CTerm Content where
    pTerm                    = VTerm
    gTerm (VTerm s)          = s
    gTerm _                  = B.bug "gTerm"
    isTerm  (VTerm _)        = True
    isTerm  _                = False

instance D.CText Content where
    pText                    = VText
    gText (VText s)          = s
    gText _                  = B.bug "gText"
    isText  (VText _)        = True
    isText  _                = False

-- ----------------------  Complex

instance D.CList Content where
    pList                    = VList
    gList (VList xs)         = xs
    gList _                  = []
    isList (VList _)         = True
    isList _                 = False

instance D.CSet Content where
    pSet                     = VSet . B.omit D.isEmpty . B.unique
    gSet (VSet x)            = x
    gSet _                   = B.bug "gSet"
    isSet  (VSet _)          = True
    isSet  _                 = False

instance D.CTie Content where
    pTie                     = VTie 
    gTie (VTie x)            = x
    gTie _                   = B.bug "gTie"
    isTie  (VTie _)          = True
    isTie  _                 = False

instance D.CRel Content where
    pRel                     = VRel
    gRel (VRel r)            = r
    gRel _                   = B.bug "gRel"
    isRel  (VRel _)          = True
    isRel  _                 = False

instance D.CInterp Content where
    pInterp                  = VInterp
    gInterp (VInterp r)      = r
    gInterp _                = B.bug "gInterp"
    isInterp  (VInterp _)    = True
    isInterp  _              = False

instance D.CType Content where
    pType                    = VType
    gType (VType r)          = r
    gType _                  = B.bug "gType"
    isType  (VType _)        = True
    isType  _                = False

-- ----------------------  Dispatch

-- | Constants for edge content, i.e., empty and end.
type DispatchEdge a = ( a, a )

-- | Functions for simple content, i.e.,
--   boolean, decimal, clock, time, code, term, and text.
type DispatchSimple a =
    ( Bool        -> a
    , D.Decimal   -> a
    , D.Clock     -> a
    , D.Time      -> a
    , String      -> a
    , String      -> a
    , String      -> a
    )

-- | Functions for complex content, i.e.,
--   list, set, tie, relation, interp, and type.
type DispatchComplex a =
    ( [Content]         -> a
    , [Content]         -> a
    , [B.Named Content] -> a
    , D.Rel Content     -> a
    , D.Interp          -> a
    , D.Type            -> a
    )

-- | Call type-specific functions on empty or end content.
dispatchEdge :: DispatchEdge a -> (Content -> a) -> (Content -> a)
dispatchEdge (empty, end) other c =
    case c of
      VEmpty    -> empty
      VEnd      -> end
      _         -> other c

-- | Call type-specific functions on simple content.
dispatchSimple :: DispatchSimple a -> (Content -> a) -> (Content -> a)
dispatchSimple (bool, dec, clock, time, code, term, text) other c =
    case c of
      VBool   b -> bool  b
      VDec    n -> dec   n
      VClock  t -> clock t
      VTime   t -> time  t
      VCode   s -> code  s
      VTerm   s -> term  s
      VText   s -> text  s
      _         -> other c

-- | Call type-specific functions on complex content.
dispatchComplex :: DispatchComplex a -> (Content -> a) -> (Content -> a)
dispatchComplex (list, set, tie, rel, interp, type_) other c =
    case c of
      VList   cs -> list   cs
      VSet    cs -> set    cs
      VTie    ts -> tie    ts
      VRel     r -> rel    r
      VInterp  i -> interp i
      VType    t -> type_  t
      _          -> other  c

-- | Call type-specific functions on content.
dispatchContent :: DispatchEdge a -> DispatchSimple a -> DispatchComplex a -> (Content -> a)
dispatchContent e s c = simple where
    simple  = dispatchSimple s edge
    edge    = dispatchEdge e complex
    complex = dispatchComplex c other
    other   = B.bug "dispatchContent"


-- ----------------------  Concrete type

-- | @Judge@ for concrete baala content.
type JudgeC = D.Judge Content

-- | @Term@ for concrete baala content.
type TermC = S.Term Content

-- | @Rel@ for concrete baala content.
type RelC = D.Rel Content

-- | Shorthand function for the Baala content type.
--
--   >>> the $ D.pText "a"
--   VText "a"
--
the :: Content -> Content
{-# INLINE the #-}
the = id

-- | Decode the Baala content from string.
--
--   >>> stringC "'a"
--   Right (VCode "a")
--
stringC :: String -> B.Ab Content
stringC = D.stringContent

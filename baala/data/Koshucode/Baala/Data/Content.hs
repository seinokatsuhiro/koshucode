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
    TermC, JudgeC, RelC, AbC, DatasetC,
    the, stringC,
  ) where

import qualified Data.Set                                as Set
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as T
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Decode             as D


-- ----------------------  Content type

{-# WARNING ContentEmpty   "Use 'empty' instead." #-}
{-# WARNING ContentBool    "Use 'pBool' instead." #-}
{-# WARNING ContentDec     "Use 'pDec' instead." #-}
{-# WARNING ContentClock   "Use 'pClock' instead." #-}
{-# WARNING ContentTime    "Use 'pTime' instead." #-}
{-# WARNING ContentCode    "Use 'pCode' instead." #-}
{-# WARNING ContentTerm    "Use 'pTerm' instead." #-}
{-# WARNING ContentText    "Use 'pText' instead." #-}
{-# WARNING ContentList    "Use 'pList' instead." #-}
{-# WARNING ContentSet     "Use 'pSet' instead." #-}
{-# WARNING ContentTie     "Use 'pTie' instead." #-}
{-# WARNING ContentRel     "Use 'pRel' instead." #-}
{-# WARNING ContentInterp  "Use 'pInterp' instead." #-}
{-# WARNING ContentType    "Use 'pType' instead." #-}
{-# WARNING ContentEnd     "Use 'end' instead." #-}

-- | The Baala content type.
data Content
    = ContentEmpty              -- ^ __1.  Edge:__                   Sign of no ordinary type
    | ContentBool   Bool        -- ^ __2.  Numeric:__                Boolean type
    | ContentDec    T.Decimal   -- ^ __3.  Numeric:__                Decimal number type
    | ContentClock  T.Clock     -- ^ __4.  Numeric:__                Clock type
    | ContentTime   T.Time      -- ^ __5.  Numeric:__                Time type
    | ContentCode   String      -- ^ __6.  Textual:__                Code type
    | ContentTerm   S.TermName  -- ^ __7.  Textual:__                Term name type
    | ContentText   String      -- ^ __8.  Textual:__                Text type
    | ContentList   [Content]   -- ^ __9.  Recursive, Collective:__  List type
    | ContentSet    [Content]   -- ^ __10. Recursive, Collective:__  Set type
    | ContentTie    [TermC]     -- ^ __11. Recursive, Relational:__  Tie type (set of terms)
    | ContentRel    RelC        -- ^ __12. Recursive, Relational:__  Relation type
    | ContentInterp T.Interp    -- ^ __13. Meta, Relational:__       Interpretation type
    | ContentType   T.Type      -- ^ __14. Meta:__                   Type for type
    | ContentEnd                -- ^ __15. Edge:__                   The end of everything
    deriving (Show)

instance Eq Content where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord Content where
    -- simple
    compare (ContentBool    x) (ContentBool    y)  = compare x y
    compare (ContentDec     x) (ContentDec     y)  = compare x y
    compare (ContentClock   x) (ContentClock   y)  = compare x y
    compare (ContentTime    x) (ContentTime    y)  = compare x y
    compare (ContentCode    x) (ContentCode    y)  = compare x y
    compare (ContentTerm    x) (ContentTerm    y)  = compare x y
    compare (ContentText    x) (ContentText    y)  = compare x y

    -- complex
    compare (ContentList    x) (ContentList    y)  = compare x y
    compare (ContentSet     x) (ContentSet     y)  = compareAsSet x y
    compare (ContentTie     x) (ContentTie     y)  = compareAsSet x y
    compare (ContentRel     x) (ContentRel     y)  = compare x y
    compare (ContentInterp  x) (ContentInterp  y)  = compare x y
    compare (ContentType    x) (ContentType    y)  = compare x y

    -- others
    compare x y  = D.typeOrder x `compare` D.typeOrder y

-- | Compare two lists as two sets.
compareAsSet :: (Ord a) => [a] -> [a] -> Ordering
compareAsSet x y = compare (Set.fromList x) (Set.fromList y)

-- | Empty content.
instance B.Default Content where
    def = D.empty

instance D.Basis Content where
    typeOf (ContentEmpty    )  = T.TypeEmpty
    typeOf (ContentBool    _)  = T.TypeBool
    typeOf (ContentDec     _)  = T.TypeDec
    typeOf (ContentClock   c)  = T.TypeClock $ Just $ T.clockPrecision c
    typeOf (ContentTime    t)  = T.TypeTime  $ Just $ T.timePrecision t
    typeOf (ContentCode    _)  = T.TypeCode
    typeOf (ContentTerm    _)  = T.TypeTerm
    typeOf (ContentText    _)  = T.TypeText
    typeOf (ContentList   cs)  = T.TypeList $ typeSum cs
    typeOf (ContentSet    cs)  = T.TypeSet  $ typeSum cs
    typeOf (ContentTie    cs)  = T.TypeTie  (D.typeOf O.<$$> cs)
    typeOf (ContentRel     _)  = T.TypeRel []
    typeOf (ContentInterp  _)  = T.TypeInterp
    typeOf (ContentType    _)  = T.TypeType
    typeOf (ContentEnd      )  = T.TypeEnd

typeSum :: D.Basis c => [c] -> T.Type
typeSum cs = case B.unique $ map D.typeOf cs of
               [t] -> t
               ts  -> T.TypeSum ts

instance D.CContent Content where

instance B.MixEncode Content where
    mixTransEncode sh c =
        case c of
          ContentCode  s   -> B.mixString $ quote  (sh s) s
          ContentText  s   -> B.mixString $ qquote (sh s) s
          ContentTerm  s   -> B.mixString $ "'" ++ S.termNameString s
          ContentDec   n   -> B.mixString $ T.encodeDecimal n
          ContentClock t   -> B.mixEncode t
          ContentTime  t   -> B.mixEncode t
          ContentBool  b   -> B.mixEncode b
          ContentEmpty     -> B.mixString "()"
          ContentEnd       -> B.mixString "(/)"

          ContentList cs   -> D.mixBracketList $ mixBar cs
          ContentSet  cs   -> D.mixBracketSet  $ mixBar cs
          ContentTie  ts   -> S.bracketWith S.bracketTie $ T.termsToMix1 sh ts
          ContentRel  r    -> B.mixTransEncode sh r
          ContentInterp i  -> B.mixEncode i
          ContentType t    -> S.bracketWith S.bracketType $ B.mixEncode t
        where
          mixBar cs   = B.mixJoinBar $ map (B.mixTransEncode sh) cs

quote :: Maybe String -> O.StringMap
quote (Nothing) s   = "'" ++ s
quote (Just s)  _   = s

qquote :: Maybe String -> O.StringMap
qquote (Nothing) "" = "\"\""
qquote (Nothing) s  = S.angleQuote s
qquote (Just s)  _  = s


-- ----------------------  Edge

instance D.CEmpty Content where
    empty                       = ContentEmpty
    isEmpty ContentEmpty        = True
    isEmpty _                   = False

instance D.CEnd Content where
    end                         = ContentEnd
    isEnd ContentEnd            = True
    isEnd _                     = False

-- ----------------------  Simple

instance D.CBool Content where
    pBool                       = ContentBool
    gBool  (ContentBool x)      = x
    gBool  _                    = B.bug "gBool"
    isBool (ContentBool _)      = True
    isBool _                    = False

instance D.CDec Content where
    pDec                        = ContentDec
    gDec  (ContentDec x)        = x
    gDec  _                     = B.bug "gDec"
    isDec (ContentDec _)        = True
    isDec _                     = False

instance D.CClock Content where
    pClock                      = ContentClock
    gClock  (ContentClock x)    = x
    gClock  _                   = B.bug "gClock"
    isClock (ContentClock _)    = True
    isClock _                   = False

instance D.CTime Content where
    pTime                       = ContentTime
    gTime  (ContentTime x)      = x
    gTime  _                    = B.bug "gTime"
    isTime (ContentTime _)      = True
    isTime _                    = False

instance D.CCode Content where
    pCode                       = ContentCode . O.tString
    gCode  (ContentCode s)      = s
    gCode  _                    = B.bug "gCode"
    isCode (ContentCode _)      = True
    isCode _                    = False

instance D.CTerm Content where
    pTerm                       = ContentTerm
    gTerm  (ContentTerm s)      = s
    gTerm  _                    = B.bug "gTerm"
    isTerm (ContentTerm _)      = True
    isTerm _                    = False

instance D.CText Content where
    pText                       = ContentText . O.tString
    gText  (ContentText s)      = s
    gText  _                    = B.bug "gText"
    isText (ContentText _)      = True
    isText _                    = False

-- ----------------------  Complex

instance D.CList Content where
    pList                       = ContentList
    gList  (ContentList cs)     = cs
    gList  _                    = []
    isList (ContentList _)      = True
    isList _                    = False

instance D.CSet Content where
    pSet                        = ContentSet . B.omit D.isEmpty . B.unique
    gSet  (ContentSet x)        = x
    gSet  _                     = B.bug "gSet"
    isSet (ContentSet _)        = True
    isSet _                     = False

instance D.CTie Content where
    pTie                        = ContentTie 
    gTie  (ContentTie ts)       = ts
    gTie  _                     = B.bug "gTie"
    isTie (ContentTie _)        = True
    isTie _                     = False

instance D.CRel Content where
    pRel                        = ContentRel
    gRel  (ContentRel r)        = r
    gRel  _                     = B.bug "gRel"
    isRel (ContentRel _)        = True
    isRel _                     = False

instance D.CInterp Content where
    pInterp                     = ContentInterp
    gInterp  (ContentInterp r)  = r
    gInterp  _                  = B.bug "gInterp"
    isInterp (ContentInterp _)  = True
    isInterp _                  = False

instance D.CType Content where
    pType                       = ContentType
    gType  (ContentType t)      = t
    gType  _                    = B.bug "gType"
    isType (ContentType _)      = True
    isType _                    = False

-- ----------------------  Dispatch

-- | Constants for edge content, i.e., empty and end.
type DispatchEdge a = ( a, a )

-- | Functions for simple content, i.e.,
--   boolean, decimal, clock, time, code, term, and text.
type DispatchSimple a =
    ( Bool        -> a
    , T.Decimal   -> a
    , T.Clock     -> a
    , T.Time      -> a
    , String      -> a
    , S.TermName  -> a
    , String      -> a
    )

-- | Functions for complex content, i.e.,
--   list, set, tie, relation, interp, and type.
type DispatchComplex a =
    ( [Content]   -> a
    , [Content]   -> a
    , [TermC]     -> a
    , RelC        -> a
    , T.Interp    -> a
    , T.Type      -> a
    )

-- | Call type-specific functions on empty or end content.
dispatchEdge :: DispatchEdge a -> (Content -> a) -> (Content -> a)
dispatchEdge (empty, end) other c =
    case c of
      ContentEmpty    -> empty
      ContentEnd      -> end
      _               -> other c

-- | Call type-specific functions on simple content.
dispatchSimple :: DispatchSimple a -> (Content -> a) -> (Content -> a)
dispatchSimple (bool, dec, clock, time, code, term, text) other c =
    case c of
      ContentBool   b -> bool  b
      ContentDec    n -> dec   n
      ContentClock  t -> clock t
      ContentTime   t -> time  t
      ContentCode   s -> code  s
      ContentTerm   s -> term  s
      ContentText   s -> text  s
      _               -> other c

-- | Call type-specific functions on complex content.
dispatchComplex :: DispatchComplex a -> (Content -> a) -> (Content -> a)
dispatchComplex (list, set, tie, rel, interp, type_) other c =
    case c of
      ContentList   cs -> list   cs
      ContentSet    cs -> set    cs
      ContentTie    ts -> tie    ts
      ContentRel     r -> rel    r
      ContentInterp  i -> interp i
      ContentType    t -> type_  t
      _                -> other  c

-- | Call type-specific functions on content.
dispatchContent :: DispatchEdge a -> DispatchSimple a -> DispatchComplex a -> (Content -> a)
dispatchContent e s c = simple where
    simple  = dispatchSimple s edge
    edge    = dispatchEdge e complex
    complex = dispatchComplex c other
    other   = B.bug "dispatchContent"


-- ----------------------  Concrete type

-- | @Judge@ for concrete baala content.
type JudgeC = T.Judge Content

-- | @Term@ for concrete baala content.
type TermC = S.Term Content

-- | @Rel@ for concrete baala content.
type RelC = T.Rel Content

-- | Abortable content.
type AbC = B.Ab Content

-- | Concrete dataset.
type DatasetC = D.Dataset Content

-- | Shorthand function for the Baala content type.
--
--   >>> the $ D.pText "a"
--   ContentText "a"
--
the :: Content -> Content
{-# INLINE the #-}
the = id

-- | Decode the Baala content from string.
--
--   Edge contents.
--
--     >>> stringC "()"
--     Right ContentEmpty
--
--     >>> stringC "(/)"
--     Right ContentEnd
--
--   Numeric contents.
--
--     >>> stringC "(+)"
--     Right (ContentBool True)
--
--     >>> stringC "12.00"
--     Right (ContentDec Decimal (2) 12)
--
--     >>> stringC "|02:30:00|"
--     Right (ContentClock |02:30:00|)
--
--     >>> stringC "2013-04-18"
--     Right (ContentTime 2013-04-18)
--
--   Textual contents.
--
--     >>> stringC "'foo"
--     Right (ContentCode "foo")
--
--     >>> stringC "'/foo"
--     Right (ContentTerm "foo")
--
--     >>> stringC "\"foo\""
--     Right (ContentText "foo")
--
--   Metadata contents.
--
--     >>> stringC "{| Aaa /x bbb /y ccc. |}"
--     Right (ContentInterp (Interp { interpWords = ..., interpTerms = ... }))
--
--     >>> stringC "[- text -]"
--     Right (ContentType TypeText)
--
--   Recursive contents.
--
--     >>> stringC "[ 'foo | 'bar ]"
--     Right (ContentList [ContentCode "foo", ContentCode "bar"])
--
--     >>> stringC "{ 'foo | 'bar }"
--     Right (ContentSet [ContentCode "foo", ContentCode "bar"])
--
--     >>> stringC "{- /x 'foo /y 'bar -}"
--     Right (ContentTie [("x", ContentCode "foo"), ("y", ContentCode "bar")])
--
--     >>> stringC "{= /x /y [ 'foo | 'bar ][ 'baz | 'qux ] =}"
--     Right (ContentRel (Rel { relHead = ..., relBody = ... }))
--
stringC :: String -> B.Ab Content
stringC = D.stringContent


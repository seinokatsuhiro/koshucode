{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Syntax.Token.Token
  (
    -- * Subtype name
    SubtypeName (..),

    -- * Token
    Token (..),
    textToken,
    unknownToken,

    -- * Subtype
    -- ** Text
    TextForm (..),

    -- ** Term
    TermType (..),

    -- ** Local
    LocalRef (..),
    unlocal,

    -- ** Blank
    BlankName (..),
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax.Symbol    as S

-- | Type wihch has subtype and its name.
class SubtypeName a where
    subtypeName :: a -> String


-- --------------------------------------------  Token type

-- | There are eleven types of tokens.
data Token
    = TText     B.CodePt TextForm String
                -- ^ 1) Text.
    | TSlot     B.CodePt Int String
                -- ^ 2) Slot name.
                --      'Int' represents slot level, i.e.,
                --      0 for local positional slots,
                --      1 for local named slots,
                --      2 for global slots
                --      —  @\@@/name/
    | TShort    B.CodePt String String
                -- ^ 3) Prefixed shorten text — /short/@.@/proper/
    | TTermN    B.CodePt Ordering S.TermName
                -- ^ 4) Term name —  @\/@/name/
    | TTerm     B.CodePt TermType S.TermPath
                -- ^ 5) Term path — @\/@/name/@\/@/name/
    | TLocal    B.CodePt (LocalRef String) Int [Token]
                -- ^ 6) Local name — @^\/@/name/
    | TOpen     B.CodePt String
                -- ^ 7) Opening bracket — @(@, @{@, etc
    | TClose    B.CodePt String
                -- ^ 8) Closing bracket — @}@, @)@, etc
    | TSpace    B.CodePt Int
                -- ^ 9) /N/ space characters.
    | TComment  B.CodePt String
                -- ^ 10) Comment — @**@/text/
    | TName     B.CodePt BlankName
                -- ^ 11) Blank name. (This is used in building content expression)
    | TUnknown  B.CodePt String B.AbortReason
                -- ^ 12) Unknown token.
      deriving (Show, Eq, Ord)

-- | @\"text\"@, @\"open\"@, ...
instance SubtypeName Token where
     subtypeName (TText     _ _ _  ) = "text"
     subtypeName (TShort    _ _ _  ) = "short"
     subtypeName (TTermN    _ _ _  ) = "term"
     subtypeName (TTerm     _ _ _  ) = "term"
     subtypeName (TLocal    _ _ _ _) = "local"
     subtypeName (TSlot     _ _ _  ) = "slot"
     subtypeName (TOpen     _ _    ) = "open"
     subtypeName (TClose    _ _    ) = "close"
     subtypeName (TSpace    _ _    ) = "space"
     subtypeName (TComment  _ _    ) = "comment"
     subtypeName (TName     _ _    ) = "name"
     subtypeName (TUnknown  _ _ _  ) = "unknown"

instance B.Name Token where
    name (TTerm     _ _ ns)  = concat ns
    name (TSlot      _ _ s)  = s
    name (TText      _ _ s)  = s
    name (TOpen        _ s)  = s
    name (TClose       _ s)  = s
    name (TComment     _ s)  = s
    name x = error $ "unknown name: " ++ show x

instance B.CodePtr Token where
    codePtList (TText    cp _ _)    = [cp]
    codePtList (TShort   cp _ _)    = [cp]
    codePtList (TTermN   cp _ _)    = [cp]
    codePtList (TTerm    cp _ _)    = [cp]
    codePtList (TLocal   cp _ _ _)  = [cp]
    codePtList (TSlot    cp _ _)    = [cp]
    codePtList (TOpen    cp _)      = [cp]
    codePtList (TClose   cp _)      = [cp]
    codePtList (TSpace   cp _)      = [cp]
    codePtList (TComment cp _)      = [cp]
    codePtList (TName    cp _)      = [cp]
    codePtList (TUnknown cp _ _)    = [cp]

instance B.PPrint Token where
    pprint = d where
        d (TText      cp q w)    = pretty "TText"    cp [show q, show w]
        d (TShort     cp a b)    = pretty "TShort"   cp [show a, show b]
        d (TTermN     cp _ n)    = pretty "TTermN"   cp [show n]
        d (TTerm      cp q ns)   = pretty "TTerm"    cp [show q, show ns]
        d (TLocal     cp n _ _)  = pretty "TLocal"   cp [show n]
        d (TSlot      cp n w)    = pretty "TSlot"    cp [show n, show w]
        d (TOpen      cp p)      = pretty "TOpen"    cp [show p]
        d (TClose     cp p)      = pretty "TClose"   cp [show p]
        d (TSpace     cp c)      = pretty "TSpace"   cp [show c]
        d (TComment   cp s)      = pretty "TComment" cp [show s]
        d (TName      cp w)      = pretty "TName"    cp [show w]
        d (TUnknown   cp w _)    = pretty "TUnknown" cp [show w]

        pretty k cp xs         = B.pprintH $ lineCol cp : k : xs
        lineCol cp             = (show $ B.codePtLineNo cp)
                                 ++ "." ++ (show $ B.codePtColumnNo cp)

-- | Create raw text token.
textToken :: String -> Token
textToken = TText B.def TextRaw

-- | Create unknown token.
unknownToken :: B.CodePt -> String -> B.Ab a -> Token
unknownToken cp w (Left a)  = TUnknown cp w a
unknownToken cp w (Right _) = TUnknown cp w $ B.abortBecause "bug?"


-- --------------------------------------------  Subtype

-- ----------------------  Text

-- | Subtype of text token.
data TextForm
    = TextUnk      -- ^ Unknown keyword
    | TextRaw      -- ^ Naked text
    | TextQ        -- ^ Single-quoted text — @\'@/code/
    | TextQQ       -- ^ Double-quoted text — @\"@/text/@\"@
    | TextKey      -- ^ Keyword literal — @<@/keyword/@>@
    | TextBar      -- ^ Text enclosed in bars — @|@/text/@|@
    | TextLicense  -- ^ Text in license section
      deriving (Show, Eq, Ord)

-- | @\"raw\"@, @\"q\"@, ...
instance SubtypeName TextForm where
    subtypeName TextUnk      = "unknown"
    subtypeName TextRaw      = "raw"
    subtypeName TextQ        = "q"
    subtypeName TextQQ       = "qq"
    subtypeName TextKey      = "key"
    subtypeName TextBar      = "bar"
    subtypeName TextLicense  = "license"


-- ----------------------  Term

-- | Type of term name.
data TermType
    = TermTypePath               -- ^ Normal term path
    | TermTypeQuoted             -- ^ Quoted term name
      deriving (Show, Eq, Ord)


-- ----------------------  Local

-- | Local relation reference.
data LocalRef a
    = LocalSymbol a        -- ^ Reference to local relation — @^r@
    | LocalNest a          -- ^ Reference to nested relation — @^/r@
      deriving (Show, Eq, Ord)

-- | Get name of local relation reference.
unlocal :: LocalRef a -> a
unlocal (LocalNest   a) = a
unlocal (LocalSymbol a) = a


-- ----------------------  Blank

-- | Blank in form.
data BlankName
    = BlankNormal   String
    | BlankInternal String
    | BlankPrefix   String
    | BlankInfix    String
    | BlankPostfix  String
    deriving (Show, Eq, Ord)

instance B.Name BlankName where
    name (BlankNormal   n)   = n
    name (BlankInternal n)   = n
    name (BlankPrefix   n)   = n
    name (BlankInfix    n)   = n
    name (BlankPostfix  n)   = n

-- | @\"normal\"@, @\"internal\"@, ...
instance SubtypeName BlankName where
    subtypeName (BlankNormal   _) = "normal"
    subtypeName (BlankInternal _) = "internal"
    subtypeName (BlankPrefix   _) = "prefix"
    subtypeName (BlankInfix    _) = "infix"
    subtypeName (BlankPostfix  _) = "postfix"


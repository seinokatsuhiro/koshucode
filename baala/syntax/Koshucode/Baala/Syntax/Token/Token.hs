{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Syntax.Token.Token
  (
    -- * Subtype string
    SubtypeString (..),

    -- * Token
    Token (..),
    textToken,

    -- * Detail type
    -- ** Text
    TextForm (..),

    -- ** Term
    TermType (..),

    -- ** Local
    Local (..),
    unlocal,

    -- ** Blank
    BlankName (..),
  ) where

import qualified Data.Generics                    as G
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax.Symbol    as S


class SubtypeString a where
    subtypeString :: a -> String


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
    | TLocal    B.CodePt (Local String) Int [Token]
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
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | @\"text\"@, @\"open\"@, ...
instance SubtypeString Token where
     subtypeString (TText     _ _ _  ) = "text"
     subtypeString (TShort    _ _ _  ) = "short"
     subtypeString (TTermN    _ _ _  ) = "term"
     subtypeString (TTerm     _ _ _  ) = "term"
     subtypeString (TLocal    _ _ _ _) = "local"
     subtypeString (TSlot     _ _ _  ) = "slot"
     subtypeString (TOpen     _ _    ) = "open"
     subtypeString (TClose    _ _    ) = "close"
     subtypeString (TSpace    _ _    ) = "space"
     subtypeString (TComment  _ _    ) = "comment"
     subtypeString (TName     _ _    ) = "name"

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

instance B.Write Token where
    writeDocWith sh = d where
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

        pretty k cp xs         = B.writeH sh $ lineCol cp : k : xs
        lineCol cp             = (show $ B.codePtLineNo cp)
                                 ++ "." ++ (show $ B.codePtColumnNo cp)

-- | Create raw text token.
textToken :: String -> Token
textToken = TText B.def TextRaw


-- --------------------------------------------  Detail type

-- ----------------------  Text

data TextForm
    = TextUnk      -- ^ Unknown keyword
    | TextRaw      -- ^ Naked text
    | TextQ        -- ^ Single-quoted text — @\'@/code/
    | TextQQ       -- ^ Double-quoted text — @\"@/text/@\"@
    | TextKey      -- ^ Keyword literal — @<@/keyword/@>@
    | TextBar      -- ^ Text enclosed in bars — @|@/text/@|@
    | TextLicense  -- ^ Text in license section
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | @\"raw\"@, @\"q\"@, ...
instance SubtypeString TextForm where
    subtypeString TextUnk      = "unknown"
    subtypeString TextRaw      = "raw"
    subtypeString TextQ        = "q"
    subtypeString TextQQ       = "qq"
    subtypeString TextKey      = "key"
    subtypeString TextBar      = "bar"
    subtypeString TextLicense  = "license"


-- ----------------------  Term

data TermType
    = TermTypePath               -- ^ Normal term path
    | TermTypeQuoted             -- ^ Quoted term name
      deriving (Show, Eq, Ord, G.Data, G.Typeable)


-- ----------------------  Local

data Local a
    = LocalSymbol a
    | LocalNest a
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

unlocal :: Local a -> a
unlocal (LocalNest   a) = a
unlocal (LocalSymbol a) = a


-- ----------------------  Blank

data BlankName
    = BlankNormal   String
    | BlankInternal String
    | BlankPrefix   String
    | BlankInfix    String
    | BlankPostfix  String
    deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name BlankName where
    name (BlankNormal   n)   = n
    name (BlankInternal n)   = n
    name (BlankPrefix   n)   = n
    name (BlankInfix    n)   = n
    name (BlankPostfix  n)   = n

-- | @\"normal\"@, @\"internal\"@, ...
instance SubtypeString BlankName where
    subtypeString (BlankNormal   _) = "normal"
    subtypeString (BlankInternal _) = "internal"
    subtypeString (BlankPrefix   _) = "prefix"
    subtypeString (BlankInfix    _) = "infix"
    subtypeString (BlankPostfix  _) = "postfix"


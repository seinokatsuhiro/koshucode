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
    nameToken,

    -- * TextForm
    TextForm (..),
    -- ** Pattern
    pattern TTextUnk,
    pattern TTextRaw,
    pattern TTextQ,
    pattern TTextQQ,
    pattern TTextKey,
    pattern TTextBar,
    pattern TTextName,
    pattern TTextLicense,
    pattern TTextSect,

    -- * BlankName
    BlankName (..),

    -- * TermType
    TermType (..),
    -- ** Pattern
    pattern TTermPath,
    pattern TTermQ,

    -- * Local
    Local (..),
    unlocal,
  ) where

import qualified Data.Generics                    as G
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax.Symbol    as S


class SubtypeString a where
    subtypeString :: a -> String


-- ----------------------  Token type

-- | There are nine types of tokens.
data Token
    = TText     B.CodePt TextForm String      -- ^ Text.
    | TName     B.CodePt BlankName            -- ^ Blank name.
    | TSlot     B.CodePt Int String           -- ^ Slot name.
                                              --   'Int' represents slot level, i.e.,
                                              --   0 for local positional slots,
                                              --   1 for local named slots,
                                              --   2 for global slots.
    | TShort    B.CodePt String String        -- ^ Prefixed shorten text.
    | TTermN    B.CodePt Ordering S.TermName  -- ^ Term name.
    | TTerm     B.CodePt TermType S.TermPath  -- ^ Term path.
    | TLocal    B.CodePt (Local String) Int [Token]  -- ^ Local name.
    | TOpen     B.CodePt String               -- ^ Opening bracket.
    | TClose    B.CodePt String               -- ^ Closing bracket.
    | TSpace    B.CodePt Int                  -- ^ /N/ space characters.
    | TComment  B.CodePt String               -- ^ Comment.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | @\"text\"@, @\"open\"@, ...
instance SubtypeString Token where
     subtypeString (TText     _ _ _  ) = "text"
     subtypeString (TName     _ _    ) = "name"
     subtypeString (TShort    _ _ _  ) = "short"
     subtypeString (TTermN    _ _ _  ) = "term"
     subtypeString (TTerm     _ _ _  ) = "term"
     subtypeString (TLocal    _ _ _ _) = "local"
     subtypeString (TSlot     _ _ _  ) = "slot"
     subtypeString (TOpen     _ _    ) = "open"
     subtypeString (TClose    _ _    ) = "close"
     subtypeString (TSpace    _ _    ) = "space"
     subtypeString (TComment  _ _    ) = "comment"

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
    codePtList (TName    cp _)      = [cp]
    codePtList (TShort   cp _ _)    = [cp]
    codePtList (TTermN   cp _ _)    = [cp]
    codePtList (TTerm    cp _ _)    = [cp]
    codePtList (TLocal   cp _ _ _)  = [cp]
    codePtList (TSlot    cp _ _)    = [cp]
    codePtList (TOpen    cp _)      = [cp]
    codePtList (TClose   cp _)      = [cp]
    codePtList (TSpace   cp _)      = [cp]
    codePtList (TComment cp _)      = [cp]

instance B.Write Token where
    writeDocWith sh = d where
        d (TText      cp q w)    = pretty "TText"    cp [show q, show w]
        d (TName      cp w)      = pretty "TName"    cp [show w]
        d (TShort     cp a b)    = pretty "TShort"   cp [show a, show b]
        d (TTermN     cp _ n)    = pretty "TTermN"   cp [show n]
        d (TTerm      cp q ns)   = pretty "TTerm"    cp [show q, show ns]
        d (TLocal     cp n _ _)  = pretty "TLocal"   cp [show n]
        d (TSlot      cp n w)    = pretty "TSlot"    cp [show n, show w]
        d (TOpen      cp p)      = pretty "TOpen"    cp [show p]
        d (TClose     cp p)      = pretty "TClose"   cp [show p]
        d (TSpace     cp c)      = pretty "TSpace"   cp [show c]
        d (TComment   cp s)      = pretty "TComment" cp [show s]
        pretty k cp xs         = B.writeH sh $ lineCol cp : k : xs
        lineCol cp             = (show $ B.codePtLineNo cp)
                                 ++ "." ++ (show $ B.codePtColumnNo cp)

-- | Create raw text token.
textToken :: String -> Token
textToken = TTextRaw B.def

-- | Create normal name token.
nameToken :: String -> Token
nameToken = TName B.def . BlankNormal


-- ----------------------  TextForm

data TextForm
    = TextUnk      -- ^ Unknown keyword
    | TextRaw      -- ^ Naked text
    | TextQ        -- ^ Single-quoted text
    | TextQQ       -- ^ Double-quoted text
    | TextKey      -- ^ Keyword literal
    | TextBar      -- ^ Text enclosed in bars
    | TextName     -- ^ Text used as name
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
    subtypeString TextName     = "name"
    subtypeString TextLicense  = "license"

-- | Unknown text token.
pattern TTextUnk cp w = TText cp TextUnk  w

-- | Raw text token.
--
--   >>> TTextRaw B.def "a"   -- a
pattern TTextRaw cp w = TText cp TextRaw  w

-- | Quoted text token.
--
--   >>> TTextQ B.def "a"   -- 'a
pattern TTextQ cp w = TText cp TextQ    w

-- | Dobule-quoted text token.
--
--   >>> TTextQQ B.def "a"   -- "a"
pattern TTextQQ cp w = TText cp TextQQ   w

-- | Keyword token.
--
--   >>> TTextKey B.def "a"   -- <a>
pattern TTextKey cp w = TText cp TextKey  w

-- | Bar-enclosed token.
--
--   >>> TTextBar B.def "a"   -- |a|
pattern TTextBar cp w = TText cp TextBar  w

pattern TTextName cp w = TText cp TextName w

pattern TTextLicense cp w = TText cp TextLicense w

pattern TTextSect cp = TTextRaw cp "==="


-- ----------------------  Term type

data TermType
    = TermTypePath               -- ^ Normal term path
    | TermTypeQuoted             -- ^ Quoted term name
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Term path token.
--
--   >>> TTermPath B.def ["a", "b"]   -- /a/b
pattern TTermPath cp ws = TTerm cp TermTypePath ws

-- | Quoted term path token.
--
--   >>> TTermQ B.def ["a"]   -- '/a
pattern TTermQ cp ws = TTerm cp TermTypeQuoted ws


-- ----------------------  Blank name

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

instance B.Write BlankName where
    writeDocWith sh (BlankNormal   n)   = B.writeDocWith sh n
    writeDocWith sh (BlankInternal n)   = B.writeDocWith sh n
    writeDocWith sh (BlankPrefix   n)   = B.writeDocWith sh n B.<+> B.doc "(prefix)"
    writeDocWith sh (BlankInfix    n)   = B.writeDocWith sh n B.<+> B.doc "(infix)"
    writeDocWith sh (BlankPostfix  n)   = B.writeDocWith sh n B.<+> B.doc "(postfix)"


-- ----------------------  Local

data Local a
    = LocalSymbol a
    | LocalNest a
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

unlocal :: Local a -> a
unlocal (LocalNest   a) = a
unlocal (LocalSymbol a) = a

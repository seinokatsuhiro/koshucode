{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Syntax.Token.Token
  (
    -- * Subtype name
    SubtypeName (..),

    -- * Token
    Token,
    TToken (..),
    tokenCp,
    rawTextToken,
    unknownToken,

    -- * Subtype
    -- ** Text
    TextForm (..),
    -- ** Local
    LocalRef (..),
    -- ** Blank
    BlankName (..),
  ) where

import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax.Symbol    as S

-- | Type wihch has subtype and its name.
class SubtypeName a where
    subtypeName :: a -> String


-- --------------------------------------------  Token type

-- | String token.
type Token = TToken String

-- | Textual token.
data TToken t
    = TText     (B.TCodePos t) TextForm t
                -- ^ __1 Textual:__ Text — @'code@, @"text"@, etc
    | TShort    (B.TCodePos t) t t
                -- ^ __2 Textual:__ Prefixed shorten text — @short.proper@
    | TTerm     (B.TCodePos t) t
                -- ^ __3 Textual:__ Term name — @\/term@

    | TLocal    (B.TCodePos t) LocalRef Int [Token]
                -- ^ __4 Symbolic:__ Local name — @^r@, @^\/r@
    | TSlot     (B.TCodePos t) Int t
                -- ^ __5 Symbolic:__ Slot name.
                --   'Int' represents slot level, i.e.,
                --   0 for local positional slots,
                --   1 for local named slots,
                --   2 for global slots
                --   —  @\@slot@, @\@\@global@
    | TName     (B.TCodePos t) BlankName
                -- ^ __6 Symbolic:__ Blank name.
                --   (This is only used in building content expression)

    | TOpen     (B.TCodePos t) t
                -- ^ __7 Punctuational:__ Opening bracket — @(@, @{@, @{=@, etc
    | TClose    (B.TCodePos t) t
                -- ^ __8 Punctuational:__ Closing bracket — @=}@, @}@, @)@, etc
    | TSpace    (B.TCodePos t) Int
                -- ^ __9 Punctuational:__ /N/ space characters
    | TComment  (B.TCodePos t) t
                -- ^ __10 Punctuational:__ Comment — @** comment line@
    | TUnknown  (B.TCodePos t) t B.AbortReason
                -- ^ __11 Other:__ Unknown token

      deriving (Show, Eq, Ord)

-- | @\"text\"@, @\"open\"@, ...
instance SubtypeName (TToken t) where
     subtypeName (TText     _ _ _  ) = "text"
     subtypeName (TShort    _ _ _  ) = "short"
     subtypeName (TTerm     _ _    ) = "term"
     subtypeName (TLocal    _ _ _ _) = "local"
     subtypeName (TSlot     _ _ _  ) = "slot"
     subtypeName (TOpen     _ _    ) = "open"
     subtypeName (TClose    _ _    ) = "close"
     subtypeName (TSpace    _ _    ) = "space"
     subtypeName (TComment  _ _    ) = "comment"
     subtypeName (TName     _ _    ) = "name"
     subtypeName (TUnknown  _ _ _  ) = "unknown"

instance (O.Textual t) => B.GetCodePos (TToken t) where
    getCPs tok = [B.cpStringify $ tokenCp tok]

-- | Code point of token.
tokenCp :: TToken t -> B.TCodePos t
tokenCp (TText    cp _ _)    = cp
tokenCp (TShort   cp _ _)    = cp
tokenCp (TTerm    cp _)      = cp
tokenCp (TLocal   cp _ _ _)  = cp
tokenCp (TSlot    cp _ _)    = cp
tokenCp (TOpen    cp _)      = cp
tokenCp (TClose   cp _)      = cp
tokenCp (TSpace   cp _)      = cp
tokenCp (TComment cp _)      = cp
tokenCp (TName    cp _)      = cp
tokenCp (TUnknown cp _ _)    = cp

-- | Create raw text token.
rawTextToken :: (O.Textual t) => t -> TToken t
rawTextToken = TText B.def TextRaw

-- | Create unknown token.
unknownToken :: B.TCodePos t -> t -> B.Ab a -> TToken t
unknownToken cp w (Left a)  = TUnknown cp w a
unknownToken cp w (Right _) = TUnknown cp w $ B.abortBecause "bug?"


-- --------------------------------------------  Subtype

-- ----------------------  Text

-- | Subtype of text token.
data TextForm
    = TextUnk      -- ^ __1.__ Unknown keyword — @\<unknown\>@
    | TextRaw      -- ^ __2.__ Naked text — @raw@
    | TextQ        -- ^ __3.__ Single-quoted text — @\'code@
    | TextQQ       -- ^ __4.__ Double-quoted text — @\"text\"@
    | TextKey      -- ^ __5.__ Keyword literal — @\<crlf\>@
    | TextTerm     -- ^ __6.__ Quoted term name — @'/term@
    | TextBar      -- ^ __7.__ Text enclosed in bars — @|9:30|@
    | TextLicense  -- ^ __8.__ Text in license section — @=== license@
      deriving (Show, Eq, Ord)

-- | @\"raw\"@, @\"q\"@, ...
instance SubtypeName TextForm where
    subtypeName TextUnk      = "unknown"
    subtypeName TextRaw      = "raw"
    subtypeName TextQ        = "q"
    subtypeName TextQQ       = "qq"
    subtypeName TextKey      = "key"
    subtypeName TextTerm     = "term"
    subtypeName TextBar      = "bar"
    subtypeName TextLicense  = "license"


-- ----------------------  Local

-- | Local relation reference.
data LocalRef
    = LocalSymbol String     -- ^ Reference to local relation — @^r@
    | LocalNest S.TermName   -- ^ Reference to nested relation — @^/r@
      deriving (Show, Eq, Ord)

-- | Get name of local relation reference.
instance B.Name LocalRef where
    name (LocalNest   n) = S.termNameContent n
    name (LocalSymbol n) = n


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


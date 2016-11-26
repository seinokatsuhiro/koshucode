{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Syntax.Token.Token
  (
    -- * Subtype name
    SubtypeName (..),

    -- * Token
    Token (..),
    rawTextToken,
    unknownToken,

    -- * Subtype
    -- ** Text
    TextForm (..),
    -- ** Local
    LocalRef (..),
    -- ** Blank
    BlankName (..),

    -- * Pattern
    pattern TTextUnk,
    pattern TTextRaw,
    pattern TTextQ,
    pattern TTextQQ,
    pattern TTextKey,
    pattern TTextBar,
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax.Symbol    as S

-- | Type wihch has subtype and its name.
class SubtypeName a where
    subtypeName :: a -> String


-- --------------------------------------------  Token type

-- | There are eleven types of tokens.
data Token
    = TText     B.CodePos TextForm String
                -- ^ __1 Textual:__ Text — @'code@, @"text"@, etc
    | TShort    B.CodePos String String
                -- ^ __2 Textual:__ Prefixed shorten text — @short.proper@
    | TTerm     B.CodePos String
                -- ^ __3 Textual:__ Term name — @\/term@

    | TLocal    B.CodePos LocalRef Int [Token]
                -- ^ __4 Symbolic:__ Local name — @^r@, @^\/r@
    | TSlot     B.CodePos Int String
                -- ^ __5 Symbolic:__ Slot name.
                --   'Int' represents slot level, i.e.,
                --   0 for local positional slots,
                --   1 for local named slots,
                --   2 for global slots
                --   —  @\@slot@, @\@\@global@
    | TName     B.CodePos BlankName
                -- ^ __6 Symbolic:__ Blank name.
                --   (This is only used in building content expression)

    | TOpen     B.CodePos String
                -- ^ __7 Punctuational:__ Opening bracket — @(@, @{@, @{=@, etc
    | TClose    B.CodePos String
                -- ^ __8 Punctuational:__ Closing bracket — @=}@, @}@, @)@, etc
    | TSpace    B.CodePos Int
                -- ^ __9 Punctuational:__ /N/ space characters
    | TComment  B.CodePos String
                -- ^ __10 Punctuational:__ Comment — @** comment line@
    | TUnknown  B.CodePos String B.AbortReason
                -- ^ __11 Other:__ Unknown token

      deriving (Show, Eq, Ord)

-- | @\"text\"@, @\"open\"@, ...
instance SubtypeName Token where
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

instance B.Name Token where
    name (TSlot      _ _ s)  = s
    name (TText      _ _ s)  = s
    name (TOpen        _ s)  = s
    name (TClose       _ s)  = s
    name (TComment     _ s)  = s
    name x = error $ "unknown name: " ++ show x

instance B.GetCodePos Token where
    getCPs (TText    cp _ _)    = [cp]
    getCPs (TShort   cp _ _)    = [cp]
    getCPs (TTerm    cp _)      = [cp]
    getCPs (TLocal   cp _ _ _)  = [cp]
    getCPs (TSlot    cp _ _)    = [cp]
    getCPs (TOpen    cp _)      = [cp]
    getCPs (TClose   cp _)      = [cp]
    getCPs (TSpace   cp _)      = [cp]
    getCPs (TComment cp _)      = [cp]
    getCPs (TName    cp _)      = [cp]
    getCPs (TUnknown cp _ _)    = [cp]

instance B.PPrint Token where
    pprint = d where
        d (TText     cp q w)    = pp2 "TText"    cp q w
        d (TShort    cp a b)    = pp2 "TShort"   cp a b
        d (TTerm     cp n)      = pp1 "TTerm"    cp n
        d (TLocal    cp n _ _)  = pp1 "TLocal"   cp n
        d (TSlot     cp n w)    = pp2 "TSlot"    cp n w
        d (TOpen     cp p)      = pp1 "TOpen"    cp p
        d (TClose    cp p)      = pp1 "TClose"   cp p
        d (TSpace    cp c)      = pp1 "TSpace"   cp c
        d (TComment  cp s)      = pp1 "TComment" cp s
        d (TName     cp w)      = pp1 "TName"    cp w
        d (TUnknown  cp w _)    = pp1 "TUnknown" cp w

        pp1 k cp x   = B.pprintH [B.pprint k, B.pprint cp, B.pprint $ show x]
        pp2 k cp x y = pp1 k cp x B.<+> B.pprint (show y)

-- | Create raw text token.
rawTextToken :: String -> Token
rawTextToken = TText B.def TextRaw

-- | Create unknown token.
unknownToken :: B.CodePos -> String -> B.Ab a -> Token
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


-- --------------------------------------------  Pattern

-- | Unknown text token.
{-# DEPRECATED TTextUnk "Use TUnk instead." #-}
pattern TTextUnk cp w = TText cp TextUnk  w

-- | Raw text token.
pattern TTextRaw cp w = TText cp TextRaw  w

-- | Quoted text token.
{-# DEPRECATED TTextQ "Use TQq instead." #-}
pattern TTextQ cp w = TText cp TextQ    w

-- | Dobule-quoted text token.
{-# DEPRECATED TTextQQ "Use TQq instead." #-}
pattern TTextQQ cp w = TText cp TextQQ   w

-- | Keyword token.
{-# DEPRECATED TTextKey "Use TKey instead." #-}
pattern TTextKey cp w = TText cp TextKey  w

-- | Bar-enclosed token.
{-# DEPRECATED TTextBar "Use TBar instead." #-}
pattern TTextBar cp w = TText cp TextBar  w


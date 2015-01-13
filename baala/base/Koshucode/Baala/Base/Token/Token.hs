{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Base.Token.Token
  (
    -- * Token
    Token (..),
    BlankName (..),
    textToken,
    nameToken,
  
    -- * TextForm
    TextForm (..),
    pattern TTextUnk,
    pattern TTextRaw,
    pattern TTextQ,
    pattern TTextQQ,
    pattern TTextKey,
    pattern TTextBar,
    pattern TTextName,
    pattern TTextSect,

    -- * Term name
    TermName, TermName2, TermName3, TermName4,
    Terminal, TermPath,
    showTermName,
    showNestedTermName,
  
    -- * Selectors
    tokenContent,
    tokenTypeText, tokenSubtypeText,
    -- $Selector
  
    -- * Predicates
    isBlankToken, sweepToken,
    isShortToken, isTermToken,
    isOpenToken, isCloseToken,
    isOpenTokenOf, isCloseTokenOf,
    -- $Predicate
  ) where

import qualified Data.Generics                  as G
import qualified Koshucode.Baala.Base.Prelude   as B
import qualified Koshucode.Baala.Base.Text      as B


-- ----------------------  Token type

-- | There are nine types of tokens.
data Token
    = TText    B.CodePt TextForm String   -- ^ Text.
    | TName    B.CodePt BlankName         -- ^ Blank name.
    | TSlot    B.CodePt Int String        -- ^ Slot name.
                                          --   'Int' represents slot level, i.e.,
                                          --   0 for local positional slots,
                                          --   1 for local named slots,
                                          --   2 for global slots.
    | TShort   B.CodePt String String     -- ^ Prefixed shorten text.
    | TTerm    B.CodePt Int TermPath      -- ^ Term path.
    | TOpen    B.CodePt String            -- ^ Opening bracket.
    | TClose   B.CodePt String            -- ^ Closing bracket.
    | TSpace   B.CodePt Int               -- ^ /N/ space characters.
    | TComment B.CodePt String            -- ^ Comment.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name Token where
    name (TTerm  _ _ ns)  = concat ns
    name (TSlot   _ _ s)  = s
    name (TText   _ _ s)  = s
    name (TOpen     _ s)  = s
    name (TClose    _ s)  = s
    name (TComment  _ s)  = s
    name x = error $ "unknown name: " ++ show x

instance B.Write Token where
    write sh = d where
        d (TText    pt q w)    = pretty "TText"    pt [show q, show w]
        d (TName    pt w)      = pretty "TName"    pt [show w]
        d (TShort   pt a b)    = pretty "TShort"   pt [show a, show b]
        d (TTerm    pt q ns)   = pretty "TTerm"    pt [show q, show ns]
        d (TSlot    pt n w)    = pretty "TSlot"    pt [show n, show w]
        d (TOpen    pt p)      = pretty "TOpen"    pt [show p]
        d (TClose   pt p)      = pretty "TClose"   pt [show p]
        d (TSpace   pt c)      = pretty "TSpace"   pt [show c]
        d (TComment pt s)      = pretty "TComment" pt [show s]
        pretty k pt xs         = B.writeH sh $ lineCol pt : k : xs
        lineCol pt             = (show $ B.codePtLineNo pt)
                                 ++ "." ++ (show $ B.codePtColumnNo pt)

textToken :: String -> Token
textToken = TText B.codePtZero TextRaw

nameToken :: String -> Token
nameToken = TName B.codePtZero . BlankNormal

instance B.CodePtr Token where
    codePtList (TText    cp _ _)   = [cp]
    codePtList (TName    cp _)     = [cp]
    codePtList (TShort   cp _ _)   = [cp]
    codePtList (TTerm    cp _ _)   = [cp]
    codePtList (TSlot    cp _ _)   = [cp]
    codePtList (TOpen    cp _)     = [cp]
    codePtList (TClose   cp _)     = [cp]
    codePtList (TSpace   cp _)     = [cp]
    codePtList (TComment cp _)     = [cp]


-- ----------------------  TextForm

data TextForm
    = TextUnk     -- ^ Unknown keyword
    | TextRaw     -- ^ Naked text
    | TextQ       -- ^ Single-quoted text
    | TextQQ      -- ^ Double-quoted text
    | TextKey     -- ^ Keyword literal
    | TextBar     -- ^ Text enclosed in bars
    | TextName    -- ^ Text used as name
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

pattern TTextUnk  cp w   = TText cp TextUnk  w
pattern TTextRaw  cp w   = TText cp TextRaw  w
pattern TTextQ    cp w   = TText cp TextQ    w
pattern TTextQQ   cp w   = TText cp TextQQ   w
pattern TTextKey  cp w   = TText cp TextKey  w
pattern TTextBar  cp w   = TText cp TextBar  w
pattern TTextName cp w   = TText cp TextName w
pattern TTextSect cp     = TTextRaw cp "==="

textFormTypeText :: TextForm -> String
textFormTypeText form =
    case form of
      TextUnk   -> "unknown"
      TextRaw   -> "raw"
      TextQ     -> "q"
      TextQQ    -> "qq"
      TextKey   -> "key"
      TextBar   -> "bar"
      TextName  -> "name"


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

instance B.Write BlankName where
    write sh (BlankNormal   n)   = B.write sh n
    write sh (BlankInternal n)   = B.write sh n
    write sh (BlankPrefix   n)   = B.write sh n B.<+> B.doc "(prefix)"
    write sh (BlankInfix    n)   = B.write sh n B.<+> B.doc "(infix)"
    write sh (BlankPostfix  n)   = B.write sh n B.<+> B.doc "(postfix)"

blankNameTypeText :: BlankName -> String
blankNameTypeText n =
    case n of
      BlankNormal   _   -> "normal"
      BlankInternal _   -> "internal"
      BlankPrefix   _   -> "prefix"
      BlankInfix    _   -> "infix"
      BlankPostfix  _   -> "postfix"


-- ---------------------- Term name

-- | Name of term, e.g., @\"file\"@ for the term name @\/file@.
type TermName    = String
type TermName2   = (String, String)
type TermName3   = (String, String, String)
type TermName4   = (String, String, String, String)

-- | Pair of term name and something.
type Terminal a  = (TermName, a)

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath    = [TermName]

showTermName :: B.Map String
showTermName n = ('/' : n)

showNestedTermName :: [String] -> String
showNestedTermName = concat . map showTermName



-- ----------------------  Selector

-- $Selector
--
--   >>> let tok = TTerm B.codePtZero 0 ["r", "x"] in tokenContent tok
--   "/r/x"
--
--   >>> let tok = textToken "flower" in (tokenTypeText tok, tokenSubtypeText tok)
--   ("text", Just "raw")

-- | Get the content of token.
tokenContent :: Token -> String
tokenContent tok =
    case tok of
      TText    _ _ s   -> s
      TName    _ op    -> B.name op
      TShort   _ a b   -> a ++ "." ++ b
      TTerm    _ _ ns  -> concatMap ('/' :) ns
      TSlot    _ _ s   -> s
      TOpen    _ s     -> s
      TClose   _ s     -> s
      TSpace   _ n     -> replicate n ' '
      TComment _ s     -> s

-- | Text of token type, e.g., @\"text\"@, @\"open\"@.
tokenTypeText :: Token -> String
tokenTypeText tok =
    case tok of
      TText    _ _ _   -> "text"
      TName    _ _     -> "name"
      TShort   _ _ _   -> "short"
      TTerm    _ _ _   -> "term"
      TSlot    _ _ _   -> "slot"
      TOpen    _ _     -> "open"
      TClose   _ _     -> "close"
      TSpace   _ _     -> "space"
      TComment _ _     -> "comment"

tokenSubtypeText :: Token -> Maybe String
tokenSubtypeText tok =
    case tok of
      TText    _ f _   -> Just $ textFormTypeText f
      TName    _ b     -> Just $ blankNameTypeText b
      TShort   _ _ _   -> Nothing
      TTerm    _ _ _   -> Nothing
      TSlot    _ n _   -> Just $ slotTypeText n
      TOpen    _ _     -> Nothing
      TClose   _ _     -> Nothing
      TSpace   _ _     -> Nothing
      TComment _ _     -> Nothing

slotTypeText :: Int -> String
slotTypeText 0   = "positional"
slotTypeText 1   = "named"
slotTypeText 2   = "global"
slotTypeText _   = "unknown"


-- ----------------------  Predicate

-- $Predicate
--
--   >>> let tok = TOpen B.codePtZero "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = TOpen B.codePtZero "{" in isOpenTokenOf "(" tok
--   False

-- | Test the token is blank, i.e., comment or space.
isBlankToken :: B.Pred Token
isBlankToken (TSpace _ _)       = True
isBlankToken (TComment _ _)     = True
isBlankToken _                  = False

-- | Remove blank tokens.
sweepToken :: B.Map [Token]
sweepToken = B.omit isBlankToken

isShortToken :: B.Pred Token
isShortToken (TShort _ _ _)     = True
isShortToken _                  = False

isTermToken :: B.Pred Token
isTermToken (TTerm _ _ _)       = True
isTermToken _                   = False

isOpenToken :: B.Pred Token
isOpenToken (TOpen _ _)         = True
isOpenToken _                   = False

isCloseToken :: B.Pred Token
isCloseToken (TClose _ _)       = True
isCloseToken _                  = False

-- | Check token is a 'TOpen' of the specific bracket.
isOpenTokenOf :: String -> B.Pred Token
isOpenTokenOf p1 (TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _               = False

-- | Check token is a 'TClose' of the specific bracket.
isCloseTokenOf :: String -> B.Pred Token
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False


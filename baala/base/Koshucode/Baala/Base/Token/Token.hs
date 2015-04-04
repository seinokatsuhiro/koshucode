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

    -- * Local
    Local (..),
    unlocal,
  
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

    -- * TermType
    TermType (..),
    pattern TTermPath,
    pattern TTermQ,

    -- * Term name
    TermName, TermName2, TermName3, TermName4,
    TermPath,
    showTermName,
    showNestedTermName,
    termP, termN, termPN,
    termsP, termsN, termsPN,
  
    -- * Selectors
    tokenContent,
    tokenTypeText, tokenSubtypeText,
    tokenParents,
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
    = TText     B.CodePt TextForm String    -- ^ Text.
    | TName     B.CodePt BlankName          -- ^ Blank name.
    | TSlot     B.CodePt Int String         -- ^ Slot name.
                                            --   'Int' represents slot level, i.e.,
                                            --   0 for local positional slots,
                                            --   1 for local named slots,
                                            --   2 for global slots.
    | TShort    B.CodePt String String      -- ^ Prefixed shorten text.
    | TTerm     B.CodePt TermType TermPath  -- ^ Term path.
    | TLocal    B.CodePt (Local String) Int [Token]  -- ^ Local name.
    | TOpen     B.CodePt String             -- ^ Opening bracket.
    | TClose    B.CodePt String             -- ^ Closing bracket.
    | TSpace    B.CodePt Int                -- ^ /N/ space characters.
    | TComment  B.CodePt String             -- ^ Comment.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name Token where
    name (TTerm     _ _ ns)  = concat ns
    name (TSlot      _ _ s)  = s
    name (TText      _ _ s)  = s
    name (TOpen        _ s)  = s
    name (TClose       _ s)  = s
    name (TComment     _ s)  = s
    name x = error $ "unknown name: " ++ show x

instance B.Write Token where
    write sh = d where
        d (TText      cp q w)    = pretty "TText"    cp [show q, show w]
        d (TName      cp w)      = pretty "TName"    cp [show w]
        d (TShort     cp a b)    = pretty "TShort"   cp [show a, show b]
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

textToken :: String -> Token
textToken = TText B.codePtZero TextRaw

nameToken :: String -> Token
nameToken = TName B.codePtZero . BlankNormal

instance B.CodePtr Token where
    codePtList (TText    cp _ _)    = [cp]
    codePtList (TName    cp _)      = [cp]
    codePtList (TShort   cp _ _)    = [cp]
    codePtList (TTerm    cp _ _)    = [cp]
    codePtList (TLocal   cp _ _ _)  = [cp]
    codePtList (TSlot    cp _ _)    = [cp]
    codePtList (TOpen    cp _)      = [cp]
    codePtList (TClose   cp _)      = [cp]
    codePtList (TSpace   cp _)      = [cp]
    codePtList (TComment cp _)      = [cp]


-- ----------------------  Local

data Local a
    = LocalSymbol a
    | LocalNest a
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

unlocal :: Local a -> a
unlocal (LocalNest   a) = a
unlocal (LocalSymbol a) = a


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


-- ----------------------  Term type

data TermType
    = TermTypePath               -- ^ Normal term path
    | TermTypeQuoted             -- ^ Quoted term name
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

pattern TTermPath cp ws       = TTerm cp TermTypePath   ws
pattern TTermQ    cp ws       = TTerm cp TermTypeQuoted ws


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

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath    = [TermName]

showTermName :: B.Map String
showTermName n = ('/' : n)

showNestedTermName :: [String] -> String
showNestedTermName = concat . map showTermName

termP :: Int -> Bool
termP = (>= 0)

termN :: Int -> Bool
termN = (< 0)

termPN :: Int -> Int -> Bool
termPN p n = termP p && termN n

termsP :: [Int] -> Bool
termsP = all termP

termsN :: [Int] -> Bool
termsN = all termN

termsPN :: [Int] -> [Int] -> Bool
termsPN p n = termsP p && termsN n


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
      TText     _ _ s    -> s
      TName     _ op     -> B.name op
      TShort    _ a b    -> a ++ "." ++ b
      TTerm     _ _ ns   -> concatMap ('/' :) ns
      TLocal    _ n _ _  -> unlocal n
      TSlot     _ _ s    -> s
      TOpen     _ s      -> s
      TClose    _ s      -> s
      TSpace    _ n      -> replicate n ' '
      TComment  _ s      -> s

-- | Text of token type, e.g., @\"text\"@, @\"open\"@.
tokenTypeText :: Token -> String
tokenTypeText tok =
    case tok of
      TText     _ _ _    -> "text"
      TName     _ _      -> "name"
      TShort    _ _ _    -> "short"
      TTerm     _ _ _    -> "term"
      TLocal    _ _ _ _  -> "local"
      TSlot     _ _ _    -> "slot"
      TOpen     _ _      -> "open"
      TClose    _ _      -> "close"
      TSpace    _ _      -> "space"
      TComment  _ _      -> "comment"

tokenSubtypeText :: Token -> Maybe String
tokenSubtypeText tok =
    case tok of
      TText     _ f _    -> Just $ textFormTypeText f
      TName     _ b      -> Just $ blankNameTypeText b
      TShort    _ _ _    -> Nothing
      TTerm     _ _ _    -> Nothing
      TLocal    _ _ _ _  -> Nothing
      TSlot     _ n _    -> Just $ slotTypeText n
      TOpen     _ _      -> Nothing
      TClose    _ _      -> Nothing
      TSpace    _ _      -> Nothing
      TComment  _ _      -> Nothing

slotTypeText :: Int -> String
slotTypeText 0   = "positional"
slotTypeText 1   = "named"
slotTypeText 2   = "global"
slotTypeText _   = "unknown"

tokenParents :: Token -> [Token]
tokenParents (TLocal _ _ _ ps) = ps
tokenParents _                 = []


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


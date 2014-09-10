{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode.

module Koshucode.Baala.Base.Token.Token
(
  -- * Token type
  Token (..),
  textToken,
  nameToken,

  -- * Blank name
  BlankName (..),

  -- * Term name
  TermName, TermName2, TermName3, TermName4,
  Terminal, TermPath,

  -- * Selectors
  tokenContent,
  tokenTypeText,

  -- * Predicates
  isBlankToken, isShortToken, isTermToken,
  isOpenTokenOf, isCloseTokenOf,

  -- * Other function
  sweepToken,
) where

import qualified Data.Generics                  as G
import qualified Koshucode.Baala.Base.Prelude   as B
import qualified Koshucode.Baala.Base.Text      as B


-- ----------------------  Token type

-- | There are ten types of tokens.
data Token
    = TText    B.CodePt Int String     -- ^ Text.
                                          --   'Int' represents quotation level, i.e.,
                                          --   0 for non-quoted,
                                          --   1 for single-quoted,
                                          --   2 for double-quoted,
                                          --   3 for @-with@ variable.
    | TName    B.CodePt BlankName      -- ^ Blank name.
    | TSlot    B.CodePt Int String     -- ^ Slot name.
                                          --   'Int' represents slot level, i.e.,
                                          --   0 for positional slots,
                                          --   1 for named slots,
                                          --   2 for global slots.
    | TShort   B.CodePt String String  -- ^ Abbreviated text.
    | TTerm    B.CodePt Int TermPath   -- ^ Term name.
    | TOpen    B.CodePt String         -- ^ Opening bracket.
    | TClose   B.CodePt String         -- ^ Closing bracket.
    | TSpace   B.CodePt Int            -- ^ /N/ space characters.
    | TComment B.CodePt String         -- ^ Comment text.
    | TUnknown B.CodePt String         -- ^ Unknown text.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name Token where
    name (TTerm  _ _ ns) = concat ns
    name (TSlot   _ _ s) = s
    name (TText   _ _ s) = s
    name (TOpen     _ s) = s
    name (TClose    _ s) = s
    name (TComment  _ s) = s
    name x = error $ "unknown name: " ++ show x

instance B.Write Token where
    write sh = d where
        d (TText    pt q w)  =  pretty "TText"    pt [show q, show w]
        d (TName    pt w)    =  pretty "TName"    pt [show w]
        d (TShort   pt a b)  =  pretty "TShort"   pt [show a, show b]
        d (TTerm    pt q ns) =  pretty "TTerm"    pt [show q, show ns]
        d (TSlot    pt n w)  =  pretty "TSlot"    pt [show n, show w]
        d (TOpen    pt p)    =  pretty "TOpen"    pt [show p]
        d (TClose   pt p)    =  pretty "TClose"   pt [show p]
        d (TSpace   pt c)    =  pretty "TSpace"   pt [show c]
        d (TComment pt s)    =  pretty "TComment" pt [show s]
        d (TUnknown pt s)    =  pretty "TUnknown" pt [show s]
        pretty k pt xs       =  B.writeH sh $ lineCol pt : k : xs
        lineCol pt           =  (show $ B.codeLineNumber pt)
                                ++ "." ++ (show $ B.codeColumnNumber pt)

textToken :: String -> Token
textToken = TText B.codeZero 0

nameToken :: String -> Token
nameToken = TName B.codeZero . BlankNormal

instance B.CodePtr Token where
    codePts (TText    cp _ _)  =  [cp]
    codePts (TName    cp _)    =  [cp]
    codePts (TShort   cp _ _)  =  [cp]
    codePts (TTerm    cp _ _)  =  [cp]
    codePts (TSlot    cp _ _)  =  [cp]
    codePts (TOpen    cp _)    =  [cp]
    codePts (TClose   cp _)    =  [cp]
    codePts (TSpace   cp _)    =  [cp]
    codePts (TComment cp _)    =  [cp]
    codePts (TUnknown cp _)    =  [cp]


-- ----------------------  Blank name

data BlankName
    = BlankNormal   String
    | BlankInternal String
    | BlankPrefix   String
    | BlankInfix    String
    | BlankPostfix  String
    deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance B.Name BlankName where
    name (BlankNormal   n)  =  n
    name (BlankInternal n)  =  n
    name (BlankPrefix   n)  =  n
    name (BlankInfix    n)  =  n
    name (BlankPostfix  n)  =  n

instance B.Write BlankName where
    write sh (BlankNormal   n)  =  B.write sh n
    write sh (BlankInternal n)  =  B.write sh n
    write sh (BlankPrefix   n)  =  B.write sh n B.<+> B.doc "(prefix)"
    write sh (BlankInfix    n)  =  B.write sh n B.<+> B.doc "(infix)"
    write sh (BlankPostfix  n)  =  B.write sh n B.<+> B.doc "(postfix)"


-- ---------------------- Term name

-- | Name of term, e.g., @\"file\"@ for the term name @\/file@.
type TermName    =  String
type TermName2   =  (String, String)
type TermName3   =  (String, String, String)
type TermName4   =  (String, String, String, String)

-- | Pair of term name and something.
type Terminal a  =  (TermName, a)

-- | Path of term names, e.g., term name @\/r\/x@
--   is correspond to path @[\"r\", \"x\"]@.
type TermPath    =  [TermName]


-- ---------------------- Selector

-- | Get the content of token.
--
--   >>> let tok = TTerm B.codeZero ["r", "x"] in tokenContent tok
--   "/r/x"
tokenContent :: Token -> String
tokenContent (TText  _ _ s)   =  s
tokenContent (TName    _ op)  =  B.name op
tokenContent (TShort _ a b)   =  a ++ "." ++ b
tokenContent (TTerm  _ _ ns)  =  concatMap ('/' :) ns
tokenContent (TSlot  _ _ s)   =  s
tokenContent (TOpen    _ s)   =  s
tokenContent (TClose   _ s)   =  s
tokenContent (TSpace   _ n)   =  replicate n ' '
tokenContent (TComment _ s)   =  s
tokenContent (TUnknown _ s)   =  s

-- | Text of token type, i.e., one of
--   @\"Text\"@, @\"Term\"@, @\"Open\"@, @\"Close\"@,
--   @\"Space\"@, @\"Comment\"@, or @\"Unknown\"@.
--
--   >>> tokenTypeText $ textToken "flower"
--   "Text"
tokenTypeText :: Token -> String
tokenTypeText (TText  _ _ _)  =  "text"
tokenTypeText (TName    _ _)  =  "name"
tokenTypeText (TShort _ _ _)  =  "short"
tokenTypeText (TTerm  _ _ _)  =  "term"
tokenTypeText (TSlot  _ _ _)  =  "slot"
tokenTypeText (TOpen    _ _)  =  "open"
tokenTypeText (TClose   _ _)  =  "close"
tokenTypeText (TSpace   _ _)  =  "space"
tokenTypeText (TComment _ _)  =  "comment"
tokenTypeText (TUnknown _ _)  =  "unknown"


-- ----------------------  Predicate

-- | Test the token is blank,
--   i.e., 'TComment' or 'TSpace'.
isBlankToken :: B.Pred Token
isBlankToken (TSpace _ _)    = True
isBlankToken (TComment _ _)  = True
isBlankToken _               = False

-- | Remove blank tokens.
sweepToken :: B.Map [Token]
sweepToken = B.omit isBlankToken

isShortToken :: B.Pred Token
isShortToken (TShort _ _ _)  = True
isShortToken _               = False

isTermToken :: B.Pred Token
isTermToken (TTerm _ _ _) = True
isTermToken _             = False

-- | Check token is a 'TOpen' of the specific bracket.
--
--   >>> let tok = TOpen 0 "(" in isOpenTokenOf "(" tok
--   True
--
--   >>> let tok = TOpen 0 "{" in isOpenTokenOf "(" tok
--   False
isOpenTokenOf :: String -> B.Pred Token
isOpenTokenOf p1 (TOpen _ p2)   = p1 == p2
isOpenTokenOf _ _               = False

-- | Check token is a 'TClose' of the specific bracket.
isCloseTokenOf :: String -> B.Pred Token
isCloseTokenOf p1 (TClose _ p2) = p1 == p2
isCloseTokenOf _ _              = False


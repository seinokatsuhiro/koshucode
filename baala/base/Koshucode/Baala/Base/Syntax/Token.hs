{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tokens in Koshucode

module Koshucode.Baala.Base.Syntax.Token
( Token (..),
  isTerm, isBlank,
  sweep, sweepLeft,
  tokens, untokens
) where
import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude
import qualified Data.Char as C

data Token
    = Word Int String   -- ^ Text
    | TermN   [String]  -- ^ Term name
    | TermP   [Int]     -- ^ Term position
    | Open    String    -- ^ Open paren, like @(@
    | Close   String    -- ^ Close paren, like @)@
    | Space   Int       -- ^ N space chars
    | Newline           -- ^ Newline char
    | Comment String    -- ^ Comment text, like @% ...@
      deriving (Show, Eq, Ord, Data, Typeable)

instance Name Token where
    name (TermN  ns) = concat ns
    name (Word  _ s) = s
    name (Open    s) = s
    name (Close   s) = s
    name (Comment s) = s
    name x = error $ "unknown name: " ++ show x

-- | Test the token is a term,
--   i.e., 'TermN' or 'TermP'
isTerm :: Token -> Bool
isTerm (TermN _)    = True
isTerm (TermP _)    = True
isTerm _            = False

-- | Test the token is blank.
--   i.e., 'Space', 'Newline', or 'Comment'.
isBlank :: Token -> Bool
isBlank (Space _)   = True
isBlank (Newline)   = True
isBlank (Comment _) = True
isBlank _           = False

-- | Remove blank tokens.
sweep :: [Token] -> [Token]
sweep = filter (not . isBlank)

-- | Skip blank tokens.
sweepLeft :: [Token] -> [Token]
sweepLeft [] = []
sweepLeft xxs@(x:xs) | isBlank x = sweepLeft xs
                     | otherwise = xxs



-- ----------------------  Untokenizer

-- | Convert back a token list to a source string
untokens :: [Token] -> String
untokens [] = []
untokens (Open a : xs) = a ++ untokens xs
untokens (x : Close a : xs) = untoken x ++ untokens (Close a : xs)
untokens [x] = untoken x
untokens (x:xs) = untoken x ++ " " ++ untokens xs

untoken :: Token -> String
untoken (Word 1 s)  = "'" ++ s ++ "'"
untoken (Word 2 s)  = "\"" ++ s ++ "\""
untoken (Word _ s)  = s
untoken (TermN  ss) = concat ss
untoken (TermP  ns) = concatMap show ns
untoken (Open   s)  = s
untoken (Close  s)  = s
untoken (Space  n)  = replicate n ' '
untoken (Newline)   = "\n"
untoken (Comment s) = "** " ++ s ++ "\n"



-- ----------------------  Tokenizer

-- | Tokenizer: split a string to a list of tokens
tokens :: String -> [Token]
tokens [] = []
tokens s = let (t, s2) = token s
           in  t : tokens s2

token :: String -> (Token, String)
token ccs =
    case ccs of
      ('*' : '*' : _)  -> comment ccs []
      ('(' : '*' : _)  -> comment ccs [] -- todo (* commented *)
      ('(' : ')' : cs) -> (Word 0 "()", cs) -- nil
      (c:cs)
        | isOpen    c  -> (Open   [c] , cs)
        | isClose   c  -> (Close  [c] , cs)
        | isSingle  c  -> (Word 0 [c] , cs)
        | isNewline c  -> (Newline    , cs)
        | c == '/'     -> term cs [c]  []
        | isQuote c    -> quote   c cs []
        | isWord c     -> word    ccs  []
        | isSpace c    -> white 1 cs
      _                -> error $ "unknown token type: " ++ ccs
    where
      -- Collect chars
      tk cs k xs = (k $ reverse xs, cs)

      word (c:cs) xs    | isWord c    = word cs (c:xs)
      word ccs2 xs                    = tk ccs2 (Word 0) xs

      quote _ [] xs                   = tk [] (Word 1) xs
      quote q (c:cs) xs | c == q      = tk cs (Word 1) xs
                        | isNewline c = error "newline in quote"
                        | otherwise   = quote q cs (c:xs)

      term (c:cs) xs ns | c == '/'    = term cs [c] $ t xs ns
                        | isWord c    = term cs (c:xs) ns
      term ccs2 xs ns                 = tk ccs2 TermN $ t xs ns
      t xs = (reverse xs :)

      white n (c:cs)    | isSpace c   = white (n+1) cs
      white n ccs2                    = (Space n, ccs2)

      comment (c:cs) xs | isMidline c = comment cs (c:xs)
      comment ccs2 xs                 = tk ccs2 Comment xs

-- e1 = tokens "aa bb"
-- e2 = tokens "'aa' '' \"cc\""
-- e3 = tokens "aa (bb (cc))"
-- e4 = tokens "/aa 00 /bb 11"
-- e5 = tokens "count /r/x/t"
-- e6 = tokens "///x /r/"



-- ----------------------  Char category

isOpen, isClose, isSingle, isQuote, isNewline :: Char -> Bool
isMidline, isSpace, isWord, isNonWord, maybeWord :: Char -> Bool

isOpen    c = c `elem` "([{"
isClose   c = c `elem` "}])"
isSingle  c = c `elem` ":="
isQuote   c = c `elem` "'\""
isNewline c = c == '\n'
isMidline   = not . isNewline
isSpace   c = C.isSpace c && not (isNewline c)
isWord    c = maybeWord c && not (isNonWord c)
isNonWord c = isOpen c || isClose c || isSingle c
maybeWord c = C.isAlphaNum c
              || C.isMark c
              || C.isSymbol c
              || C.isPunctuation c


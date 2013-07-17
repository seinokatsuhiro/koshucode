{-# OPTIONS_GHC -Wall #-}

{-| Term values. -}

module Koshucode.Baala.Vanilla.Value.Val
( Val (..)
, stringv, intv, listv, relv, nov
, binv, toInt, toString
, valRangeMinMax
, module Koshucode.Baala.Base.Data
) where

import qualified Data.List as L

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Content

{-| Type for values. -}

data Val
    = Stringv  String      -- ^ String value
    | Intv     Int         -- ^ Integer value
    | Boolv    Bool        -- ^ Boolean value
    | Listv    [Val]       -- ^ List of values
    | Setv     [Val]       -- ^ Set of values
    | Termsetv [Named Val] -- ^ List of values
    | Relv     (Rel Val)   -- ^ Relational value
    | Nov                  -- ^ Sign of no ordinary values
      deriving (Show, Eq, Ord)

instance BoolValue Val where
    boolValue = Boolv
    isBoolValue (Boolv _) = True
    isBoolValue _         = False

instance PrimitiveContent Val where

instance IntValue Val where
    intValue = Intv
    isIntValue (Intv _) = True
    isIntValue _        = False

instance StringValue Val where
    stringValue = Stringv
    isStringValue (Stringv _) = True
    isStringValue _           = False
    theStringValue (Stringv s) = s
    theStringValue _           = ""

instance ListValue Val where
    listValue = Listv
    isListValue (Listv _)   = True
    isListValue _           = False
    theListValue (Listv xs) = xs
    theListValue _          = []

instance SetValue Val where
    setValue = Setv . unique
    isSetValue (Setv _)   = True
    isSetValue _          = False

instance TermsetValue Val where
    termsetValue = Termsetv
    isTermsetValue (Termsetv _) = True
    isTermsetValue _            = False

instance RelValue Val where
    relValue = Relv
    theRelValue (Relv r) = r
    theRelValue _        = undefined
    isRelValue  (Relv _) = True
    isRelValue  _        = False

instance Nil Val where
    nil = Nov
    isNil Nov = True
    isNil _   = False

instance Pretty Val where
    doc (Stringv s)   = text $ escape s
    doc (Intv n)      = text "int" <+> int n
    doc (Boolv b)
        | b           = text "#true"
        | otherwise   = text "#false"
    doc (Nov)         = text "()"
    doc (Listv xs)    = text "[" <+> hsep (map doc xs) <+> text "]"
    doc (Setv xs)     = text "{" <+> hsep (map doc xs) <+> text "}"
    doc (Termsetv xs) = text "{|" <+> hsep (map docTerms xs) <+> text "|}"
    doc (Relv r)      = doc r

docTerms :: (Pretty a) => Named a -> Doc
docTerms (n, x) = text n <+> doc x

instance Value Val where
    appendContent (Nov) x = x
    appendContent x (Nov) = x
    appendContent (Stringv s1) (Stringv s2) = Stringv $ s1 ++ s2
    appendContent _ _ = nil

stringv :: String -> Val
stringv = Stringv

intv    :: Int -> Val
intv    = Intv

listv   :: [Val] -> Val
listv   = Listv

relv    :: Rel Val -> Val
relv    = Relv

nov     :: Val
nov     = Nov

escape :: String -> String
escape = join . hashSplit where
    join :: [String] -> String
    join [] = ""
    join [x] = x
    join xs = L.intercalate " " $ "text" : xs



-- ----------------------  

binv :: (Int -> Int -> Int) -> Val -> Map Val
binv op (Intv x) (Intv y) = Intv $ op x y
binv _ _ _ = Nov

valRangeMinMax :: Val -> Val -> [Val]
valRangeMinMax (Intv a) (Intv b) = map Intv [a .. b]
valRangeMinMax _ _ = undefined

toInt :: Val -> Int
toInt (Stringv x) = read x
toInt (Intv    x) = x
toInt x = error $ "not integer: " ++ show x

toString :: Val -> String
toString (Stringv x) = x
toString (Intv    x) = show x
toString _ = undefined


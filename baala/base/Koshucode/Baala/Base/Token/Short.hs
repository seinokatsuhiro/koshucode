{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Token.Short
( ShortDef,
  Short (..),
  shortMap,
  shortMapM,
  shortM,
  shortTrim,

  ShortDoc (..),
  shortText,
  shortDocH,
  shortDocColon,
) where

import qualified Data.List                    as L
import qualified Text.PrettyPrint             as D
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B

type ShortDef = B.Named String

data Short a =
    Short { shortHead :: [ShortDef]
          , shortBody :: a }
    deriving (Show, Ord, Eq)

instance Functor Short where
    fmap f (Short a b) = Short a $ f b

shortMap :: (Functor f) => (a -> b) -> [f a] -> [f b]
shortMap = map . fmap

shortM :: (Monad m) => Short (m a) -> m (Short a)
shortM (Short he bo) = return . Short he =<< bo

shortMapM :: (Monad m) => (a -> m b) -> [Short a] -> m [Short b]
shortMapM f = mapM $ shortM . fmap f

shortTrim :: B.Map [Short [a]]
shortTrim = filter $ not . null . shortBody


-- ----------------------  ShortDoc

class (B.Pretty a) => ShortDoc a where
    shortDoc :: [ShortDef] -> a -> B.Doc

instance ShortDoc Int where
    shortDoc _ = D.int

instance ShortDoc String where
    shortDoc _ = D.text

instance ShortDoc Bool where
    shortDoc _ True  = D.text "#true"
    shortDoc _ False = D.text "#false"

instance (ShortDoc a) => ShortDoc (B.Named a) where
    shortDoc sh (n, x) = D.text n D.<+> shortDoc sh x

shortText :: [ShortDef] -> B.Map String
shortText = loop where
    loop [] s = '\'' : s
    loop ((prefix, long) : sh) s =
        case L.stripPrefix long s of
          Just s2 | s2 /= "" -> prefix ++ "." ++ s2
          _ -> loop sh s

shortDocColon :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocColon sh = D.hsep . shortDocColons sh

shortDocColons :: (ShortDoc a) => [ShortDef] -> [a] -> [D.Doc]
shortDocColons sh = L.intersperse (D.text ":") . map (shortDoc sh)

shortDocH :: (ShortDoc a) => [ShortDef] -> [a] -> D.Doc
shortDocH sh = D.hsep . map (shortDoc sh)


{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Write
( -- * Data type
  Write (..),
  StringMap,

  -- * Derivative
  writeH, writeV,
  writeColon,

  -- * Simple writer
  doc, doch, docv,
  docWrap, docWraps,
) where

import qualified Data.List                     as L
import qualified Text.PrettyPrint              as D
import qualified Koshucode.Baala.Base.Prelude  as B



-- ----------------------  Data type

class Write a where
    write :: StringMap -> a -> B.Doc

type StringMap = B.Map String

instance Write B.Doc where
    write _ x = x

instance Write Int where
    write _ = D.int

instance Write String where
    write _ = D.text

instance Write Bool where
    write _ True  = D.text "<1>"
    write _ False = D.text "<0>"

instance (Write a) => Write (B.Named a) where
    write sh (n, x) = D.text n D.<+> write sh x



-- ----------------------  Derivative

writeH :: (Write a) => StringMap -> [a] -> D.Doc
writeH sh = D.hsep . map (write sh)

writeV :: (Write a) => StringMap -> [a] -> D.Doc
writeV sh = D.vcat . map (write sh)

-- | Colon-seperated document.
--
--   >>> docBracket $ docColon [True, False]
--   [ #true : #false ]
writeColon :: (Write a) => StringMap -> [a] -> D.Doc
writeColon sh = D.hsep . writeColons sh

-- | Colon-seperated list.
writeColons :: (Write a) => StringMap -> [a] -> [D.Doc]
writeColons sh = L.intersperse (D.text ":") . map (write sh)



-- ----------------------  Simple writer

doc :: (Write a) => a -> D.Doc
doc = write id

docv :: (Write a) => [a] -> D.Doc
docv = writeV id

doch :: (Write a) => [a] -> D.Doc
doch = writeH id

-- | Wrap in open and close brackets.
--   Put spaces between content and brackets.
--
--   >>> docWraps "(" ")" "abc"
--   ( abc )
docWraps :: (Write a) => String -> String -> a -> D.Doc
docWraps open close a = D.text open D.<+> doc a D.<+> D.text close

-- | Wrap in open and close brackets.
--
--   >>> docWrap "(" ")" "abc"
--   (abc)
docWrap :: (Write a) => String -> String -> a -> D.Doc
docWrap open close a = D.text open D.<> doc a D.<> D.text close


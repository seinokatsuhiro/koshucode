{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Text.Write
  ( -- * Class
    StringMap,
    Write (..),

    -- * Derivative
    writeH, writeV,
    writeColon, writeBar, writeSep,
    writeSepsWith,
    writeTerm, writeTerms,
    -- $Derivative

    -- * Simple writer
    doc, doch, docv,
    docWrap, docWraps,
    doc02, docConcat,
    -- $Simple
  ) where

import qualified Data.List                     as L
import qualified Text.PrettyPrint              as D
import qualified Koshucode.Baala.Base.Prelude  as B



-- ----------------------  Data type

-- | Mapping from string to string.
type StringMap = B.Map String

-- | Writer with string mapping.
class Write a where
    write :: StringMap -> a -> B.Doc

instance Write B.Doc where
    write _ x = x

instance Write Int where
    write _ = D.int

instance Write Integer where
    write _ = D.integer

instance Write String where
    write _ = D.text

instance Write Bool where
    write _ True  = D.text "<1>"
    write _ False = D.text "<0>"

instance (Write a) => Write (B.Named a) where
    write sh = writeTerm $ write sh



-- ----------------------  Derivative

-- $Derivative
--
--   /Examples/
--
--   Colon-seperated document.
--
--     >>> writeColon id [True, False]
--     <1> : <0>
--
--   List of terms.
--
--     >>> writeTerms doc [("x", False), ("y", True)]
--     /x <0> /y <1>

writeH :: (Write a) => StringMap -> [a] -> B.Doc
writeH sh = D.hsep . map (write sh)

writeV :: (Write a) => StringMap -> [a] -> B.Doc
writeV sh = D.vcat . map (write sh)

writeColon :: (Write a) => StringMap -> [a] -> B.Doc
writeColon sh = writeSep ":" sh

writeBar :: (Write a) => StringMap -> [a] -> B.Doc
writeBar sh = writeSep "|" sh

writeSep :: (Write a) => String -> StringMap -> [a] -> B.Doc
writeSep sep sh = D.hsep . writeSeps sep sh

writeSeps :: (Write a) => String -> StringMap -> [a] -> [B.Doc]
writeSeps sep sh = writeSepsWith (write sh) sep

writeSepsWith :: (Write a) => (a -> B.Doc) -> String -> [a] -> [B.Doc]
writeSepsWith w sep = L.intersperse (D.text sep) . map w

writeTerm :: (a -> B.Doc) -> B.Named a -> B.Doc
writeTerm w (n, x) = D.text ('/' : n) B.<+> w x

writeTerms :: (a -> B.Doc) -> [B.Named a] -> B.Doc
writeTerms w = doch . (writeTerm w `map`)



-- ----------------------  Simple writer

-- $Simple
--
--  Simple writers are writers with identity string mapping.
--  For example, @doc@ is equivalent for @write id@.
--
--  /Examples/
--
--  Wrap in open and close brackets.
--
--     >>> docWrap "[" "]" "abc"
--     [abc]
--
--  Put spaces between content and brackets.
--
--     >>> docWraps "[" "]" "abc"
--     [ abc ]

doc :: (Write a) => a -> B.Doc
doc = write id

docv :: (Write a) => [a] -> B.Doc
docv = writeV id

doch :: (Write a) => [a] -> B.Doc
doch = writeH id

docWrap :: (Write a) => String -> String -> a -> B.Doc
docWrap = docWrapBody (B.<>)

docWraps :: (Write a) => String -> String -> a -> B.Doc
docWraps = docWrapBody (B.<+>)

docWrapBody :: (Write a) => (B.Doc -> B.Doc -> B.Doc) -> String -> String -> a -> B.Doc
docWrapBody p open close a = D.text open `p` doc a `p` D.text close

doc02 :: Int -> B.Doc
doc02 n | n < 10    = doc $ '0' : show n
        | otherwise = doc n

docConcat :: String -> B.Bin B.Doc
docConcat sep x y = x B.<> doc sep B.<> y


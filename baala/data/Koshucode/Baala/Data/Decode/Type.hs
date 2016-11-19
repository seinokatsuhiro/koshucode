{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode specific content.

module Koshucode.Baala.Data.Decode.Type
  ( treesTexts, treeText,
    treesInterp,
    treesType,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Type             as D
import qualified Koshucode.Baala.Data.Decode.Term      as D
import qualified Koshucode.Baala.Data.Decode.Message   as Msg

import Koshucode.Baala.Syntax.TTree.Pattern


-- ----------------------  Text

-- | Get single text from token trees.
--
--   >>> S.tt "'aa 'bb" >>= treesTexts True
--   Right ["aa","bb"]
--
--   >>> S.tt "\"aa\" \"bb\"" >>= treesTexts True
--   Right ["aabb"]
--
treesTexts :: Bool -> [S.TTree] -> B.Ab [String]
treesTexts q = mapM $ treeText q

-- | Get text from token tree.
--
--   >>> S.tt1 "aa" >>= treeText False
--   Right "aa"
--
treeText :: Bool -> S.TTree -> B.Ab String
treeText q (L tok) = tokenString q tok
treeText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenString :: Bool -> S.Token -> B.Ab String
tokenString True  (S.TText _ q w) | q > S.TextRaw  = Right w
tokenString False (S.TTextRaw _ w)                 = Right w
tokenString _ _  =  Msg.nothing


-- ----------------------  Interp

-- | Get interpretation from token trees.
--
--   >>> S.tt "term /a" >>= treesInterp
--   Right (Interp { interpWords = [InterpText "term", InterpTerm "a"],
--                   interpTerms = ["a"] })
--
treesInterp :: [S.TTree] -> B.Ab D.Interp
treesInterp = Right . D.interp B.<=< mapM treeInterpWord

treeInterpWord :: S.TTree -> B.Ab D.InterpWord
treeInterpWord (B.TreeB _ _ _) = Msg.nothing
treeInterpWord (B.TreeL x) =
    case x of
      S.TText _ _ w    -> Right $ D.InterpText w
      S.TTermN _ _ n   -> Right $ D.InterpTerm $ S.toTermName n
      S.TTerm _ _ [n]  -> Right $ D.InterpTerm n
      _                -> Msg.nothing


-- ----------------------  Type

-- | Decode type content.
treesType :: [S.TTree] -> B.Ab D.Type
treesType = gen where
    gen xs = case S.divideTreesByBar xs of
               [x] ->  single x
               xs2 ->  Right . D.TypeSum =<< mapM gen xs2

    single [B _ xs]          = gen xs
    single (LText f n : xs)
        | f == S.TextRaw     = dispatch n xs
        | otherwise          = Msg.quoteType n
    single []                = Right $ D.TypeSum []
    single _                 = Msg.unkType ""

    precision ws [LRaw w] | w `elem` ws = Right $ Just w
    precision _ []  = Right Nothing
    precision _ _   = Msg.unkType "precision"

    clock = ["sec", "min", "hour", "day"]
    time  = "month" : clock

    dispatch "any"     _    = Right D.TypeAny
    dispatch "empty"   _    = Right D.TypeEmpty
    dispatch "boolean" _    = Right D.TypeBool
    dispatch "text"    _    = Right D.TypeText
    dispatch "code"    _    = Right D.TypeCode
    dispatch "decimal" _    = Right D.TypeDec
    dispatch "clock"   xs   = Right . D.TypeClock  =<< precision clock xs
    dispatch "time"    xs   = Right . D.TypeTime   =<< precision time  xs
    dispatch "binary"  _    = Right D.TypeBin
    dispatch "term"    _    = Right D.TypeTerm
    dispatch "type"    _    = Right D.TypeType
    dispatch "interp"  _    = Right D.TypeInterp

    dispatch "tag"   xs     = case xs of
                                [tag, colon, typ]
                                    | treeText False colon == Right ":"
                                      -> do tag' <- treeText False tag
                                            typ' <- gen [typ]
                                            Right $ D.TypeTag tag' typ'
                                _   -> Msg.unkType "tag"
    dispatch "list"  xs     = Right . D.TypeList =<< gen xs
    dispatch "set"   xs     = Right . D.TypeSet  =<< gen xs
    dispatch "tuple" xs     = do ts <- mapM (gen. B.li1) xs
                                 Right $ D.TypeTuple ts
    dispatch "tie"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ D.TypeTie ts2
    dispatch "rel"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd $ B.mapSndTo gen ts1
                                 Right $ D.TypeRel ts2
    dispatch n _            = Msg.unkType n

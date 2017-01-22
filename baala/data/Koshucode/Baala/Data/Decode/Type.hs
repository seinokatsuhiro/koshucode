{-# OPTIONS_GHC -Wall #-}

-- | Decode specific-type contents.

module Koshucode.Baala.Data.Decode.Type
  ( treesTexts,
    treesInterp,
    treesType,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type                  as T
import qualified Koshucode.Baala.Data.Decode.Term      as D
import qualified Koshucode.Baala.Syntax.Pattern        as P
import qualified Koshucode.Baala.Data.Decode.Message   as Msg


-- ----------------------  Text

-- | Get single text from token trees.
--
--   >>> S.withTrees (treesTexts True :: [S.Tree] -> B.Ab [String]) "'aa 'bb"
--   Right ["aa","bb"]
--
treesTexts :: Bool -> [S.TTree t] -> B.Ab [t]
treesTexts = mapM . treeText

-- | Get text from token tree.
--
--   >>> S.toTree "aa" >>= treeText False
--   Right "aa"
--
treeText :: Bool -> S.TTree t -> B.Ab t
treeText q (P.L tok) = tokenString q tok
treeText _ _ = Msg.nothing

-- | Get quoted/unquoted text.
tokenString :: Bool -> S.TToken t -> B.Ab t
tokenString True  (P.T q w) | q > S.TextRaw  = Right w
tokenString False (P.TRaw w)                 = Right w
tokenString _ _  = Msg.nothing


-- ----------------------  Interpretation

-- | Decode interpretation from token trees.
--
--   >>> S.toTrees "term /a" >>= treesInterp
--   Right (Interp { interpWords = [InterpText "term", InterpTerm "a"],
--                   interpTerms = ["a"] })
--
treesInterp :: (O.Textual t, S.ToTermName t) => [S.TTree t] -> B.Ab T.Interp
treesInterp = Right . T.interp O.#. mapM treeInterpWord

treeInterpWord :: (O.Textual t, S.ToTermName t) => S.TTree t -> B.Ab T.InterpWord
treeInterpWord (B.TreeB _ _ _) = Msg.nothing
treeInterpWord (B.TreeL x) =
    case x of
      S.TText _ _ w   -> Right $ T.InterpText $ O.tString w
      S.TTerm _ n     -> Right $ T.InterpTerm $ S.toTermName n
      _               -> Msg.nothing


-- ----------------------  Type

-- | Decode type content.
treesType :: [S.Tree] -> B.Ab T.Type
treesType = gen where
    gen xs = case S.divideTreesByBar xs of
               [x] ->  single x
               xs2 ->  Right . T.TypeSum O.# mapM gen xs2

    single [P.B _ xs]        = gen xs
    single (P.LText f n : xs)
        | f == S.TextRaw     = dispatch n xs
        | otherwise          = Msg.quoteType n
    single []                = Right $ T.TypeSum []
    single _                 = Msg.unkType ""

    precision ws [P.LRaw w] | w `elem` ws = Right $ Just w
    precision _ []  = Right Nothing
    precision _ _   = Msg.unkType "precision"

    clock = ["sec", "min", "hour", "day"]
    time  = "month" : clock

    dispatch "any"     _    = Right T.TypeAny
    dispatch "empty"   _    = Right T.TypeEmpty
    dispatch "boolean" _    = Right T.TypeBool
    dispatch "text"    _    = Right T.TypeText
    dispatch "code"    _    = Right T.TypeCode
    dispatch "decimal" _    = Right T.TypeDec
    dispatch "clock"   xs   = Right . T.TypeClock  O.# precision clock xs
    dispatch "time"    xs   = Right . T.TypeTime   O.# precision time  xs
    dispatch "binary"  _    = Right T.TypeBin
    dispatch "term"    _    = Right T.TypeTerm
    dispatch "type"    _    = Right T.TypeType
    dispatch "interp"  _    = Right T.TypeInterp

    dispatch "tag"   xs     = case xs of
                                [tag, colon, typ]
                                    | treeText False colon == Right ":"
                                      -> do tag' <- treeText False tag
                                            typ' <- gen [typ]
                                            Right $ T.TypeTag tag' typ'
                                _   -> Msg.unkType "tag"
    dispatch "list"  xs     = Right . T.TypeList O.# gen xs
    dispatch "set"   xs     = Right . T.TypeSet  O.# gen xs
    dispatch "tuple" xs     = do ts <- mapM (gen. B.list1) xs
                                 Right $ T.TypeTuple ts
    dispatch "tie"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd (gen O.<$$> ts1)
                                 Right $ T.TypeTie ts2
    dispatch "rel"   xs     = do ts1 <- D.treesTerms xs
                                 ts2 <- B.sequenceSnd (gen O.<$$> ts1)
                                 Right $ T.TypeRel ts2
    dispatch n _            = Msg.unkType n

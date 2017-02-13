{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode contents from token trees.

module Koshucode.Baala.Data.Decode.Content
  ( -- * Functions
    DecodeContent,
    CalcContent, 
    stringContent,
    charsContent,
    treeContent,
    treesJudge,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as T
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Decode.Numeric     as D
import qualified Koshucode.Baala.Data.Decode.Term        as D
import qualified Koshucode.Baala.Data.Decode.Type        as D
import qualified Koshucode.Baala.Syntax.Pattern          as P
import qualified Koshucode.Baala.Base.Message            as Msg
import qualified Koshucode.Baala.Data.Decode.Message     as Msg


-- ----------------------  General content

-- | Content decoder.
type DecodeContent t c = S.TTree t -> B.Ab c

-- | Content calculator.
type CalcContent t c = S.TTree t -> B.Ab c

-- | Decode content from string.
stringContent :: (D.CContent c) => String -> B.Ab c
stringContent = treeContent O.#. (S.toTree :: String -> B.Ab S.Tree)

{-| Decode content from chars. -}
charsContent :: (D.CContent c) => S.Chars -> B.Ab c
charsContent = treeContent O.#. (S.toTree :: S.Chars -> B.Ab S.Tree)

-- | Decode content from token tree.
treeContent :: forall t c. (S.TextualTermName t, D.CContent c) => DecodeContent t c
treeContent tree = Msg.abLiteral tree $ cons tree where
    cons :: DecodeContent t c
    cons x@(P.L tok)
        = (putDecimal O.# D.treesDigits [x]) O.<||>
          (D.putTime  O.# D.treesTime [x])   O.<||>
          (D.putClock O.# D.tokenClock tok)  O.<||>
          (tokenContent tok)
    cons (P.B b xs) = case b of
        S.BracketGroup   -> group xs
        S.BracketList    -> D.putList   O.# treesContents cons xs
        S.BracketSet     -> D.putSet    O.# treesContents cons xs
        S.BracketTie     ->                 treesTie      cons xs
        S.BracketRel     -> D.putRel    O.# treesRel      cons xs
        S.BracketType    -> D.putType   O.# D.treesType        xs
        S.BracketInterp  -> D.putInterp O.# D.treesInterp      xs
        _                -> Msg.unkBracket
    cons _ = B.bug "treeContent"

    group :: [S.TTree t] -> B.Ab c
    group xs@(P.LRaw _ : _)  = (putDecimal O.# D.treesDigits xs) O.<||>
                               (D.putTime  O.# D.treesTime xs)
    group []                 = Right D.empty
    group _                  = Msg.unkContent

    putDecimal = D.putDec O.#. T.decodeDecimal

tokenContent :: (S.TextualTermName t, D.CContent c) => S.TToken t -> B.Ab c
tokenContent (P.T f w)
    | f <= S.TextRaw     = keyword w
    | f == S.TextQ       = D.putCode w
    | f == S.TextTerm    = D.putTerm $ S.toTermName w
    | otherwise          = D.putText w
    where
      keyword "(+)"  = Right D.true
      keyword "(-)"  = Right D.false
      keyword "(/)"  = Right D.end
      keyword "dum"  = Right D.dum
      keyword "dee"  = Right D.dee
      keyword "0"    = Right D.false  -- obsolete
      keyword "1"    = Right D.true   -- obsolete
      keyword _      = Msg.unkContent
tokenContent _ = Msg.unkContent

-- List of contents
--
--   { 0 | 1 | 2 }
--   [ 0 | 1 | 2 ]
--     .........
--     :
--     treesContents
--
treesContents :: (O.Textual t, D.CContent c) => DecodeContent t c -> [S.TTree t] -> B.Ab [c]
treesContents _   [] = Right []
treesContents cons cs = lt `mapM` S.divideTreesByBar cs where
    lt []   = Right D.empty
    lt [x]  = cons x
    lt xs   = cons $ B.TreeB S.BracketGroup Nothing xs

-- Contents enclosed in angle brackets
--
--   {- /a 1  /b 2  /c 3 -}   << words "a b c" >>
--      ................         .............
--      :                        :
--      treesTie                 treesTie
--
treesTie :: (S.TextualTermName t, D.CContent c) => DecodeContent t c -> [S.TTree t] -> B.Ab c
treesTie cons xs@(P.LTermOrd _ _ : _) = D.putTie O.# treesTerms cons xs
treesTie _ [] = D.putTie []
treesTie _ [P.LRaw "words", P.LQq ws] = D.putList (D.pText <$> O.tWords ws)
treesTie _ _ = Msg.adlib "unknown tie"

-- Terms
--
--   /a 1  /b 2  /c 3
--   ................
--   :
--   treesTerms
--
treesTerms :: (S.ToTermName t, D.CContent c) => DecodeContent t c -> [S.TTree t] -> B.Ab [S.Term c]
treesTerms cons = mapM p O.#. D.treesTerms1 where
    p (name, tree) = Right . (name,) O.# cons tree

-- | Decode judge from token trees.
--   Judges itself are not content type.
--   It can be only used in the top-level of resources.
treesJudge ::
    (S.TextualTermName t, D.CContent c)
    => D.CacheT t        -- ^ Term name cache
    -> T.AssertType       -- ^ Assertion type
    -> S.JudgeClass       -- ^ Judgement class
    -> [S.TTree t]        -- ^ Trees of terms
    -> B.Ab (D.CacheT t, T.Judge c)   -- ^ Error or decoded judgement
treesJudge cc q cl trees =
    do (cc', ts) <- D.treesTermsCached cc trees
       terms <- mapM term ts
       Right (cc', T.assertAs q cl terms)
    where
      term (n, ts) = do c <- treeContent $ S.ttreeGroup ts
                        Right (n, c)


-- ----------------------  Relation
--
--        .............  treesRel  .............
--        :                                    :
--     {= /a /b /c  [ 0 | 1 | 2 ]  [ 3 | 4 | 5 ] =}
--        ........  .............  .............
--        :         :              :
--        :         treeTuple      treeTuple
--        treesTermNames
--
--
treesRel :: (S.TextualTermName t, D.CContent c) => DecodeContent t c -> [S.TTree t] -> B.Ab (T.Rel c)
treesRel cons xs =
    do bo <- treeTuple cons n `mapM` xs'
       Right $ T.Rel he $ B.unique bo
    where
      (ns, xs')  = treesTermNames xs
      n          = length ns
      he         = T.headFrom ns

-- | Split term names.
treesTermNames :: (S.ToTermName t) => [S.TTree t] -> ([S.TermName], [S.TTree t])
treesTermNames = terms [] where
    terms ns (P.LTermOrd ord n : xs) = terms (S.toTermNameOrd ord n : ns) xs
    terms ns xs = (reverse ns, xs)

-- | Decode specific number of contents.
treeTuple :: (O.Textual t, D.CContent c) => DecodeContent t c -> Int -> S.TTree t -> B.Ab [c]
treeTuple cons n g@(P.B S.BracketList xs) =
    do cs <- treesContents cons xs
       let n' = length cs
       B.when (n /= n') $ Msg.abLiteral g $ Msg.oddRelation n n'
       Right cs
treeTuple _ _ g = Msg.abLiteral g $ Msg.reqRelTuple


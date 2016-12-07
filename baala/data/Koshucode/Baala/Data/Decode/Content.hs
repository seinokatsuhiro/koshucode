{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decode contents from token trees.

module Koshucode.Baala.Data.Decode.Content
  ( -- * Functions
    DecodeContent,
    CalcContent, 
    stringContent,
    treeContent,
    treesJudge,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data.Type               as D
import qualified Koshucode.Baala.Data.Class              as D
import qualified Koshucode.Baala.Data.Decode.Numeric     as D
import qualified Koshucode.Baala.Data.Decode.Term        as D
import qualified Koshucode.Baala.Data.Decode.Type        as D
import qualified Koshucode.Baala.Syntax.Pattern          as P
import qualified Koshucode.Baala.Base.Message            as Msg
import qualified Koshucode.Baala.Data.Decode.Message     as Msg


-- ----------------------  General content

pattern Text f s <- S.TText _ f s

-- | Content decoder.
type DecodeContent c = S.TTree -> B.Ab c

-- | Content calculator.
type CalcContent c = S.TTree -> B.Ab c

-- | Decode content from string.
stringContent :: (D.CContent c) => String -> B.Ab c
stringContent = S.toTree B.>=> treeContent

-- | Decode content from token tree.
treeContent :: forall c. (D.CContent c) => DecodeContent c
treeContent tree = Msg.abLiteral tree $ cons tree where
    cons :: DecodeContent c
    cons x@(P.L tok)
        = eithcon (eithcon (eithcon (token tok)
            D.putClock  $ D.tokenClock tok)
            D.putTime   $ D.treesTime   [x])
            decimal     $ D.treesDigits [x]
    cons (P.B b xs) = case b of
        S.BracketGroup   -> group xs
        S.BracketList    -> D.putList   =<< treesContents cons xs
        S.BracketSet     -> D.putSet    =<< treesContents cons xs
        S.BracketTie     ->                 treesTie      cons xs
        S.BracketRel     -> D.putRel    =<< treesRel      cons xs
        S.BracketType    -> D.putType   =<< D.treesType        xs
        S.BracketInterp  -> D.putInterp =<< D.treesInterp      xs
        _                -> Msg.unkBracket
    cons _ = B.bug "treeContent"

    token :: S.Token -> B.Ab c
    token (Text n w)
        | n <= S.TextRaw     = keyword w
        | n == S.TextQ       = D.putCode w
        | n == S.TextTerm    = D.putTerm $ S.toTermName w
        | otherwise          = D.putText w
    token t                  = Msg.unkWord $ S.tokenContent t

    group xs@(P.LText f _ : _)
        | f == S.TextRaw   = case decimal =<< D.treesDigits xs of
                               Right c  -> Right c
                               Left _   -> D.putTime =<< D.treesTime xs
    group []               = Right D.empty
    group _                = Msg.adlib "unknown content"

    eithcon f      = either (const f)
    decimal        = D.putDec B.<.> D.decodeDecimal

    keyword :: String -> B.Ab c
    keyword "(+)"  = Right D.true
    keyword "(-)"  = Right D.false
    keyword "(/)"  = Right D.end
    keyword "0"    = Right D.false  -- obsolete
    keyword "1"    = Right D.true   -- obsolete
    keyword "dum"  = Right D.dum
    keyword "dee"  = Right D.dee
    keyword w      = Msg.unkWord w

-- List of contents
--
--   { 0 | 1 | 2 }
--   [ 0 | 1 | 2 ]
--     .........
--     :
--     treesContents
--
treesContents :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab [c]
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
treesTie :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab c
treesTie cons xs@(P.LTerm _ : _) = D.putTie =<< treesTerms cons xs
treesTie _ [] = D.putTie []
treesTie _ [P.LRaw "words", P.LQq ws] = D.putList $ map D.pText $ words ws
treesTie _ _ = Msg.adlib "unknown tie"

-- Terms
--
--   /a 1  /b 2  /c 3
--   ................
--   :
--   treesTerms
--
treesTerms :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab [S.Term c]
treesTerms cons = mapM p B.<.> D.treesTerms1 where
    p (name, tree) = Right . (name,) =<< cons tree

-- | Decode judge from token trees.
--   Judges itself are not content type.
--   It can be only used in the top-level of resources.
treesJudge ::
    (D.CContent c)
    => D.CacheT           -- ^ Term name cache
    -> D.AssertType       -- ^ Assertion type
    -> D.JudgeClass       -- ^ Judgement class
    -> [S.TTree]          -- ^ Trees of terms
    -> B.Ab (D.CacheT, D.Judge c)   -- ^ Error or decoded judgement
treesJudge cc q cl trees =
    do (cc', ts) <- D.treesTermsCached cc trees
       terms <- mapM term ts
       Right (cc', D.assertAs q cl terms)
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
treesRel :: (D.CContent c) => DecodeContent c -> [S.TTree] -> B.Ab (D.Rel c)
treesRel cons xs =
    do bo <- treeTuple cons n `mapM` xs'
       Right $ D.Rel he $ B.unique bo
    where
      (ns, xs')  = treesTermNames xs
      n          = length ns
      he         = D.headFrom ns

-- | Split term names.
treesTermNames :: [S.TTree] -> ([S.TermName], [S.TTree])
treesTermNames = terms [] where
    terms ns (P.LTerm n : xs) = terms (S.toTermName n : ns) xs
    terms ns xs = (reverse ns, xs)

-- | Decode specific number of contents.
treeTuple :: (D.CContent c) => DecodeContent c -> Int -> S.TTree -> B.Ab [c]
treeTuple cons n g@(P.B S.BracketList xs) =
    do cs <- treesContents cons xs
       let n' = length cs
       B.when (n /= n') $ Msg.abLiteral g $ Msg.oddRelation n n'
       Right cs
treeTuple _ _ g = Msg.abLiteral g $ Msg.reqRelTuple


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Line-like token-level structure of source code.

module Koshucode.Baala.Core.Resource.Clause
  ( -- * Data type
    Clause (..),
    ClauseHead (..),
    ClauseBody (..),
  
    -- * Functions
    clauseTypeText,
    consClause,
  ) where

import qualified Data.Char                              as Ch
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax                 as S
import qualified Koshucode.Baala.Type                   as T
import qualified Koshucode.Baala.Core.Lexmap            as C
import qualified Koshucode.Baala.Syntax.Pattern         as P
import qualified Koshucode.Baala.Data.Message           as Msg
import qualified Koshucode.Baala.Core.Resource.Message  as Msg


-- ----------------------  Data type

-- | Line-like structure of source code.
data Clause t = Clause
    { clauseHead    :: ClauseHead t   -- ^ Common part of the clause.
    , clauseBody    :: ClauseBody t   -- ^ Proper part of the clause.
    } deriving (Show)

-- | Common part of clause.
data ClauseHead t = ClauseHead
    { clauseSecNo   :: C.SecNo          -- ^ Section number of the clause.
    , clauseShort   :: [S.ShortDef t]   -- ^ Short setting.
    , clauseAbout   :: [S.TToken t]     -- ^ About setting.
    , clauseSource  :: S.TokenClause t  -- ^ Source code of the clause.
    } deriving (Show)

-- | Proper part of clause.
data ClauseBody t
    = CJudge    T.AssertType S.JudgeClass
                       [S.TToken t]    -- ^ __Relational:__ Judge
    | CAssert   T.AssertType S.JudgeClass
                       [S.TToken t]    -- ^ __Relational:__ Assertion
    | CRelmap   String [S.TToken t]    -- ^ __Relational:__ Source of relmap
    | CSlot     String [S.TToken t]    -- ^ __Relational:__ Global slot

    | CInput    [S.TToken t]           -- ^ __Resource:__ Input point
    | COutput   [S.TToken t]           -- ^ __Resource:__ Output point
    | COption   [S.TToken t]           -- ^ __Resource:__ Option settings
    | CExport   String                 -- ^ __Resource:__ Exporting name

    | CEcho     (S.TokenClause t)      -- ^ __Other:__ Echo text
    | CLicense  String                 -- ^ __Other:__ License text
    | CBodies   [ClauseBody t]         -- ^ __Other:__ Multiple bodies
    | CUnknown  (B.Ab ())              -- ^ __Other:__ Unknown clause
      deriving (Show)

-- | The empty clause heading.
instance B.Default (ClauseHead t) where
    def = ClauseHead { clauseSecNo  = 0
                     , clauseShort  = []
                     , clauseAbout  = []
                     , clauseSource = B.def }

instance (O.Textual t) => B.GetCodePos (Clause t) where
    getCPs = B.getCPs . clauseHead

instance (O.Textual t) => B.GetCodePos (ClauseHead t) where
    getCPs = B.getCPs . clauseSource

-- | Name of clause type. e.g., @\"relmap\"@, @\"assert\"@.
clauseTypeText :: Clause t -> String
clauseTypeText (Clause _ body) =
    case body of
      CInput    _       -> "input"
      CExport   _       -> "export"
      CRelmap   _ _     -> "relmap"
      CAssert   _ _ _   -> "assert"
      CJudge    _ _ _   -> "judge"
      CSlot     _ _     -> "slot"
      COption   _       -> "option"
      COutput   _       -> "output"
      CEcho     _       -> "echo"
      CLicense  _       -> "license"
      CBodies   _       -> "bodies"
      CUnknown  _       -> "unknown"


-- ----------------------  Construction

-- | Convert token list into clause list.
--   Result clause list does not contain
--   'CRelmap' and 'CAssert'. Instead of them,
--   'TTokmap' and 'CAssert' are contained.
--   This function does not depend on 'C.ConsLexmap'.
--
--   >>> consClause . B.tokenize $ "a : source A /x /y"
--   [ TTokmap ( TokenClause
--                [TText 1 0 "a", TSpace 2 1, ..., TTerm 11 ["/y"]]
--                [CodeLine 1 "a : source A /x /y" [TText 1 0 "a", ...]] )
--            "a" [ TText 5 0 "source"
--                , TText 7 0 "A"
--                , TTerm 9 ["/x"]
--                , TTerm 11 ["/y"]
--                ]]

-- | First step of constructing 'Resource'.
consClause :: (O.Textual t) => [S.TToken t] -> C.SecNo -> [S.TokenLine t] -> [Clause t]
consClause resAbout sec = loop h0 . S.tokenClauses where
    h0 = B.def { clauseSecNo = sec }

    loop _ []     = []
    loop h (x:xs) = case consClauseEach resAbout $ h { clauseSource = x } of
                      (Clause h1 (CBodies bs), h2) ->
                          (Clause h1 <$> bs) ++ loop h2 xs
                      (c, h2)  -> c : loop h2 xs

consClauseEach :: forall t. (O.Textual t) => [S.TToken t] -> ClauseHead t -> (Clause t, ClauseHead t)
consClauseEach resAbout h@(ClauseHead sec sh about src) = rslt where

    rslt = case tokens of
             Right ts                    -> dispatch ts
             Left tok@(S.TUnknown _ _ a) -> unkClause [tok] $ Left a
             Left tok@(S.TShort _ pre _) -> unkClause [tok] $ Msg.unresPrefix $ O.tString pre
             Left tok                    -> unkClause [tok] $ Msg.bug "clause"

    tokens | O.some unks    = Left $ head unks     -- include unknown tokens
           | O.some sh      = mapM unshorten orig  -- unshorten short tokens
           | O.some shorts  = Left $ head shorts   -- include short tokens
           | otherwise      = Right orig

    unshorten :: S.TToken t -> Either (S.TToken t) (S.TToken t)
    unshorten t@(S.TShort n pre b) =
        case lookup pre sh of
          Just l  -> Right $ S.TText n S.TextQQ (l O.++ b)
          Nothing -> Left t
    unshorten t = Right t

    -- ----------------------  Utility

    orig           = B.clauseTokens src
    checks         = filter checkToken orig
    unks           = filter S.isUnknownToken checks
    shorts         = filter S.isShortToken checks
    checkToken t   = S.isUnknownToken t || S.isShortToken t
    isDelim        = ( `elem` ["=", ":", "|"] )
    lower          = O.tMap Ch.toLower
    empty          = CBodies []
    unkClause ts a = clause $ CUnknown $ unk ts a

    -- ----------------------  Clause dispatcher

    dispatch :: [S.TToken t] -> (Clause t, ClauseHead t)
    dispatch (P.TBar (O.cut -> O.Jp '|' k) : xs)   -- Frege's judgement stroke
                                = clause   $ frege (lower k) xs
    dispatch (P.TRaw name : P.TRaw is : body)
        | isDelim is            = clause   $ CRelmap (O.tString name) body
    dispatch (P.TRaw eq : _) | eq == "==="
                                = newSec
    dispatch (P.TRaw k : xs)
        | k == "input"          = clause   $ CInput xs
        | k == "include"        = clause   $ CInput xs
        | k == "export"         = clause   $ expt xs
        | k == "short"          = newShort $ short xs
        | k == "about"          = newAbout xs
        | k == "option"         = clause   $ COption xs
        | k == "output"         = clause   $ COutput xs
        | k == "echo"           = clause   $ CEcho src
        | k == "****"           = clause   $ empty
    dispatch (S.TSlot _ S.SlotGlobal n : xs)
                                = clause   $ CSlot (O.tString n) xs
    dispatch []                 = clause   $ empty
    dispatch [P.TLicense ln]    = clause   $ CLicense $ O.tString $ O.trimEnd ln
    dispatch _                  = clause   $ CUnknown $ unkAtStart []

    -- Return form
    clause :: ClauseBody t -> (Clause t, ClauseHead t)
    clause b           = (Clause h b, h)
    newSec             = (Clause h empty, h { clauseSecNo = sec + 1 })
    newShort (sh', b)  = (Clause h b,     h { clauseShort = sh' })
    newAbout about'    = (Clause h empty, h { clauseAbout = about' })

    -- Error messages
    unk ts msg     = let cp = B.getCPs $ B.headNull (head orig) ts
                     in Msg.abClause cp msg
    unkAt ts xs    = unk ts $ Msg.unkClause xs
    unkAtStart     = unkAt orig
    unkEEA e f a   = unkAtStart $ Msg.expect2Actual e f a

    -- ----------------------  Judgement or assertion

    -- Frege's content lines, or logical qualities
    frege :: t -> [S.TToken t] -> ClauseBody t
    frege "--"     = judge T.AssertAffirm
    frege "-x"     = judge T.AssertDeny
    frege "-xx"    = judge T.AssertMultiDeny
    frege "-c"     = judge T.AssertChange
    frege "-cc"    = judge T.AssertMultiChange
    frege "-v"     = judge T.AssertViolate

    frege "=="     = assert T.AssertAffirm
    frege "=x"     = assert T.AssertDeny
    frege "=xx"    = assert T.AssertMultiDeny
    frege "=c"     = assert T.AssertChange
    frege "=cc"    = assert T.AssertMultiChange
    frege "=v"     = assert T.AssertViolate

    frege s        = const $ CUnknown $ unkEEA
                             "|--, |-x, |-xx, |-c, |-cc, |-v,"
                             "  or |=x, |=xx, |=c, |=cc, |=v"
                             ("|" O.++ O.tString s)

    -- Judgement
    judge q (P.T _ p : xs)  = CJudge q (O.tString p) $ addAbout xs
    judge _ ts              = CUnknown $ judgeError ts

    judgeError []           = unkAtStart ["Give a judgement pattern"]
    judgeError ts           = unkAt ts ["Use text in judgement pattern"]

    addAbout xs
        | null resAbout && null about  = xs
        | otherwise                    = resAbout ++ about ++ xs

    -- Assertion
    assert q (P.T _ p : xs) =
        case S.splitTokensBy isDelim xs of
          Just (_, _, expr)      -> a expr
          Nothing                -> a xs
        where a expr              = CAssert q (O.tString p) expr
    assert _ ts                   = CUnknown $ judgeError ts

    -- ----------------------  Short signs

    short xs  = case wordPairs xs of
                  Just sh'  -> (sh', shortCheck sh')
                  Nothing   -> (sh, CUnknown $ unkAtStart [])

    shortCheck :: [S.ShortDef t] -> ClauseBody t
    shortCheck sh'
        | O.some prefix    = abort $ Msg.dupPrefix prefix
        | O.some replace   = abort $ Msg.dupReplacement replace
        | O.some invalid   = abort $ Msg.invalidPrefix invalid
        | otherwise        = empty
        where (pre, rep)   = unzip sh'
              prefix       = B.duplicates pre
              replace      = B.duplicates rep
              invalid      = B.omit isShortPrefix pre
              abort msg    = CUnknown $ unk orig msg

    -- Others
    expt (P.T _ n : P.T _ ":" : xs)
                       = CBodies [CExport $ O.tString n, CRelmap (O.tString n) xs]
    expt [P.T _ n]     = CExport $ O.tString n
    expt _             = CUnknown $ unkAtStart []

-- | Test string is short prefix.
isShortPrefix :: (O.Textual t) => O.Test t
isShortPrefix = O.tAll Ch.isAlpha

pairs :: [a] -> Maybe [(a, a)]
pairs (a:b:cs)  = do cs' <- pairs cs
                     Just $ (a, b) : cs'
pairs []        = Just []
pairs _         = Nothing

wordPairs :: [S.TToken t] -> Maybe [(t, t)]
wordPairs toks =
    do ps <- pairs toks
       mapM wordPair ps
    where
      wordPair (P.T _ a, P.T _ b) = Just (a, b)
      wordPair _ = Nothing


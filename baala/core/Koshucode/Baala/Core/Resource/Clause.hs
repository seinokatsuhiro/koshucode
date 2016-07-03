{-# LANGUAGE PatternSynonyms #-}
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

import qualified Data.Char                              as Char
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Syntax                 as S
import qualified Koshucode.Baala.Data                   as D
import qualified Koshucode.Baala.Core.Lexmap            as C
import qualified Koshucode.Baala.Data.Message           as Msg
import qualified Koshucode.Baala.Core.Resource.Message  as Msg


-- ----------------------  Data type

-- | Line-like structure of source code.
data Clause = Clause
    { clauseHead    :: ClauseHead   -- ^ Common part of the clause.
    , clauseBody    :: ClauseBody   -- ^ Proper part of the clause.
    } deriving (Show)

-- | Common part of clause.
data ClauseHead = ClauseHead
    { clauseSecNo   :: C.SecNo          -- ^ Section number of the clause.
    , clauseShort   :: [S.ShortDef]     -- ^ Short setting.
    , clauseAbout   :: [S.Token]        -- ^ About setting.
    , clauseSource  :: S.TokenClause    -- ^ Source code of the clause.
    } deriving (Show)

-- | Proper part of clause.
data ClauseBody
    = CInput    [S.Token]                            -- ^ Input point
    | CExport   String                               -- ^ Exporting name
    | CRelmap   String [S.Token]                     -- ^ Source of relmap
    | CAssert   D.AssertType D.JudgeClass [S.Token]  -- ^ Assertion
    | CJudge    D.AssertType D.JudgeClass [S.Token]  -- ^ Judge
    | CSlot     String [S.Token]                     -- ^ Global slot
    | COption   [S.Token]                            -- ^ Option settings
    | COutput   [S.Token]                            -- ^ Output point
    | CEcho     S.TokenClause                        -- ^ Echo text
    | CLicense  String                               -- ^ License text
    | CBodies   [ClauseBody]                         -- ^ Multiple bodies
    | CUnknown  (B.Ab ())                            -- ^ Unknown clause
      deriving (Show)

-- | The empty clause heading.
instance B.Default ClauseHead where
    def = ClauseHead { clauseSecNo  = 0
                     , clauseShort  = []
                     , clauseAbout  = []
                     , clauseSource = B.codeClauseEmpty }

instance B.CodePtr ClauseHead where
    codePtList = B.codePtList . clauseSource

-- | Name of clause type. e.g., @\"relmap\"@, @\"assert\"@.
clauseTypeText :: Clause -> String
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
consClause :: [S.Token] -> C.SecNo -> [S.TokenLine] -> [Clause]
consClause resAbout sec = loop h0 . S.tokenClauses where
    h0 = B.def { clauseSecNo = sec }

    loop _ []     = []
    loop h (x:xs) = case consClauseEach resAbout $ h { clauseSource = x } of
                      (Clause h1 (CBodies bs), h2) ->
                          (Clause h1 <$> bs) ++ loop h2 xs
                      (c, h2)  -> c : loop h2 xs

consClauseEach :: [S.Token] -> ClauseHead -> (Clause, ClauseHead)
consClauseEach resAbout h@(ClauseHead sec sh about src) = rslt where

    -- ----------------------  Utility

    rslt = case tokens of
             Right ts -> dispatch ts
             Left tok@(S.TUnknown _ _ a)
                      -> (unkClause $ unk [tok] $ Left a, h)
             Left tok@(S.TShort _ pre _)
                      -> (unkClause $ unk [tok] $ Msg.unresPrefix pre, h)
             Left tok -> (unkClause $ unk [tok] $ Msg.bug "short", h)

    original = B.clauseTokens src
    unks     = filter isUnknown original
    tokens | B.notNull unks = Left $ head unks
           | B.notNull sh   = unshorten `mapM` original
           | otherwise      = case filter S.isShortToken original of
                                []       -> Right original
                                tok : _  -> Left tok

    isUnknown (S.TUnknown _ _ _) = True
    isUnknown _                  = False

    unshorten :: S.Token -> Either S.Token S.Token
    unshorten t@(S.TShort n pre b) = case lookup pre sh of
                                       Just l  -> Right $ S.TTextQQ n $ l ++ b
                                       Nothing -> Left t
    unshorten t = Right t

    isDelim        = ( `elem` ["=", ":", "|"] )
    lower          = map Char.toLower
    empty          = CBodies []
    unkClause a    = Clause h $ CUnknown a

    -- ----------------------  Clause dispatcher

    dispatch (S.TTextBar _ ('|' : k) : xs)   -- Frege's judgement stroke
                                    = clause    $ frege (lower k) xs
    dispatch (S.TTextRaw _ name : S.TTextRaw _ is : body)
        | isDelim is                = clause    $ CRelmap name body
    dispatch (S.TTextSect _ : _)    = newSec
    dispatch (S.TTextRaw _ k : xs)
        | k == "input"              = clause    $ CInput xs
        | k == "include"            = clause    $ CInput xs
        | k == "export"             = clause    $ expt xs
        | k == "short"              = newShort  $ short xs
        | k == "about"              = newAbout  xs
        | k == "option"             = clause    $ COption xs
        | k == "output"             = clause    $ COutput xs
        | k == "echo"               = clause    $ CEcho src
        | k == "****"               = clause    $ empty
    dispatch (S.TSlot _ 2 n : xs)   = clause    $ CSlot n xs
    dispatch []                     = clause    $ empty
    dispatch [S.TTextLicense _ ln]  = clause    $ CLicense $ B.trimRight ln
    dispatch _                      = clause    $ CUnknown $ unkAtStart []

    -- Return form
    clause b              = (Clause h b, h)
    newSec                = (Clause h empty, h { clauseSecNo = sec + 1 })
    newShort (sh', b)     = (Clause h b,     h { clauseShort = sh' })
    newAbout about'       = (Clause h empty, h { clauseAbout = about' })

    -- Error messages
    unk ts msg     = let cp = B.codePtList $ B.headNull (head original) ts
                     in Msg.abClause cp msg
    unkAt ts xs    = unk ts $ Msg.unkClause xs
    unkAtStart     = unkAt original
    unkEEA e f a   = unkAtStart $ Msg.expect2Actual e f a

    -- ----------------------  Judgement or assertion

    -- Frege's content lines, or logical qualities
    frege "--"     = judge D.AssertAffirm
    frege "-x"     = judge D.AssertDeny
    frege "-xx"    = judge D.AssertMultiDeny
    frege "-c"     = judge D.AssertChange
    frege "-cc"    = judge D.AssertMultiChange
    frege "-v"     = judge D.AssertViolate

    frege "=="     = assert D.AssertAffirm
    frege "=x"     = assert D.AssertDeny
    frege "=xx"    = assert D.AssertMultiDeny
    frege "=c"     = assert D.AssertChange
    frege "=cc"    = assert D.AssertMultiChange
    frege "=v"     = assert D.AssertViolate

    frege s        = const $ CUnknown $ unkEEA
                             "|--, |-x, |-xx, |-c, |-cc, |-v,"
                             "  or |=x, |=xx, |=c, |=cc, |=v"
                             ("|" ++ s)

    -- Judgement
    judge q (S.TText _ _ p : xs)  = CJudge q p $ addAbout xs
    judge _ ts                    = CUnknown $ judgeError ts

    judgeError []                 = unkAtStart ["Give a judgement pattern"]
    judgeError ts                 = unkAt ts ["Use text in judgement pattern"]

    addAbout xs | null resAbout && null about  = xs
                | otherwise                    = resAbout ++ about ++ xs

    -- Assertion
    assert q (S.TText _ _ p : xs) =
        case S.splitTokensBy isDelim xs of
          Just (_, _, expr)      -> a expr
          Nothing                -> a xs
        where a expr              = CAssert q p expr
    assert _ ts                   = CUnknown $ judgeError ts

    -- ----------------------  Short signs

    short xs  = case wordPairs xs of
                  Just sh'  -> (sh', shortCheck sh')
                  Nothing   -> (sh, CUnknown $ unkAtStart [])

    shortCheck :: [S.ShortDef] -> ClauseBody
    shortCheck sh'
        | B.notNull prefix    = abort $ Msg.dupPrefix prefix
        | B.notNull replace   = abort $ Msg.dupReplacement replace
        | B.notNull invalid   = abort $ Msg.invalidPrefix invalid
        | otherwise           = empty
        where (pre, rep)      = unzip sh'
              prefix          = B.duplicates pre
              replace         = B.duplicates rep
              invalid         = B.omit S.isShortPrefix pre
              abort msg       = CUnknown $ unk original msg

    -- Others
    expt (S.TText _ _ n : S.TText _ _ ":" : xs)
                                  = CBodies [CExport n, CRelmap n xs]
    expt [S.TText _ _ n]          = CExport n
    expt _                        = CUnknown $ unkAtStart []


pairs :: [a] -> Maybe [(a, a)]
pairs (a:b:cs)  = do cs' <- pairs cs
                     Just $ (a, b) : cs'
pairs []        = Just []
pairs _         = Nothing

wordPairs :: [S.Token] -> Maybe [(String, String)]
wordPairs toks =
    do p <- pairs toks
       mapM wordPair p
    where
      wordPair :: (S.Token, S.Token) -> Maybe (String, String)
      wordPair (S.TText _ _ a, S.TText _ _ b) = Just (a, b)
      wordPair _ = Nothing


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Intermidiate structure between 'String' and 'Resource'.

module Koshucode.Baala.Core.Resource.Clause
  ( -- * Data type
    -- $Documentation
    Clause (..),
    ClauseHead (..),
    ClauseBody (..),
  
    -- * Functions
    clauseHeadEmpty,
    clauseTypeText,
    consClause,
  ) where

import qualified Data.Generics                 as G
import qualified Data.Char                     as Char
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Data          as C
import qualified Koshucode.Baala.Core.Lexmap   as C
import qualified Koshucode.Baala.Core.Message  as Msg


-- ----------------------  Data type

data Clause =
    Clause { clauseHead    :: ClauseHead
           , clauseBody    :: ClauseBody
           } deriving (Show, G.Data, G.Typeable)

data ClauseHead = ClauseHead
    { clauseSource  :: B.TokenClause
    , clauseSecNo   :: C.SecNo
    , clauseShort   :: [B.ShortDef]
    , clauseAbout   :: [B.Token]
    } deriving (Show, G.Data, G.Typeable)

data ClauseBody
    = CInput    [B.Token]                          -- ^ Input point
    | CExport   String                             -- ^ Exporting name
    | CRelmap   String [B.Token]                   -- ^ Source of relmap
    | CAssert   C.AssertType B.JudgePat [B.Token]  -- ^ Assertion
    | CJudge    C.AssertType B.JudgePat [B.Token]  -- ^ Judge
    | CSlot     String [B.Token]                   -- ^ Global slot
    | COption   [B.Token]                          -- ^ Option settings
    | COutput   [B.Token]                          -- ^ Output point
    | CEcho     B.TokenClause                      -- ^ Echo text
    | CLicense  String                             -- ^ License text
      deriving (Show, G.Data, G.Typeable)

instance B.CodePtr Clause where
    codePtList = B.codePtList . clauseHead

instance B.CodePtr ClauseHead where
    codePtList = B.codePtList . clauseSource

-- | The empty clause heading.
clauseHeadEmpty :: ClauseHead
clauseHeadEmpty = ClauseHead B.codeClauseEmpty 0 [] []

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
consClause :: [B.Token] -> C.SecNo -> [B.TokenLine] -> [B.Ab Clause]
consClause add sec = loop h0 . B.tokenClauses where
    h0 = clauseHeadEmpty { clauseSecNo = sec }

    loop _ []     = []
    loop h (x:xs) = let (cs, h') = consClauseEach add $ h { clauseSource = x }
                       in cs ++ loop h' xs

consClauseEach :: [B.Token] -> ClauseHead -> ([B.Ab Clause], ClauseHead)
consClauseEach add h@(ClauseHead src sec sh ab) = result where

    original = B.clauseTokens src

    tokens | sh /= []   = lengthen `mapM` original
           | otherwise  = case filter B.isShortToken original of
                            []        -> Right original
                            tok : _   -> Left tok

    result = case tokens of
               Right ts -> dispatch ts
               Left tok@(B.TShort _ pre _)
                        -> (unk [tok] $ Msg.unresPrefix pre, h)
               Left tok -> (unk [tok] $ Msg.bug "short", h)

    c0             = Right . Clause h
    c1             = B.li1 . c0

    isDelim        = ( `elem` ["=", ":", "|"] )
    lower          = map Char.toLower

    -- clause dispatcher

    dispatch (B.TTextBar _ ('|' : k) : xs)   -- Frege's judgement stroke
                                    = normal    $ frege (lower k) xs
    dispatch (B.TTextRaw _ name : B.TTextRaw _ is : body)
        | isDelim is                = normal    $ rmap name body
    dispatch (B.TTextSect _ : _)    = newSec
    dispatch (B.TTextRaw _ k : xs)
        | k == "input"              = normal    $ c1 $ CInput xs
        | k == "include"            = normal    $ c1 $ CInput xs
        | k == "export"             = normal    $ expt xs
        | k == "short"              = newShort  $ short xs
        | k == "about"              = newAbout  xs
        | k == "option"             = normal    $ c1 $ COption xs
        | k == "output"             = normal    $ c1 $ COutput xs
        | k == "echo"               = normal    $ c1 $ CEcho src
        | k == "****"               = normal    []
    dispatch (B.TSlot _ 2 n : xs)   = normal    $ c1 $ CSlot n xs
    dispatch []                     = normal    []
    dispatch [B.TTextLicense _ ln]  = normal    $ c1 $ CLicense $ B.trimRight ln
    dispatch _                      = normal    $ unkAtStart []

    normal cs             = (cs, h)
    newSec                = ([], h { clauseSecNo = sec + 1 })
    newShort (sh', cs)    = (cs, h { clauseShort = sh' })
    newAbout ab'          = ([], h { clauseAbout = ab' })

    -- error messages

    unk ts msg     = let cp = B.codePtList $ B.headOr (head original) ts
                     in [Msg.abClause cp msg]
    unkAt ts xs    = unk ts $ Msg.unkClause xs
    unkAtStart     = unkAt original
    unkEEA e f a   = unkAtStart $ Msg.expect2Actual e f a

    -- Frege's content lines, or logical qualities

    frege "--"     = judge C.AssertAffirm
    frege "-x"     = judge C.AssertDeny
    frege "-xx"    = judge C.AssertMultiDeny
    frege "-c"     = judge C.AssertChange
    frege "-cc"    = judge C.AssertMultiChange
    frege "-v"     = judge C.AssertViolate

    frege "=="     = assert C.AssertAffirm
    frege "=x"     = assert C.AssertDeny
    frege "=xx"    = assert C.AssertMultiDeny
    frege "=c"     = assert C.AssertChange
    frege "=cc"    = assert C.AssertMultiChange
    frege "=v"     = assert C.AssertViolate

    frege s        = const $ unkEEA
                             "|--, |-x, |-xx, |-c, |-cc, |-v,"
                             "  or |=x, |=xx, |=c, |=cc, |=v"
                             ("|" ++ s)

    -- judgement and assertion, or source and sink

    judge q (B.TText _ _ p : xs)  = c1 $ CJudge q p $ add ++ ab ++ xs
    judge _ ts                    = judgeError ts

    judgeError []                 = unkAtStart ["Give a judgement pattern"]
    judgeError ts                 = unkAt ts ["Use text in judgement pattern"]

    assert q (B.TText _ _ p : xs) =
        case B.splitTokensBy isDelim xs of
          Right (_, _, expr)      -> a expr
          Left  expr              -> a expr
        where a expr              = c1 $ CAssert q p expr
    assert _ ts                   = judgeError ts

    -- lengthen short signs

    lengthen :: B.Token -> Either B.Token B.Token
    lengthen t@(B.TShort n pre b) = case lookup pre sh of
                                      Just l  -> Right $ B.TTextQQ n $ l ++ b
                                      Nothing -> Left t
    lengthen t = Right t

    short xs                      = case wordPairs xs of
                                      Just sh'  -> (sh', checkShort sh')
                                      Nothing   -> (sh, unkAtStart [])

    checkShort :: [B.ShortDef] -> [B.Ab Clause]
    checkShort sh'
        | B.notNull prefix    = abort $ Msg.dupPrefix prefix
        | B.notNull replace   = abort $ Msg.dupReplacement replace
        | B.notNull invalid   = abort $ Msg.invalidPrefix invalid
        | otherwise           = []
        where (pre, rep)      = unzip sh'
              prefix          = B.duplicates pre
              replace         = B.duplicates rep
              invalid         = B.omit B.isShortPrefix pre
              abort msg       = unk original msg

    -- others

    rmap n xs                     = c1 $ CRelmap n xs

    expt (B.TText _ _ n : B.TText _ _ ":" : xs)
                                  = c0 (CExport n) : rmap n xs
    expt [B.TText _ _ n]          = c1 $ CExport n
    expt _                        = unkAtStart []


pairs :: [a] -> Maybe [(a, a)]
pairs (a:b:cs)  = do cs' <- pairs cs
                     Just $ (a, b) : cs'
pairs []        = Just []
pairs _         = Nothing

wordPairs :: [B.Token] -> Maybe [(String, String)]
wordPairs toks =
    do p <- pairs toks
       mapM wordPair p
    where
      wordPair :: (B.Token, B.Token) -> Maybe (String, String)
      wordPair (B.TText _ _ a, B.TText _ _ b) = Just (a, b)
      wordPair _ = Nothing



-- ----------------------
-- $Documentation
--
--  There are eight types of 'Clause'.
--  Textual representation of 'Resource' is a list of clauses.
--  'consClause' constructs clause list from resource text.
--
--  [@short@ prefix full ...]
--    Clause for declarations of short signs
--
--  [@|--@ pattern \/name content ...]
--    Affirmative judgement clause
--
--  [@|-X@ pattern \/name content ...]
--    Denial judgement clause
--
--  [name @:@ relmap]
--    Relmap clause
--
--  [@|==@ pattern @:@ relmap]
--    Affirmative assertion clause
--
--  [@|=X@ pattern @:@ relmap]
--    Denial assertion clause
--
--  [@|=V@ pattern @:@ relmap]
--    Violated assertion clause
--
--  [@****@ blah blah blah ...]
--    Comment clause


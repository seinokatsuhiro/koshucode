{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text decomposition by subtext.
--
-- = Subtext
--
--   Subtext is a domain-specific language for text decomposition.
--   The @subtext@ relmap operator provides the functionality of this language.
--   In the following descrition, we use a symbol
--   /E/ for subtext expression,
--   //P/ for present term name,
--   //N/ for new term name,
--   /N/ for symbolic name,
--   /T/ for text,
--   /C/ for character (single-letter text).
--
-- == Relmap operator
--
--   The @subtext@ operator decompose text using subtext expression.
--   There are two ways for describing expression: single or bundle.
--
--   [ subtext /P ( E ) ]
--     Decompose text content of /\/P/ by single expression /E/.
--   [ subtext /P { N1 = E1 | N2 = E2 | ... } ]
--     Decompose text by bundle of named expressions.
--     Text decomposition starts from /N1/.
--
-- == Basic patterns
--
--   [ ? ]
--     Any character.
--   [ ?? ]
--     Context-dependent pattern.
--   [ begin ]
--     Beginning of text.
--   [ end ]
--     End of text.
--
-- == Texts
--
--   [ T ]
--     Text literal as /T/.
--   [ char T ]
--     Alternative occurences of characters included in /T/.
--   [ word T ]
--     Alternative occurences of words included in /T/.
--   [ C1 to C2 ]
--     Character betweeen C1 and C2.
--
--   [ space ]
--     Unicode space character or space-like control character
--     including tabs (HT\/VT), newlines (CR\/LF), or form feed (FF).
--     See 'Data.Char.isSpace'.
--   [ digit ]
--     ASCII digit.
--     See 'Data.Char.isDigit'.
--   [ letter ]
--     Unicode letter character.
--     See 'Data.Char.isLetter'.
--
--   [ SP ]
--     One-or-more spaces.
--   [ 012 ]
--     One-or-more digits.
--   [ ABC ]
--     One-or-more letters.
--
-- == Combinations
--
--   [ E1 | E2 ]
--     Alternative occurence of /E1/ or /E2/.
--   [ E1 ++ E2 ]
--     Sequence of /E1/ followed by /E2/.
--   [ E1 or E2 ]
--     Alternative occurence of /E1/ or /E2/.
--   [ E1 and E2 ]
--     /E1/ and additional condition /E2/.
--   [ not E ]
--     Inverted condition.
--   [ last E ]
--     Find last /E/.
--   [ ( E ) ]
--     Group expression /E/.
--   [ &#x5B; E &#x5D; ]
--     Zero-or-one occurences of /E/. (Option)
--   [ { E } ]
--     Zero-or-more occurences of /E/. (Zero-repeat)
--   [ {- E -} ]
--     One-or-more occurences of /E/. (One-repeat)
--
-- == Gathering subtext
--
--   [ /N E ]
--     Submatch for /E/ whose name is /\/N/.
--   [ /N ]
--     Named submatch, equivalent to /\/N/ ??.
--   [ off E ]
--     Turn off gathering text in /E/.
--   [ on E ]
--     Turn on gathering text in /E/.

module Koshucode.Baala.Rop.Flat.Subtext
  ( consSubtext, relmapSubtext, relkitSubtext,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Subtext            as T
import qualified Koshucode.Baala.Subtext.Expr       as T
import qualified Koshucode.Baala.Subtext.Para       as T
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg


-- --------------------------------------------  Operator

consSubtext :: (D.CContent c) => C.RopCons c
consSubtext med =
  do term  <- Op.getTerm   med "-term"
     sub   <- Op.getTrees  med "-subtext"
     trim  <- Op.getSwitch med "-trim"
     b     <- parseBundle sub
     let ns    = submatchNamesBundle b
         match = T.matchBundle b
     Right $ relmapSubtext med (term, ns, match, trim)

submatchNamesBundle :: CharBundle -> [String]
submatchNamesBundle b = T.submatchNames $ T.seq (snd <$> b)

relmapSubtext :: (D.CContent c) => C.Intmed c -> (S.TermName, [S.TermName], T.CharMatch, Bool) -> C.Relmap c
relmapSubtext med = C.relmapFlow med . relkitSubtext

relkitSubtext :: (D.CContent c) => (S.TermName, [S.TermName], T.CharMatch, Bool) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitSubtext _ Nothing = Right C.relkitNothing
relkitSubtext (n, ns, match, trim) (Just he1) = Right kit2 where
    pick    = Op.picker he1 [n]
    he2     = D.headAppend ns he1
    kit2    = C.relkitJust he2 $ C.RelkitOneToOne False f2
    result  = subtextResult trim ns
    f2 cs   = case pick cs of
               [c] | D.isText c
                   -> case match $ D.gText c of
                        Just (_, rs) -> result rs ++ cs
                        Nothing      -> result [] ++ cs
               _ -> result [] ++ cs

subtextResult :: (D.CEmpty c, D.CText c) => Bool -> [S.TermName] -> [(S.TermName, String)] -> [c]
subtextResult trim ns rs = result <$> ns where
    result n = case lookup n rs of
                 Just t  -> D.pText $ trimIf trim t
                 Nothing -> D.empty

trimIf :: Bool -> String -> String
trimIf True  t = B.trimBoth t
trimIf False t = t


-- --------------------------------------------  Parser

-- | Type for subtext expression.
type CharExpr = T.Expr Char

-- | Type for subtext bundle.
type CharBundle = T.Bundle Char

pattern L tok   <- B.TreeL tok
pattern B g xs  <- B.TreeB g _ xs
pattern Key  n  <- S.TTextRaw _ n
pattern Text t  <- S.TTextQQ  _ t
pattern Char c  <- S.TTextQQ  _ [c]
pattern Term n  <- S.TTermN _ _ n

unknownSyntax :: (Show a) => a -> B.Ab b
unknownSyntax x = Msg.adlib $ "subtext syntax error " ++ show x

unknownBracket :: (Show a) => a -> B.Ab b
unknownBracket g = Msg.adlib $ "subtext unknown bracket " ++ show g

unknownKeyword :: String -> B.Ab b
unknownKeyword n = Msg.adlib $ "subtext unknown keyword " ++ n

divide :: String -> S.TTreesTo [[S.TTree]]
divide s = S.divideTreesBy (== s)

-- | Parse token trees into subtext bundle.
parseBundle :: [S.TTree] -> B.Ab CharBundle
parseBundle = bundle where
    bundle xs@[B S.BracketSet sub] =
        case step1 `mapM` divide "|" sub of
          Left _    -> single xs
          Right nxs -> do let ns = fst <$> nxs
                          step2 ns `mapM` nxs
    bundle xs = single xs

    single xs = do e <- parseSubtext [] xs
                   Right $ [("start", e)]

    step1 :: [S.TTree] -> B.Ab (String, [S.TTree])
    step1 xs = case divide "=" xs of
                 [[L (Key n)], x] -> Right (n, x)
                 _                -> unknownSyntax xs

    step2 :: [String] -> (String, [S.TTree]) -> B.Ab (String, CharExpr)
    step2 ns (n, x) = do e <- parseSubtext ns x
                         Right (n, e)

-- | Parse token trees into subtext expression.
parseSubtext :: [String] -> [S.TTree] -> B.Ab CharExpr
parseSubtext ns = trees False where

    -- Trees
    trees :: Bool -> [S.TTree] -> B.Ab CharExpr
    trees _ [L (Key n), x]   = keyOp n x
    trees _ [L (Term n), x]  = Right . T.sub n =<< tree x  -- /N E
    trees _ [L (Term n)]     = Right $ T.sub n T.what      -- /N
    trees _ []               = Right T.succ                -- ()
    trees _ [x]              = tree x
    trees False xs           = opTop xs
    trees True  xs           = unknownSyntax $ show xs

    -- Leaf or branch
    tree :: S.TTree -> B.Ab CharExpr
    tree (L x)        = leaf x
    tree (B g xs)     = branch g xs
    tree x            = unknownSyntax x

    leaf :: S.Token -> B.Ab CharExpr
    leaf (Text t)     = Right $ T.equal t     -- "LITERAL"
    leaf (Key n)      = key n
    leaf x            = unknownSyntax x

    branch :: S.BracketType -> [S.TTree] -> B.Ab CharExpr
    branch S.BracketGroup xs  = opTop xs            -- ( E )
    branch S.BracketSet   xs  = bracket T.many  xs  -- { E }
    branch S.BracketTie   xs  = bracket T.many1 xs  -- {- E -}
    branch S.BracketList  xs  = bracket T.maybe xs  -- [ E ]
    branch br             _   = unknownBracket br

    bracket op xs = do e <- trees False xs
                       Right $ op e

    -- Prefix operators
    keyOp "char" (L (Text s))  = Right $ T.char s             -- char E
    keyOp "word" (L (Text s))  = Right $ T.word s             -- word E
    keyOp "not"  x             = Right . T.not    =<< tree x  -- not E
    keyOp "last" x             = Right . T.last   =<< tree x  -- last E
    keyOp "on"   x             = Right . T.gather =<< tree x  -- on E
    keyOp "off"  x             = Right . T.skip   =<< tree x  -- off E
    keyOp n _                  = unknownSyntax n

    key n | n `elem` ns  = Right $ T.change n
    key "?"              = Right T.any
    key "??"             = Right T.what
    key "begin"          = Right T.begin
    key "end"            = Right T.end
    key "space"          = Right T.space
    key "digit"          = Right T.digit
    key "letter"         = Right T.letter
    key "SP"             = many1 T.space
    key "012"            = many1 T.digit
    key "ABC"            = many1 T.letter
    key n                = unknownKeyword n

    many1 = Right . T.many1

    -- Infix operators
    opTop   = opAlt
    opAlt   = inf "|"   T.or   opSeq   -- E | E | E
    opSeq   = inf "++"  T.seq  opOr    -- E ++ E ++ E
    opOr    = inf "or"  T.or   opAnd   -- E or E or E
    opAnd   = inf "and" T.and  opTo    -- E and E and E
    opTo xs = case divide "to" xs of   -- E to E
                 [[L (Char from)], [L (Char to)]]
                        -> Right $ T.to from to
                 [xs2]  -> trees True xs2  -- Turn on loop check
                 _      -> unknownSyntax xs

    inf op f g xs = Right . f =<< mapM g (divide op xs)


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
--   [ ( E ) ]
--     Group expression /E/.
--
-- == Texts
--
--   [ T ]
--     Text literal as /T/.
--   [ char T ]
--     Alternative occurences of characters included in /T/.
--   [ word T ]
--     Alternative occurences of words included in /T/.
--   [ cat T ... ]
--     Characters in the Unicode general categories,
--     /T/ is one-letter or two-letter category name.
--     See 'T.categoryLookup'.
--   [ C1 to C2 ]
--     Characters betweeen C1 and C2.
--
--   [ sp ]
--     Unicode space character or space-like control character
--     including tabs (HT\/VT), newlines (CR\/LF), or form feed (FF).
--     See 'Data.Char.isSpace'.
--   [ 012 ]
--     Unicode number character.
--     See 'T.categoryNumber'.
--   [ digit ]
--     ASCII digits.
--     See 'Data.Char.isDigit'.
--   [ abc ]
--     Unicode letter or mark character.
--     See 'T.categoryLetter' or 'T.categoryMark'.
--   [ +- ]
--     Unicode punctuation or symbol character.
--     See 'T.categoryPunctuation' or 'T.categorySymbol'.
--   [ ascii ]
--     ASCII characters.
--     See 'Data.Char.isAscii'.
--   [ latin-1 ]
--     ISO 8859-1 (Latin-1) characters.
--     See 'Data.Char.isLatin1'.
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
--   [ before E ]
--     Sequence of not /E/.
--   [ E as T ]
--     Replace to text /T/ when /E/ is matched.
--   [ stay E ]
--     Match /E/ but input position is not changed.
--
-- == Repetition
--
--   [ { E } ]
--     Zero-or-more occurences of /E/. (Zero-repeat)
--   [ { E } ( N ) ]
--     /N/-times occurences of /E/.
--   [ { E } ( L to ) ]
--     Repetition of /E/ with lower bound /L/.
--   [ { E } ( to U ) ]
--     Repetition of /E/ with upper bound /U/.
--   [ { E } ( L to U ) ]
--     Repetition of /E/ with lower bound /L/ and upper bound /U/.
--   [ &#x5B; E &#x5D; ]
--     Zero-or-one occurences of /E/. (Option)
--   [ {- E -} ]
--     One-or-more occurences of /E/. (One-repeat)
--   [ E1 sep E2 ]
--     /E1/-separated /E2/. (Zero-repeat)
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
  ( SubtextPara, consSubtext, relmapSubtext, relkitSubtext,
  ) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Subtext            as T
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg


-- --------------------------------------------  Operator

type SubtextPara = (S.TermName, [T.NameDepth], T.CharMatch, Bool)

consSubtext :: (D.CContent c) => C.RopCons c
consSubtext med =
  do term  <- Op.getTerm   med "-term"
     sub   <- Op.getTrees  med "-subtext"
     trim  <- Op.getSwitch med "-trim"
     b     <- parseBundle sub
     let ns    = T.bundleSubmatch b
         match = T.matchBundle b
     Right $ relmapSubtext med (term, ns, match, trim)

relmapSubtext :: (D.CContent c) => C.Intmed c -> SubtextPara -> C.Relmap c
relmapSubtext med = C.relmapFlow med . relkitSubtext

relkitSubtext :: (D.CContent c) => SubtextPara -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitSubtext _ Nothing = Right C.relkitNothing
relkitSubtext (n, ns, match, trim) (Just he1) = Right kit2 where
    pick    = Op.picker he1 [n]
    he2     = D.headAppend (fst <$> ns) he1
    kit2    = C.relkitJust he2 $ C.RelkitOneToOne False f2
    result  = subtextResult trim ns
    f2 cs   = case pick cs of
               [c] | D.isText c
                   -> case match $ D.gText c of
                        Just (_, rs) -> result rs ++ cs
                        Nothing      -> result [] ++ cs
               _ -> result [] ++ cs

subtextResult :: (D.CEmpty c, D.CText c, D.CList c) =>
    Bool -> [T.NameDepth] -> [(S.TermName, String)] -> [c]
subtextResult trim ns rs = result <$> ns where
    text = D.pText . trimIf trim
    result (n, depth) =
        case lookup' n rs of
          ts | depth > 0  -> D.pList (text <$> ts)
          t : _           -> text t
          []              -> D.empty

lookup' :: (Eq a) => a -> [(a, b)] -> [b]
lookup' n ass = snd <$> filter eq ass where
    eq (n', _) = n == n'

trimIf :: Bool -> String -> String
trimIf True  t = B.trimBoth t
trimIf False t = t


-- --------------------------------------------  Parser

-- | Type for subtext bundle.
type CharBundle = T.Bundle Char

pattern L tok   <- B.TreeL tok
pattern K n     <- L (Key n)
pattern To      <- Key "to"

pattern B g xs  <- B.TreeB g _ xs
pattern G xs    <- B S.BracketGroup xs
pattern Empty   <- G []

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

unknownCategory :: String -> B.Ab b
unknownCategory n = Msg.adlib $ "subtext unknown general category " ++ n

divide :: String -> S.TTreesTo [[S.TTree]]
divide s = S.divideTreesBy (== s)

-- | Parse token trees into subtext bundle.
parseBundle :: [S.TTree] -> B.Ab CharBundle
parseBundle = bundle where
    bundle xs@[B S.BracketSet sub] =
        case step1 `mapM` divide "|" sub of
          Left _    -> single xs
          Right nxs -> do let ns = fst <$> nxs
                          es <- step2 ns `mapM` nxs
                          Right $ T.bundle es
    bundle xs = single xs

    single xs = do e <- parseSubtext [] xs
                   Right $ T.bundle [("start", e)]

    step1 :: [S.TTree] -> B.Ab (String, [S.TTree])
    step1 xs = case divide "=" xs of
                 [[K n], x] -> Right (n, x)
                 _                -> unknownSyntax xs

    step2 :: [String] -> (String, [S.TTree]) -> B.Ab (String, T.CharExpr)
    step2 ns (n, x) = do e <- parseSubtext ns x
                         Right (n, e)

-- | Parse token trees into subtext expression.
parseSubtext :: [String] -> [S.TTree] -> B.Ab T.CharExpr
parseSubtext ns = trees False where

    -- Trees
    trees :: Bool -> [S.TTree] -> B.Ab T.CharExpr
    trees False xs              = opTop xs
    trees True (K n : xs)       = pre n xs
    trees True [L (Term n), x]  = Right . T.sub n =<< tree x  -- /N E
    trees True [L (Term n)]     = Right $ T.sub n T.what      -- /N
    trees True []               = Right T.succ                -- ()
    trees True [x]              = tree x
    trees True  xs              = unknownSyntax $ show xs

    -- Leaf or branch
    tree :: S.TTree -> B.Ab T.CharExpr
    tree (L x)        = leaf x
    tree (B g xs)     = branch g xs
    tree x            = unknownSyntax x

    leaf :: S.Token -> B.Ab T.CharExpr
    leaf (Text t)     = Right $ T.equal t     -- "LITERAL"
    leaf (Key n)      = pre n []
    leaf x            = unknownSyntax x

    branch :: S.BracketType -> [S.TTree] -> B.Ab T.CharExpr
    branch S.BracketGroup   = opTop            -- ( E )
    branch S.BracketSet     = bracket T.many   -- { E }
    branch S.BracketTie     = bracket T.many1  -- {- E -}
    branch S.BracketList    = bracket T.maybe  -- [ E ]
    branch br               = const $ unknownBracket br

    bracket op xs = do e <- trees False xs
                       Right $ op e

    times :: [S.TTree] -> [S.TTree] -> B.Ab T.CharExpr
    times [ Empty , L To , Empty ] = bracket (T.many)
    times [ K a   , L To         ] = bracket (T.min (int a))
    times [ K a   , L To , Empty ] = bracket (T.min (int a))
    times [         L To , K b   ] = bracket (T.max (int b))
    times [ Empty , L To , K b   ] = bracket (T.max (int b))
    times [ K a   , L To , K b   ] = bracket (T.minMax (int a) (int b))
    times [ K a                  ] = bracket (T.minMax (int a) (int a))
    times xs = const $ unknownSyntax $ show xs

    int s = case B.readInt s of
              Just n   -> n
              Nothing  -> error "require integer"

    -- Infix operators
    opTop :: [S.TTree] -> B.Ab T.CharExpr
    opTop [B S.BracketSet xs, G m] = times m xs
    opTop xs = opAlt xs
    opAlt    = inf "|"   T.or    opSeq   -- E | E | E
    opSeq    = inf "++"  T.seq   opOr    -- E ++ E ++ E
    opOr     = inf "or"  T.or    opAnd   -- E or E or E
    opAnd    = inf "and" T.and   opSep   -- E and E and E

    inf op f g xs = Right . f =<< mapM g (divide op xs)

    opSep xs = case divide "sep" xs of  -- E sep E
                 [sep, v] -> do ev   <- trees False v
                                esep <- trees False sep
                                Right $ T.sep esep ev
                 [xs2]    -> opAs xs2
                 _        -> unknownSyntax xs

    opAs xs = case divide "as" xs of    -- E as E
                 [xs2, [L (Text to)]]
                        -> do e <- trees False xs2
                              Right $ T.asConst to e
                 [xs2]  -> opTo xs2
                 _      -> unknownSyntax xs

    opTo xs = case divide "to" xs of    -- C to C
                 [[L (Char from)], [L (Char to)]]
                        -> Right $ T.to from to
                 [xs2]  -> trees True xs2  -- Turn on loop check
                 _      -> unknownSyntax xs

    -- Prefix operators
    pre n [] | n `elem` ns   = Right $ T.change n

    pre "?"       []         = Right T.any
    pre "??"      []         = Right T.what
    pre "begin"   []         = Right T.begin
    pre "end"     []         = Right T.end
    pre "digit"   []         = Right T.digit
    pre "ascii"   []         = Right T.ascii
    pre "latin-1" []         = Right T.latin1
    pre "sp"      []         = Right T.space
    pre "012"     []         = Right $ T.categoryList T.categoryNumber
    pre "abc"     []         = Right $ T.categoryList (T.categoryLetter ++ T.categoryMark)
    pre "+-"      []         = Right $ T.categoryList (T.categoryPunctuation ++ T.categorySymbol)

    pre "char" [L (Text s)]  = Right $ T.char s             -- char T
    pre "word" [L (Text s)]  = Right $ T.word s             -- word T
    pre "not"    [x]         = Right . T.not    =<< tree x  -- not E
    pre "last"   [x]         = Right . T.last   =<< tree x  -- last E
    pre "before" [x]         = Right . T.before =<< tree x  -- before E
    pre "stay"   [x]         = Right . T.stay   =<< tree x  -- stay E
    pre "on"     [x]         = Right . T.gather =<< tree x  -- on E
    pre "off"    [x]         = Right . T.skip   =<< tree x  -- off E
    pre "cat"    xs          = do ks <- keyword `mapM` xs   -- cat T
                                  case T.category $ unwords ks of
                                    Right e -> Right e
                                    Left n  -> unknownCategory n
    pre n _                  = unknownKeyword n

    keyword (L (Key s))      = Right s
    keyword x                = unknownSyntax x


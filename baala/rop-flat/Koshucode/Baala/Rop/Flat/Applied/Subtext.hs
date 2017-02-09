{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text decomposition by subtext.

module Koshucode.Baala.Rop.Flat.Applied.Subtext
  ( ropsAppliedSubtext,
    -- * Relmap
    SubtextPara, consSubtext, relmapSubtext, relkitSubtext,
    -- * Subtext
    -- $Subtext
  ) where

import qualified Koshucode.Baala.Subtext            as T
import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Syntax.Pattern     as P
import qualified Koshucode.Baala.Rop.Base.Message   as Msg


-- | Implementation of relational operators.
ropsAppliedSubtext :: (K.CContent c) => [C.Rop c]
ropsAppliedSubtext = Rop.rops "applied"
    [ consSubtext    K.& [ "subtext /N E"
                           K.& "-term -subtext . -trim?" ]
    ]


-- --------------------------------------------  Operator

-- | Type of subtext parameters.
type SubtextPara = (K.TermName, [T.NameDepth], T.CharMatch, Bool)

-- | __subtext \/P E__
--
--   Construct @subtext@ relmap.
--   Decompose text content of \/P
--   by matcing with subtext expression E.
--   Terms contained in E are added to output relation.
--   Expression E is either single or bundle:
--
--   [( E1 )]
--     Decompose text by single expression E.
--   [{ N1 = E1 | N2 = E2 | ... }]
--     Decompose text by bundle of named expressions.
--     Text decomposition starts from N1.
--
consSubtext :: (K.CContent c) => C.RopCons c
consSubtext med =
  do term  <- Rop.getTerm   med "-term"
     sub   <- Rop.getTrees  med "-subtext"
     trim  <- Rop.getSwitch med "-trim"
     b     <- parseBundle sub
     let ns    = T.bundleSubmatch b
         match = T.matchBundle b
     Right $ relmapSubtext med (term, ns, match, trim)

-- | Create @subtext@ relmap.
relmapSubtext :: (K.CContent c) => C.Intmed c -> SubtextPara -> C.Relmap c
relmapSubtext med = C.relmapFlow med . relkitSubtext

-- | Create @subtext@ relkit.
relkitSubtext :: (K.CContent c) => SubtextPara -> Maybe K.Head -> K.Ab (C.Relkit c)
relkitSubtext _ Nothing = C.relkitUnfixed
relkitSubtext (n, ns, match, trim) (Just he1) = Right kit where
    pick    = K.pickDirect [n] he1
    he2     = K.headAppend ((K.toTermName . fst) <$> ns) he1
    kit     = C.relkitLine False he2 flow
    result  = subtextResult trim ns
    flow cs = case pick cs of
               [c] | K.isText c
                   -> case match $ K.gText c of
                        Just (_, rs) -> result rs ++ cs
                        Nothing      -> result [] ++ cs
               _ -> result [] ++ cs

subtextResult :: (K.CEmpty c, K.CText c, K.CList c) =>
    Bool -> [T.NameDepth] -> [(String, String)] -> [c]
subtextResult trim ns rs = result <$> ns where
    text = K.pText . trimIf trim
    result (n, depth) =
        case lookup' n rs of
          ts | depth > 0  -> K.pList (text <$> ts)
          t : _           -> text t
          []              -> K.empty

lookup' :: (Eq a) => a -> [(a, b)] -> [b]
lookup' n ass = snd <$> filter eq ass where
    eq (n', _) = n == n'

trimIf :: Bool -> String -> String
trimIf True  t = K.trimBoth t
trimIf False t = t


-- --------------------------------------------  Parser

-- | Type for subtext bundle.
type CharBundle = T.Bundle String Char

pattern LChar c <- P.LQq [c]
pattern To      <- P.LRaw "to"
pattern Empty   <- P.BGroup []

unknownSyntax :: (Show a) => a -> K.Ab b
unknownSyntax x = Msg.adlib $ "subtext syntax error " ++ show x

unknownBracket :: (Show a) => a -> K.Ab b
unknownBracket g = Msg.adlib $ "subtext unknown bracket " ++ show g

unknownKeyword :: String -> K.Ab b
unknownKeyword n = Msg.adlib $ "subtext unknown keyword " ++ n

unknownCategory :: String -> K.Ab b
unknownCategory n = Msg.adlib $ "subtext unknown general category " ++ n

divide :: String -> [K.Tree] -> [[K.Tree]]
divide s = K.divideTreesBy (== s)

-- | Parse token trees into subtext bundle.
parseBundle :: [K.Tree] -> K.Ab CharBundle
parseBundle = bundle where
    bundle xs@[P.BSet sub] =
        case step1 `mapM` divide "|" sub of
          Left _    -> single xs
          Right nxs -> do let ns = fst <$> nxs
                          es <- step2 ns `mapM` nxs
                          Right $ T.bundle es
    bundle xs = single xs

    single xs = do e <- parseSubtext [] xs
                   Right $ T.bundle [("start", e)]

    step1 :: [K.Tree] -> K.Ab (String, [K.Tree])
    step1 xs = case divide "=" xs of
                 [[P.LRaw n], x] -> Right (n, x)
                 _               -> unknownSyntax xs

    step2 :: [String] -> (String, [K.Tree]) -> K.Ab (String, T.CharExpr)
    step2 ns (n, x) = do e <- parseSubtext ns x
                         Right (n, e)

-- | Parse token trees into subtext expression.
parseSubtext :: [String] -> [K.Tree] -> K.Ab T.CharExpr
parseSubtext ns = trees False where

    -- Trees
    trees :: Bool -> [K.Tree] -> K.Ab T.CharExpr
    trees False xs              = opTop xs
    trees True (P.LRaw n : xs)  = pre n xs
    trees True [P.LTerm n, x]   = Right . T.sub n =<< tree x  -- /N E
    trees True [P.LTerm n]      = Right $ T.sub n T.what      -- /N
    trees True []               = Right T.succ                -- ()
    trees True [x]              = tree x
    trees True  xs              = unknownSyntax $ show xs

    -- Leaf or branch
    tree :: K.Tree -> K.Ab T.CharExpr
    tree (P.L x)      = leaf x
    tree (P.B g xs)   = branch g xs
    tree x            = unknownSyntax x

    leaf :: K.Token -> K.Ab T.CharExpr
    leaf (P.TQq t)    = Right $ T.equal t     -- "LITERAL"
    leaf (P.TRaw n)   = pre n []
    leaf x            = unknownSyntax x

    branch :: K.BracketType -> [K.Tree] -> K.Ab T.CharExpr
    branch K.BracketGroup   = opTop            -- ( E )
    branch K.BracketSet     = bracket T.many   -- { E }
    branch K.BracketTie     = bracket T.many1  -- {- E -}
    branch K.BracketList    = bracket T.maybe  -- [ E ]
    branch br               = const $ unknownBracket br

    bracket op xs = do e <- trees False xs
                       Right $ op e

    times :: [K.Tree] -> [K.Tree] -> K.Ab T.CharExpr
    times [ Empty   , To , Empty    ] = bracket (T.many)
    times [ P.LRaw a, To            ] = bracket (T.min (int a))
    times [ P.LRaw a, To , Empty    ] = bracket (T.min (int a))
    times [           To , P.LRaw b ] = bracket (T.max (int b))
    times [ Empty   , To , P.LRaw b ] = bracket (T.max (int b))
    times [ P.LRaw a, To , P.LRaw b ] = bracket (T.minMax (int a) (int b))
    times [ P.LRaw a                ] = bracket (T.minMax (int a) (int a))
    times xs = const $ unknownSyntax $ show xs

    int s = case K.tInt s of
              Just n   -> n
              Nothing  -> error "require integer"

    text (P.LQq x)             = Right x
    text (P.BGroup [P.LQq x])  = Right x
    text x                     = unknownSyntax x

    -- Infix operators
    opTop :: [K.Tree] -> K.Ab T.CharExpr
    opTop [P.BSet xs, P.BGroup m] = times m xs
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
                 [xs2, [P.LRaw "lower"]] -> Right . T.asLower =<< trees False xs2
                 [xs2, [P.LRaw "upper"]] -> Right . T.asUpper =<< trees False xs2
                 [xs2, [P.LRaw "wrap", a, b]]  -> do at <- text a
                                                     bt <- text b
                                                     e  <- trees False xs2
                                                     Right $ T.asWrap at bt e 
                 [xs2, [P.LRaw "prepend", a]]  -> do at <- text a
                                                     e  <- trees False xs2
                                                     Right $ T.asPrepend at e
                 [xs2, [P.LRaw "append", b]]   -> do bt <- text b
                                                     e  <- trees False xs2
                                                     Right $ T.asAppend bt e
                     
                 [xs2, [P.LQq to]]  -> Right . T.asConst to =<< trees False xs2
                 [xs2]              -> opTo xs2
                 _                  -> unknownSyntax xs

    opTo xs = case divide "to" xs of    -- C to C
                 [[LChar fr], [LChar to]] -> Right $ T.to fr to
                 [xs2]            -> trees True xs2  -- Turn on loop check
                 _                -> unknownSyntax xs

    -- Prefix operators
    pre n [] | n `elem` ns  = Right $ T.change n

    pre "?"       []        = Right T.any
    pre "??"      []        = Right T.what
    pre "begin"   []        = Right T.begin
    pre "end"     []        = Right T.end
    pre "digit"   []        = Right T.digit
    pre "ascii"   []        = Right T.ascii
    pre "latin-1" []        = Right T.latin1
    pre "sp"      []        = Right T.space
    pre "012"     []        = Right $ T.categoryList K.categoryNumber
    pre "abc"     []        = Right $ T.categoryList K.categoryAlpha
    pre "+-"      []        = Right $ T.categoryList K.categorySign
    pre "open"    []        = Right $ T.categoryList K.categoryOpen
    pre "close"   []        = Right $ T.categoryList K.categoryClose
    pre "koshu-symbol"  []  = Right koshuSymbol
    pre "koshu-general" []  = Right koshuGeneral
    pre "koshu-plain"   []  = Right koshuPlain
    pre "koshu-numeric" []  = Right koshuNumeric

    pre "char"   [P.LQq s]  = Right $ T.char s             -- char T
    pre "word"   [P.LQq s]  = Right $ T.word s             -- word T
    pre "not"    [x]        = Right . T.not    =<< tree x  -- not E
    pre "last"   [x]        = Right . T.last   =<< tree x  -- last E
    pre "before" [x]        = Right . T.before =<< tree x  -- before E
    pre "stay"   [x]        = Right . T.stay   =<< tree x  -- stay E
    pre "on"     [x]        = Right . T.gather =<< tree x  -- on E
    pre "off"    [x]        = Right . T.skip   =<< tree x  -- off E
    pre "cat"    xs         = do ks <- keyword `mapM` xs   -- cat T
                                 case T.category $ unwords ks of
                                   Right e -> Right e
                                   Left n  -> unknownCategory n
    pre n _                 = unknownKeyword n

    keyword (P.LRaw s)      = Right s
    keyword x               = unknownSyntax x

koshuSymbol :: T.CharExpr
koshuSymbol = T.elem "koshu-symbol" K.isSymbolChar

koshuGeneral :: T.CharExpr
koshuGeneral = T.elem "koshu-general" K.isGeneralChar

koshuPlain :: T.CharExpr
koshuPlain = T.elem "koshu-plain" K.isPlainChar

koshuNumeric :: T.CharExpr
koshuNumeric = T.elem "koshu-numeric" K.isNumericChar


-- --------------------------------------------  Subtext

-- $Subtext
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
-- == Basic pattern
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
--
-- == Text
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
--   [ open ]
--     Open punctuations.
--     See 'T.categoryOpen'.
--   [ close ]
--     Close punctuations.
--     See 'T.categoryClose'.
--   [ ascii ]
--     ASCII characters.
--     See 'Data.Char.isAscii'.
--   [ latin-1 ]
--     ISO 8859-1 (Latin-1) characters.
--     See 'Data.Char.isLatin1'.
--   [ koshu-symbol ]
--     Symbol character in Koshucode.
--   [ koshu-general ]
--     General character in Koshucode.
--   [ koshu-plain ]
--     Plain character in Koshucode.
--   [ koshu-numeric ]
--     Numeric character in Koshucode.
--
-- == Combination
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
-- == Modification
--
--   [ E as T ]
--     Replace to text /T/ when /E/ is matched.
--   [ E as wrap T1 T2 ]
--     Wrap matched text in /T1/ and /T2/.
--   [ E as prepend T ]
--     Prepend /T/ to matched text.
--   [ E as append T ]
--     Append /T/ to matched text.
--   [ E as lower ]
--     Convert text to lower case when /E/ is matched.
--   [ E as upper ]
--     Convert text to upper case when /E/ is matched.

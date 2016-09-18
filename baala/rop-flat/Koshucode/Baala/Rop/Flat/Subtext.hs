{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text decomposition by subtext.

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
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg


-- --------------------------------------------  Operator

consSubtext :: (D.CContent c) => C.RopCons c
consSubtext med =
  do n <- Op.getTerm  med "-term"
     t <- Op.getTrees med "-expr"
     e <- parseSubtext t
     Right $ relmapSubtext med (n, T.submatchNames e, T.matchExpr e)

relmapSubtext :: (D.CContent c) => C.Intmed c -> (S.TermName, [S.TermName], T.CharMatch) -> C.Relmap c
relmapSubtext med = C.relmapFlow med . relkitSubtext

relkitSubtext :: (D.CContent c) => (S.TermName, [S.TermName], T.CharMatch) -> Maybe D.Head -> B.Ab (C.Relkit c)
relkitSubtext _ Nothing = Right C.relkitNothing
relkitSubtext (n, ns, match) (Just he1) = Right kit2 where
    pick    =  Op.picker he1 [n]
    he2     = D.headAppend ns he1
    kit2    = C.relkitJust he2 $ C.RelkitOneToOne False f2
    result  = subtextResult ns
    f2 cs   = case pick cs of
               [c] | D.isText c
                   -> case match $ D.gText c of
                        Just (_, rs) -> result rs ++ cs
                        Nothing      -> result [] ++ cs
               _ -> result [] ++ cs

subtextResult :: (D.CEmpty c, D.CText c) => [S.TermName] -> [(S.TermName, String)] -> [c]
subtextResult ns rs = result <$> ns where
    result n = case lookup n rs of
                 Just t  -> D.pText t
                 Nothing -> D.empty


-- --------------------------------------------  Parser

type CharExpr = T.Expr Char

pattern L tok   <- B.TreeL tok
pattern Key  n  <- S.TTextRaw _ n
pattern Text t  <- S.TTextQQ  _ t
pattern Char c  <- S.TTextQQ  _ [c]
pattern Term n  <- S.TTermN _ _ n

-- | Parse token trees into subtext expression.
parseSubtext :: [S.TTree] -> B.Ab CharExpr
parseSubtext = trees False where

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
    tree (B.TreeL x)      = leaf x
    tree (B.TreeB g _ xs) = branch g xs

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
    keyOp "on"   x             = Right . T.gather =<< tree x  -- on E
    keyOp "off"  x             = Right . T.skip   =<< tree x  -- off E
    keyOp n _                  = unknownSyntax n

    key "?"           = Right T.any
    key "??"          = Right T.what
    key "begin"       = Right T.begin
    key "end"         = Right T.end
    key "space"       = Right T.space
    key "digit"       = Right T.digit
    key "letter"      = Right T.letter
    key "SP"          = many1 T.space
    key "012"         = many1 T.digit
    key "ABC"         = many1 T.letter
    key x             = unknownSyntax x

    many1 = Right . T.many1

    -- Infix operators
    opTop   = opAlt
    opAlt   = inf "|"   T.or   opSeq   -- E | E | E
    opSeq   = inf "++"  T.seq  opOr    -- E ++ E ++ E
    opOr    = inf "or"  T.or   opNot   -- E or E or E
    opNot   = inf "not" T.not' opTo    -- E not E not E
    opTo xs = case divide "to" xs of   -- E to E
                 [[L (Char from)], [L (Char to)]]
                        -> Right $ T.to from to
                 [xs2]  -> trees True xs2  -- Turn on loop check
                 _      -> unknownSyntax xs

    inf op f g xs = Right . f =<< mapM g (divide op xs)
    divide s = S.divideTreesBy (== s)

    -- Syntax error
    unknownSyntax x  = Msg.adlib $ "subtext syntax error " ++ show x
    unknownBracket g = Msg.adlib $ "subtext unknown bracket " ++ show g


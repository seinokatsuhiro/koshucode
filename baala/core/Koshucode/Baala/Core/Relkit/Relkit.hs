{-# OPTIONS_GHC -Wall #-}

-- | Specialized relmap.

module Koshucode.Baala.Core.Relkit.Relkit
  ( 
    -- * Datatype
    Relkit (..), RelkitCore (..),
  
    RelkitBody, RelkitKey, RelkitDef,
    Relbmap, RelSelect,
  
    RelkitFlow, RelkitHook', RelkitBinary, RelkitConfl,

    -- * Constructor
    relkit,
    relkitJust, relkitNothing,
    relkitId,
    relkitConst, relkitConstEmpty,
    relkitConstSingleton, relkitConstBody,
    relkitSource,
    relkitCopy, relkitNest, relkitNestVar,
    relkitSetSource,

    Local, Lexical,
    localsLines,
    a2lookup,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Lexmap  as C



-- ----------------------  Datatype

-- | Specialized relmap.
data Relkit c = Relkit
    { relkitHead :: Maybe B.Head
    , relkitBody :: RelkitBody c
    }

instance B.Monoid (Relkit c) where
    mempty = relkitConst B.reldee
    mappend (Relkit _ bo1) (Relkit he2 bo2) =
        relkit he2 $ RelkitAppend bo1 bo2

data RelkitCore c
    = RelkitFull         Bool (                [[c]] -> [[c]] )
    | RelkitOneToMany    Bool (                 [c]  -> [[c]] )
    | RelkitOneToOne     Bool (                 [c]  ->  [c]  )
    | RelkitPred              (                 [c]  -> Bool  )

    | RelkitAbFull       Bool ( [Relbmap c] -> [[c]] -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbMany  Bool ( [Relbmap c] ->  [c]  -> B.Ab [[c]] ) [RelkitBody c]
    | RelkitOneToAbOne   Bool ( [Relbmap c] ->  [c]  -> B.Ab  [c]  ) [RelkitBody c]
    | RelkitAbSemi            (                [[c]] -> B.Ab Bool  ) (RelkitBody c)
    | RelkitAbPred            (                 [c]  -> B.Ab Bool  )

    | RelkitAppend       (RelkitBody c) (RelkitBody c)
    | RelkitConst        [[c]]
    | RelkitId

    | RelkitSource       B.JudgePat [B.TermName]

    | RelkitLink         C.RopName RelkitKey (Maybe (RelkitBody c))
    | RelkitCopy         B.Token C.RopName (RelkitBody c)
    | RelkitNestVar      B.Token C.RopName
    | RelkitNest         B.Token [(String, Int)] (RelkitBody c)

instance Show (RelkitCore c) where
    show (RelkitFull        _ _)   = "RelkitFull"
    show (RelkitOneToMany   _ _)   = "RelkitOneToMany"
    show (RelkitOneToOne    _ _)   = "RelkitOneToOne"
    show (RelkitPred          _)   = "RelkitPred"

    show (RelkitAbFull    _ _ _)   = "RelkitAbFull"
    show (RelkitOneToAbMany _ _ _) = "RelkitOneToAbMany"
    show (RelkitOneToAbOne _ _ _)  = "RelkitOneToAbOne"
    show (RelkitAbSemi      _ _)   = "RelkitAbSemi"
    show (RelkitAbPred        _)   = "RelkitAbPred"

    show (RelkitConst         _)   = "RelkitConst"
    show (RelkitAppend      x y)   = "RelkitAppend " ++ show [x,y]
    show (RelkitId             )   = "RelkitId"

    show (RelkitSource     p ns)   = "RelkitSource " ++ p ++ " " ++ show ns
    show (RelkitLink      n _ _)   = "RelkitLink " ++ n
    show (RelkitCopy      _ _ _)   = "RelkitCopy "
    show (RelkitNestVar     _ n)   = "RelkitNestVar " ++ n
    show (RelkitNest      _ _ _)   = "RelkitNest "

type RelkitBody c = B.Sourced (RelkitCore c)
type RelkitKey    = (Maybe B.Head, [C.Lexmap])
type RelkitDef c  = (RelkitKey, Relkit c)

-- | Relation selector
type RelSelect c = B.JudgePat -> [String] -> B.Rel c

-- | Mapping for body of relation.
type Relbmap c = [[c]] -> B.Ab [[c]]

-- | Make 'C.Relkit' from heading of input relation.
type RelkitFlow c     = Maybe B.Head -> B.Ab (Relkit c)

-- | Make 'C.Relkit' from hook data and input heading.
type RelkitHook' h c  = h c -> RelkitFlow c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c   = Relkit c -> RelkitFlow c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c    = [Relkit c] -> RelkitFlow c


-- ----------------------  Constructor

relkit :: Maybe B.Head -> RelkitCore c -> Relkit c
relkit he = Relkit he . B.Sourced []

relkitJust :: B.Head -> RelkitCore c -> Relkit c
relkitJust he = relkit $ Just he

relkitNothing :: Relkit c
relkitNothing = relkit Nothing RelkitId

relkitId :: Maybe B.Head -> Relkit c
relkitId he = relkit he RelkitId

relkitConst :: B.Rel c -> Relkit c
relkitConst (B.Rel he bo) = relkitJust he $ RelkitConst bo

relkitConstEmpty :: [B.TermName] -> Relkit c
relkitConstEmpty ns = relkitConstBody ns []

relkitConstSingleton :: [B.TermName] -> [c] -> Relkit c
relkitConstSingleton ns tuple = relkitConstBody ns [tuple]

relkitConstBody :: [B.TermName] -> [[c]] -> Relkit c
relkitConstBody ns bo = kit where
    he  = B.headFrom ns
    kit = relkitJust he $ RelkitConst bo

relkitSource :: B.JudgePat -> [B.TermName] -> Relkit c
relkitSource p ns = relkitJust he kit where
    he  = B.headFrom ns
    kit = RelkitSource p ns

relkitCopy :: B.Token -> String -> B.Map (Relkit c)
relkitCopy p n (Relkit he kitb) = relkit he $ RelkitCopy p n kitb

relkitNest :: B.Token -> [(String, Int)] -> B.Map (Relkit c)
relkitNest p nest (Relkit he kitb) = relkit he $ RelkitNest p nest kitb

relkitNestVar :: B.Token -> String -> B.Head -> Relkit c
relkitNestVar p n he = relkitJust he $ RelkitNestVar p n

relkitSetSource :: (B.CodePtr a) => a -> B.Map (Relkit c)
relkitSetSource src (Relkit he (B.Sourced _ core)) =
    Relkit he $ B.Sourced (B.codePtList src) core


-- ----------------------  Local relations

type Local a = Lexical [B.Named a]

type Lexical a = (B.Token, a)

localsLines :: [Local a] -> [String]
localsLines xs = map desc $ a2keys xs where
    desc (a, bs) = B.tokenContent a ++ " / " ++ unwords bs

a2keys :: [(a, [(b, c)])] -> [(a, [b])]
a2keys = B.mapSndTo (map fst)

a2expand :: [(a, [(b, c)])] -> [((a, b), c)]
a2expand = concatMap f where
    f (a, bc)   = map (g a) bc
    g a (b, c)  = ((a, b), c)

a2lookup :: (Eq a, Eq b) => a -> b -> [(a, [(b, c)])] -> Maybe c
a2lookup a b = lookup a B.>=> lookup b


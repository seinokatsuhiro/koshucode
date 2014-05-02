{-# OPTIONS_GHC -Wall #-}

-- | Implementation of relmap operators.

module Koshucode.Baala.Core.Relmap.Operator
( -- * Rop
  Rop (..),
  RopUse (..),
  RopCons,

  -- * Relmap
  Relmap (..),
  Rodmap,
  relmapId,
  mapToRelmap,
  relmapLexList,

  -- * Relkit
  RelkitCalc,
  RelkitGlobal,
  RelkitBinary,
  RelkitConfl,

  -- * Global
  Global (..),
  globalCommandLine,
  globalFill,
  global,
  globalSyntax,
  globalFunction,
) where

import qualified Data.Version                           as D
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.Lexmap     as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Relkit     as C


-- ----------------------  Rop

-- | Implementation of relmap operator
data Rop c = Rop
    { ropName     :: String        -- ^ Operator name
    , ropGroup    :: String        -- ^ Operator group
    , ropSorter   :: C.RodSorter   -- ^ Operand sorter
    , ropCons     :: RopCons c     -- ^ Constructor of operator
    , ropUsage    :: String        -- ^ Usage of operator
    }

instance Show (Rop c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop (" ++ group ++ "/" ++ name ++ ")"

-- | Constructor of relmap operator
type RopCons c = RopUse c -> B.Ab (Relmap c)

-- | Use of relmap operator
data RopUse c = RopUse
    { ropGlobal    :: Global c
    , ropLexmap    :: C.Lexmap     -- ^ Syntactic data of operator use
    , ropSubrelmap :: [Relmap c]   -- ^ Subrelmaps
    } deriving (Show)

instance B.TokenListing (RopUse c) where
    tokenListing = B.tokenListing . ropLexmap



-- ----------------------  Relmap

-- | Relmap with operand.
type Rodmap c = C.Rody (Relmap c)

-- | Generic relmap.
data Relmap c
    = RelmapSource   C.Lexmap B.JudgePattern [B.TermName]
      -- ^ Retrieve a relation from a dataset
    | RelmapConst    C.Lexmap (B.Rel c)
      -- ^ Constant relation

    | RelmapGlobal   C.Lexmap (Global c -> RelkitCalc c)
      -- ^ Relmap that maps relations to a relation with globals
    | RelmapCalc     C.Lexmap (RelkitConfl c) [Relmap c]
      -- ^ Relmap that maps relations to a relation

    | RelmapCopy     C.Lexmap String (Relmap c)
      -- ^ Relmap for environment of input relation
    | RelmapWith     C.Lexmap [(B.TermName, String)] (Relmap c)
      -- ^ Relmap for environment of nested relations
    | RelmapLink     C.Lexmap String C.Rod
      -- ^ Relmap reference

    | RelmapAppend   (Relmap c) (Relmap c)
      -- ^ Connect two relmaps


-- | Map function to relmaps.
mapToRelmap :: B.Map (Relmap c) -> B.Map (Relmap c)
mapToRelmap f = mf where
    mf (RelmapAppend r1 r2)   = RelmapAppend (mf r1) (mf r2)
    mf (RelmapCalc   h g rs)  = RelmapCalc   h g (map mf rs)
    mf r1 = f r1

instance Show (Relmap c) where
    show = showRelmap

showRelmap :: Relmap c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)  = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)     = "RelmapConst "  ++ show (B.name r) ++ " _"

    sh (RelmapGlobal _ _)     = "RelmapGlobal " ++ show (B.name r)
    sh (RelmapCalc   _ _ rs)  = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs

    sh (RelmapCopy _ n r1)    = "RelmapCopy "   ++ show n ++ joinSubs [r1]
    sh (RelmapWith _ ns r1)   = "RelmapWith "   ++ show ns ++ joinSubs [r1]
    sh (RelmapLink _ n _)     = "RelmapLink "   ++ show n
    sh (RelmapAppend r1 r2)   = "RelmapAppend"  ++ joinSubs [r1, r2]

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance B.Monoid (Relmap c) where
    mempty  = relmapId
    mappend = RelmapAppend

-- | Identity relmap.
relmapId :: Relmap c
relmapId = RelmapCalc lexid (const $ Right . C.relkitId) []

lexid :: C.Lexmap
lexid = C.Lexmap (B.tokenWord "id") [("@operand", [])] [] "id"

instance B.Name (Relmap c) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  h _)     = C.lexOpText h
    name (RelmapCalc   h _ _)   = C.lexOpText h
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance B.Pretty (Relmap c) where
    doc (RelmapSource lx _ _)  = B.doc lx
    doc (RelmapConst  lx _)    = B.doc lx

    doc (RelmapGlobal lx _)    = B.doc lx -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapCalc   lx _ _)  = B.doc lx -- hang (text $ name m) 2 (doch (map doc ms))

    doc (RelmapCopy   _ _ r1)  = B.doc r1
    doc (RelmapWith   _ _ r1)  = B.doc r1
    doc (RelmapLink   lx _ _)  = B.doc lx
    doc (RelmapAppend r1 r2)   = B.docHang (B.doc r1) 2 (docRelmapAppend r2)

docRelmapAppend :: Relmap c -> B.Doc
docRelmapAppend = B.docv . map pipe . relmapAppendList where
    pipe m = B.doc "|" B.<+> B.doc m

{-| Expand 'RelmapAppend' to list of 'Relmap' -}
relmapAppendList :: Relmap c -> [Relmap c]
relmapAppendList = expand where
    expand (RelmapAppend r1 r2) = expand r1 ++ expand r2
    expand r = [r]

instance B.TokenListing (Relmap c) where
    tokenListing = B.tokenListing . relmapLex

instance Ord (Relmap c) where
    r1 `compare` r2 = relmapLexList r1 `compare` relmapLexList r2

instance Eq (Relmap c) where
    r1 == r2  =  compare r1 r2 == EQ

relmapLex :: Relmap c -> Maybe C.Lexmap
relmapLex r = case relmapLexList r of
                []       -> Nothing
                (lx : _) -> Just lx

relmapLexList :: Relmap c -> [C.Lexmap]
relmapLexList = collect where
    collect (RelmapSource  lx _ _)  = [lx]
    collect (RelmapConst   lx _)    = [lx]

    collect (RelmapGlobal  lx _)    = [lx]
    collect (RelmapCalc    lx _ _)  = [lx]

    collect (RelmapCopy    lx _ _)  = [lx]
    collect (RelmapWith    lx _ _)  = [lx]
    collect (RelmapLink    lx _ _)  = [lx]
    collect (RelmapAppend  r1 r2)   = collect r1 ++ collect r2



-- ----------------------  Relkit

-- | Make 'C.Relkit' from heading of input relation.
type RelkitCalc c   = Maybe B.Relhead -> B.Ab (C.Relkit c)

-- | Make 'C.Relkit' from globals and input heading.
type RelkitGlobal c = Global c -> RelkitCalc c

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c = C.Relkit c -> RelkitCalc c

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c  = [(C.Relkit c)] -> RelkitCalc c



-- ----------------------  Global

data Global c = Global
      { globalVersion :: D.Version
      , globalRops    :: [Rop c]
      , globalCops    :: ([C.Cop c], [B.Named B.InfixHeight])
      , globalProgram :: String
      , globalArgs    :: [String]
      , globalJudges  :: [B.Judge c]
      , globalSelect  :: C.RelSelect c
      }

instance Show (Global c) where
    show Global { globalRops = rops, globalCops = (cops, _) }
        = let nr = length rops
              nc = length cops
          in "Global (" ++ show nr ++ " rops, " ++ show nc ++ " cops)"

globalCommandLine :: Global c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

globalFill :: (C.CContent c) => B.Map (Global c)
globalFill g = g

global :: Global c
global = Global { globalVersion = D.Version [] []
                , globalRops    = []
                , globalCops    = ([], [])
                , globalProgram = ""
                , globalArgs    = []
                , globalJudges  = []
                , globalSelect  = \_ _ -> B.reldee }

globalSyntax :: Global c -> ([C.Cop c], [B.Named B.InfixHeight])
globalSyntax g = (syn, htab) where
    syn  = filter C.isCopSyntax $ fst cops
    htab = snd cops
    cops = globalCops g

globalFunction :: Global c -> [C.Cop c]
globalFunction = filter C.isCopFunction . fst . globalCops


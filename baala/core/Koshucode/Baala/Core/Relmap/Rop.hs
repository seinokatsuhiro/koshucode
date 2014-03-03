{-# OPTIONS_GHC -Wall #-}

-- | Implementation of relmap operators.

module Koshucode.Baala.Core.Relmap.Rop
( -- * Rop
  Rop (..),
  RopUse (..),
  RopCons,
  getArg1, getArg2, getArg3,

  -- * Relmap
  Relmap (..),
  mapToRelmap,

  -- * Relkit
  RelkitCalc,
  RelkitGlobal,
  RelkitBinary,
  RelkitConfl,

  -- * Global
  Global (..),
  RelSelect,
  globalCommandLine,
  globalFill,
  global,
  globalSyntax,
  globalFunction,
) where

import qualified Data.Monoid                            as D
import qualified Data.Version                           as D
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Content           as C
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Relkit     as C


-- ----------------------  Rop

-- | Implementation of relmap operator
data Rop c = Rop
    { ropName     :: String           -- ^ Operator name
    , ropGroup    :: String           -- ^ Operator group
    , ropSorter   :: C.RopFullSorter  -- ^ Operand sorter
    , ropCons     :: RopCons c        -- ^ Constructor of operator
    , ropUsage    :: String           -- ^ Usage of operator
    }

instance Show (Rop c) where
    show Rop { ropName = name, ropGroup = group }
        = "Rop (" ++ group ++ "/" ++ name ++ ")"

-- | Constructor of relmap operator
type RopCons c = RopUse c -> B.Ab (Relmap c)

-- | Use of relmap operator
data RopUse c = RopUse
    { ropGlobal    :: Global c
    , ropHalf      :: C.HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubrelmap :: [Relmap c]     -- ^ Subrelmaps
    } deriving (Show)

instance B.TokenListing (RopUse c) where
    tokenListing = B.tokenListing . ropHalf

getArg1 :: [B.Ab c] -> B.Ab (B.Ab c)
getArg1 [x] = Right x
getArg1 _ = Left $ B.AbortCalc [] $ B.ACUnmatchType []

getArg2 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c)
getArg2 [x, y] = Right (x, y)
getArg2 _ = Left $ B.AbortCalc [] $ B.ACUnmatchType []

getArg3 :: [B.Ab c] -> B.Ab (B.Ab c, B.Ab c, B.Ab c)
getArg3 [x, y, z] = Right (x, y, z)
getArg3 _ = Left $ B.AbortCalc [] $ B.ACUnmatchType []



-- ----------------------  Relmap

-- | Relation-to-relation mapping.
--   A 'Relmap' is correspond to a use of relational operator.
data Relmap c
    = RelmapSource   C.HalfRelmap B.JudgePattern [B.Termname]
      -- ^ Retrieve a relation from a dataset
    | RelmapConst    C.HalfRelmap (B.Rel c)
      -- ^ Constant relation

    | RelmapGlobal   C.HalfRelmap (Global c -> RelkitCalc c)
      -- ^ Relmap that maps relations to a relation with globals
    | RelmapCalc     C.HalfRelmap (RelkitConfl c) [Relmap c]
      -- ^ Relmap that maps relations to a relation

    | RelmapLink     C.HalfRelmap String (Maybe (Relmap c))
      -- ^ Relmap reference
    | RelmapAlias    C.HalfRelmap (Relmap c)
      -- ^ Equavalent relmap
    | RelmapAppend   (Relmap c) (Relmap c)
      -- ^ Connect two relmaps

-- | Map function to relmaps.
mapToRelmap :: B.Map (Relmap c) -> B.Map (Relmap c)
mapToRelmap f = mf where
    mf (RelmapAlias  h r1)          = RelmapAlias  h (mf r1)
    mf (RelmapAppend r1 r2)         = RelmapAppend (mf r1) (mf r2)
    mf (RelmapCalc   h g rs)        = RelmapCalc   h g (map mf rs)
    mf (RelmapLink   h n (Just r1)) = RelmapLink   h n (Just $ mf r1)
    mf r1 = f r1

instance Show (Relmap c) where
    show = showRelmap

showRelmap :: Relmap c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)  = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)     = "RelmapConst "  ++ show (B.name r) ++ " _"

    sh (RelmapGlobal _ _)     = "RelmapGlobal " ++ show (B.name r)
    sh (RelmapCalc   _ _ rs)  = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs

    sh (RelmapLink _ n _)     = "RelmapLink "   ++ show n
    sh (RelmapAlias  _ r2)    = "RelmapAlias "  ++ show r2
    sh (RelmapAppend r1 r2)   = "RelmapAppend"  ++ joinSubs [r1, r2]

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance D.Monoid (Relmap c) where
    mempty  = RelmapCalc halfid (const $ Right . C.relkitId) []
    mappend = RelmapAppend

halfid :: C.HalfRelmap
halfid = C.HalfRelmap (B.tokenWord "id") [("operand", [])] [] "id"

instance B.Name (Relmap c) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  h _)     = C.halfOpText h
    name (RelmapCalc   h _ _)   = C.halfOpText h
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance B.Pretty (Relmap c) where
    doc (RelmapSource h _ _)   = B.doc h
    doc (RelmapConst  h _)     = B.doc h

    doc (RelmapGlobal h _)     = B.doc h -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapCalc   h _ _)   = B.doc h -- hang (text $ name m) 2 (doch (map doc ms))

    doc (RelmapLink   _ n _)   = B.doc n
    doc (RelmapAlias  h _)     = B.doc h
    doc (RelmapAppend m1 m2)   = B.docHang (B.doc m1) 2 (docRelmapAppend m2)

docRelmapAppend :: Relmap c -> B.Doc
docRelmapAppend = B.docv . map pipe . relmapAppendList where
    pipe m = B.doc "|" B.<+> B.doc m

{-| Expand 'RelmapAppend' to list of 'Relmap' -}
relmapAppendList :: Relmap c -> [Relmap c]
relmapAppendList = expand where
    expand (RelmapAppend r1 r2) = expand r1 ++ expand r2
    expand r = [r]

instance B.TokenListing (Relmap c) where
    tokenListing r = B.tokenListing $ relmapHalf r

relmapHalf :: Relmap c -> Maybe C.HalfRelmap
relmapHalf = half where
    half (RelmapSource h _ _)   = Just h
    half (RelmapConst  h _)     = Just h

    half (RelmapGlobal h _)     = Just h
    half (RelmapCalc   h _ _)   = Just h

    half (RelmapLink _ _ _)     = Nothing
    half (RelmapAlias  h _)     = Just h
    half (RelmapAppend m1 _)    = relmapHalf m1



-- ----------------------  Relkit

-- | Make 'C.Relkit' from heading of input relation.
type RelkitCalc c =  B.Relhead -> B.Ab (C.Relkit c)

-- | Make 'C.Relkit' from globals and input heading.
type RelkitGlobal c = Global c -> B.Relhead -> B.Ab (C.Relkit c)

-- | Make 'C.Relkit' from one subrelmap and input heading.
type RelkitBinary c = C.Relkit c -> B.Relhead -> B.Ab (C.Relkit c)

-- | Make 'C.Relkit' from multiple subrelmaps and input heading.
type RelkitConfl c =  [(C.Relkit c)] -> B.Relhead -> B.Ab (C.Relkit c)



-- ----------------------  Global

data Global c = Global
      { globalVersion :: D.Version
      , globalRops    :: [Rop c]
      , globalCops    :: ([C.Cop c], [B.Named B.InfixHeight])
      , globalProgram :: String
      , globalArgs    :: [String]
      , globalJudges  :: [B.Judge c]
      , globalSelect  :: RelSelect c
      }

instance Show (Global c) where
    show Global { globalRops = rops, globalCops = (cops, _) }
        = let nr = length rops
              nc = length cops
          in "Global (" ++ show nr ++ " rops, " ++ show nc ++ " cops)"

-- | Relation selector
type RelSelect c = B.JudgePattern -> [String] -> B.Rel c

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


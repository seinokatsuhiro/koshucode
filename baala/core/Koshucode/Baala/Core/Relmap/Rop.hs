{-# OPTIONS_GHC -Wall #-}

{-| Implementation of relmap operators. -}

module Koshucode.Baala.Core.Relmap.Rop
( -- * Global
  Global (..),
  RelSelect,
  globalCommandLine,
  global,

  -- * Rop
  Rop (..),
  RopUse (..),
  RopCons,

  -- * Relmap
  Relmap (..),
) where

import qualified Data.Monoid                            as M
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Core.Relmap.HalfRelmap as C
import qualified Koshucode.Baala.Core.Relmap.Operand    as C
import qualified Koshucode.Baala.Core.Relmap.Relfy      as C


-- ----------------------  Global

data Global c = Global
      { globalRops    :: [Rop c]
      , globalProgram :: String
      , globalArgs    :: [String]
      , globalJudges  :: [B.Judge c]
      , globalSelect  :: RelSelect c
      }

{-| Relation selector -}
type RelSelect c = B.JudgePattern -> [String] -> B.Rel c

globalCommandLine :: Global c -> [String]
globalCommandLine Global { globalProgram = prog, globalArgs = args }
    = prog : args

global :: Global c
global = Global { globalRops    = []
                , globalProgram = ""
                , globalArgs    = []
                , globalJudges  = []
                , globalSelect  = sel }
    where
      sel _ _ = B.reldee



-- ----------------------  Rop

{-| Implementation of relmap operator -}
data Rop c = Rop
    { ropName       :: String           -- ^ Operator name
    , ropGroup      :: String           -- ^ Operator group
    , ropFullSorter :: C.RopFullSorter  -- ^ Operand sorter
    , ropCons       :: RopCons c        -- ^ Constructor of operator
    , ropUsage      :: String           -- ^ Usage of operator
    }

{-| Constructor of relmap operator -}
type RopCons c = RopUse c -> B.Ab (Relmap c)

{-| Use of relmap operator -}
data RopUse c = RopUse {
      ropHalf      :: C.HalfRelmap   -- ^ Syntactic data of operator use
    , ropSubrelmap :: [Relmap c]     -- ^ Subrelmaps
    } deriving (Show)


-- ----------------------  Relmap

{-| Relation-to-relation mapping.
    A 'Relmap' is correspond to a use of relational operator. -}
data Relmap c
    -- | Retrieve a relation from a dataset
    = RelmapSource C.HalfRelmap B.JudgePattern [B.Termname]
    -- | Constant relation
    | RelmapConst  C.HalfRelmap (B.Rel c)
    -- | Equavalent relmap
    | RelmapAlias  C.HalfRelmap (Relmap c)
    -- | Relmap that maps relations to a relation
    | RelmapCalc   C.HalfRelmap (C.RelmapConflRelfy c) [Relmap c]
    -- | Relmap that maps relations to a relation
    | RelmapGlobal C.HalfRelmap (Global c -> C.RelmapCalcRelfy c)
    -- | Connect two relmaps
    | RelmapAppend (Relmap c) (Relmap c)
    -- | Relmap reference
    | RelmapName   C.HalfRelmap String

instance Show (Relmap c) where
    show = showRelmap

showRelmap :: Relmap c -> String
showRelmap r = sh r where
    sh (RelmapSource _ p xs)  = "RelmapSource " ++ show p ++ " " ++ show xs
    sh (RelmapConst  _ _)     = "RelmapConst "  ++ show (B.name r) ++ " _"
    sh (RelmapAlias  _ r2)    = "RelmapAlias "  ++ show r2
    sh (RelmapCalc   _ _ rs)  = "RelmapCalc "   ++ show (B.name r) ++ " _" ++ joinSubs rs
    sh (RelmapGlobal _ _)     = "RelmapGlobal " ++ show (B.name r)
    sh (RelmapAppend r1 r2)   = "RelmapAppend"  ++ joinSubs [r1, r2]
    sh (RelmapName _ n)       = "RelmapName "   ++ show n

    joinSubs = concatMap sub
    sub r2 = " (" ++ sh r2 ++ ")"

instance M.Monoid (Relmap c) where
    mempty  = RelmapCalc halfid (const C.relfyId) []
    mappend = RelmapAppend

halfid :: C.HalfRelmap
halfid = C.HalfRelmap "id" (B.tokenWord "id") [("operand", [])] []

instance B.Name (Relmap c) where
    name (RelmapSource _ _ _)   = "source"
    name (RelmapConst  h _)     = C.halfOpText h
    name (RelmapCalc   h _ _)   = C.halfOpText h
    name (RelmapAppend _ _)     = "append"
    name _ = undefined

instance B.Pretty (Relmap c) where
    doc (RelmapSource h _ _)   = B.doc h
    doc (RelmapConst  h _)     = B.doc h
    doc (RelmapAlias  h _)     = B.doc h
    doc (RelmapCalc   h _ _)   = B.doc h -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapGlobal h _)     = B.doc h -- hang (text $ name m) 2 (doch (map doc ms))
    doc (RelmapAppend m1 m2)   = B.docHang (B.doc m1) 2 (docRelmapAppend m2)
    doc (RelmapName   _ n)     = B.doc n

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
    half (RelmapAlias  h _)     = Just h
    half (RelmapCalc   h _ _)   = Just h
    half (RelmapGlobal h _)     = Just h
    half (RelmapAppend m1 _)    = relmapHalf m1
    half (RelmapName _ _)       = Nothing


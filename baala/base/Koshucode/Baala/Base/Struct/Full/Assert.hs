{-# OPTIONS_GHC -Wall #-}

-- | Data structure for mapping relation to judges

module Koshucode.Baala.Base.Struct.Full.Assert
( Assert (..)
, affirm, deny
, runAssertJudges
, runAssertDataset
) where
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Struct.Full.Relmap

-- | Affirm or deny a relation.
--   It consists of logical quality, relsign, and relmap.
data Assert v = Assert
    { assertQuality :: Bool
    , assertRelsign :: Relsign
    , assertRelmap  :: Relmap v
    } deriving (Show)

instance Name (Assert v) where
    name (Assert True  _ _) = "affirm"
    name (Assert False _ _) = "deny"

instance Pretty (Assert v) where
    doc a@(Assert _ k m) =
        hang (text (name a) <+> text k) 2 (doc m)

-- | Make affirmed judges from a relation.
affirm :: Relsign -> Relmap v -> Assert v
affirm = Assert True

-- | Make denied judges from a relation.
deny :: Relsign -> Relmap v -> Assert v
deny = Assert False

-- | Calculate assertion list.
runAssertJudges
    :: (Ord v, Nil v)
    => [Assert v]   -- ^ Assertion list
    -> [Judge v]    -- ^ Input judges
    -> AbortOr [Judge v]    -- ^ Output judges
runAssertJudges as = runAssertDataset as . dataset

-- | Calculate assertion list.
runAssertDataset :: (Ord v, Nil v) => [Assert v] -> Dataset v
                 -> AbortOr [Judge v]
runAssertDataset as ds = mapM each as >>= return . concat
    where each (Assert q s m) = do
            let judges (Rel h b) = map (judge h) b
                judge h = Judge q s . zip (headNames h)
            r <- runRelmap ds m reldee
            return $ judges r


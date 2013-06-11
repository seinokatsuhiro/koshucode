{-# OPTIONS_GHC -Wall #-}

--  /P  present term        | pick   /P /P ...
--  /N  new term            | cut    /P /P ...
--  /X  arbitrary term      | rename /N /P ...
--  E   value expression    | reval  /P E ...
--  V   value itself        | val    /N E ...
--  RM  relmap              | meet-share RM /P /P ...

module Koshucode.Baala.Base.Kit.WithName
( withP
, withP2
, withNP
, withN1
, WithTerms
, WithTerms2
) where

import Koshucode.Baala.Base.Data
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

type WithTerms  v = ([String] -> RelmapFun v) -> String -> RelmapFun v
type WithTerms2 v = ([(String, String)] -> RelmapFun v) -> String -> RelmapFun v

-- | Call a relmap function with present term names.
withP :: WithTerms v
withP f ns r =
    case maybeTermNames ns of
      Nothing  -> reldum
      Just ns2 -> f ns2 r

-- | Call a relconfl function with present term names.
withP2 :: ([String] -> [Rel v] -> RelmapFun v) -> String -> [Rel v] -> RelmapFun v
withP2 f ns [rm] r =
    case maybeTermNames ns of
      Nothing  -> reldum
      Just ns2 -> f ns2 [rm] r
withP2 _ _ _ _ = undefined

-- | Call a relmap function with new-and-present term names.
withNP :: WithTerms2 v
withNP f ns r = naming ns where
    naming ns2 =
        case maybeTermNames ns2 of
          Nothing  -> reldum
          Just ns3 -> pairing ns3
    pairing ns2 =
        case maybePairs ns2 of
          Nothing  -> reldum
          Just ns3 -> f ns3 r

-- | Call a relmap function with new term names.
withN1 :: WithTerms v
withN1 = withNk 1

withNk :: Int -> WithTerms v
withNk p f ns r =
    case maybeTermNames ns of
      Nothing  -> reldum
      Just ns2 -> if length ns2 == p
                  then f ns2 r
                  else reldum

maybeTermNames :: String -> Maybe [String]
maybeTermNames = maybeFromEither . termNames . tokenTrees . tokens

maybeFromEither :: Either a b -> Maybe b
maybeFromEither = either (const Nothing) Just

-- unterm :: Token -> Int
-- unterm (TermP [n]) = n
-- unterm t = error $ "unterm: " ++ show t

-- untermn :: Token -> String
-- untermn (TermN [n]) = n
-- untermn t = error $ "unterm: " ++ show t


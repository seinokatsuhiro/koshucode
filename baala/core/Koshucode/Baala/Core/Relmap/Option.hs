{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Assertion options.

module Koshucode.Baala.Core.Relmap.Option
  ( Option,
    OptionContent (..),
    option,
    optionBool,
    optionParse,
  ) where

import qualified Data.Map.Strict                      as Map
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Syntax               as S
import qualified Koshucode.Baala.Data                 as D
import qualified Koshucode.Baala.Data.Message         as Msg
import qualified Koshucode.Baala.Core.Relmap.Message  as Msg


-- | Option type.
type Option c = Map.Map String (OptionContent c)

-- | Content of assertion option.
data OptionContent c
    = OptionBool Bool
    | OptionChar [Char] Char
    | OptionTerms [S.TermName]
      deriving (Show, Eq, Ord)

-- | Option set.
option :: (D.CBool c, D.CText c) => Option c
option = Map.fromList
         [ ("order"    , OptionBool False)
         , ("sep-char" , OptionChar ":|" ':')
         , ("forward"  , OptionTerms [])
         , ("backward" , OptionTerms []) ]

-- | Get boolean option value.
optionBool :: String -> Option c -> Bool
optionBool name opt =
    case Map.lookup name opt of
      Just (OptionBool b) -> b
      _                   -> B.bug "unknown option"

-- | Parse assertion option.
optionParse :: (Eq c, D.CBool c, D.CText c)
  => D.CalcContent c -> [S.Token] -> B.AbMap (Option c)
optionParse calc toks opt =
    do assn <- optionAssn toks
       B.foldM (optionUpdate calc) opt assn

type NamedT a = ((String, [S.TTree]), a)

optionAssn :: [S.Token] -> B.Ab [NamedT [S.TTree]]
optionAssn toks =
    do trees <- S.toTrees toks
       case B.assocBy maybeName trees of
         ([], assoc) -> Right assoc
         _           -> Msg.adlib "extra input"
    where
      maybeName pt@(S.TextLeafRaw _ n) = Just (n, [pt])
      maybeName _ = Nothing

optionUpdate :: (Eq c, D.CBool c, D.CText c)
   => D.CalcContent c -> Option c -> NamedT [S.TTree] -> B.Ab (Option c)
optionUpdate calc opt ((name, pt), trees) =
    Msg.abOption pt $ do
      case Map.lookup name opt of
        Just oc  -> Msg.abOption trees $ upd oc
        Nothing  -> Msg.adlib $ "unknown option: " ++ name
    where
      abc = calc $ S.ttreeGroup trees

      upd (OptionBool    _) = do bool <- D.getBool abc
                                 ins $ OptionBool bool

      upd (OptionChar cs _) = do text <- D.getText abc
                                 case text of
                                   [ch] | elem ch cs -> ins $ OptionChar cs ch
                                   _                 -> Msg.adlib "not one letter"

      upd (OptionTerms _)   = do terms <- D.treesFlatNames trees
                                 ins $ OptionTerms terms
                                                        
      ins oc = Right $ Map.insert name oc opt


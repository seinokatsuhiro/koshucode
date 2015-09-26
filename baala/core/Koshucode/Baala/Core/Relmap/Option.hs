{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Options.

module Koshucode.Baala.Core.Relmap.Option
  ( Option,
    OptionContent (..),
    option,
    optionBool,
    optionParse,
  ) where

import qualified Data.Map.Strict               as Map
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Data          as B
import qualified Koshucode.Baala.Data          as C
import qualified Koshucode.Baala.Core.Message  as Msg


type Option c = Map.Map String (OptionContent c)

data OptionContent c
    = OptionBool Bool
    | OptionChar [Char] Char
    | OptionTerms [B.TermName]
      deriving (Show, Eq, Ord)

option :: (C.CBool c, C.CText c) => Option c
option =
    Map.fromList
           [ ("order"    , OptionBool False)
           , ("sep-char" , OptionChar ":|" ':')
           , ("forward"  , OptionTerms [])
           , ("backward" , OptionTerms []) ]

optionBool :: String -> Option c -> Bool
optionBool name opt =
    case Map.lookup name opt of
      Just (OptionBool b) -> b
      _                   -> B.bug "unknown option"

optionParse :: (Eq c, C.CBool c, C.CText c)
  => C.ContentCalc c -> [B.Token] -> B.AbMap (Option c)
optionParse calc toks opt =
    do assn <- optionAssn toks
       B.foldM (optionUpdate calc) opt assn

type NamedT a = ((String, [B.TTree]), a)

optionAssn :: [B.Token] -> B.Ab [NamedT [B.TTree]]
optionAssn toks =
    do trees <- B.ttrees toks
       case B.assocBy maybeName trees of
         ([], assoc) -> Right assoc
         _           -> Msg.adlib "extra input"
    where
      maybeName pt@(B.TextLeafRaw _ n) = Just (n, [pt])
      maybeName _ = Nothing

optionUpdate :: (Eq c, C.CBool c, C.CText c)
   => C.ContentCalc c -> Option c -> NamedT [B.TTree] -> B.Ab (Option c)
optionUpdate calc opt ((name, pt), trees) =
    Msg.abOption pt $ do
      case Map.lookup name opt of
        Just oc  -> Msg.abOption trees $ upd oc
        Nothing  -> Msg.adlib $ "unknown option: " ++ name
    where
      abc = calc $ B.ttreeGroup trees

      upd (OptionBool    _) = do bool <- C.getBool abc
                                 ins $ OptionBool bool

      upd (OptionChar cs _) = do text <- C.getText abc
                                 case text of
                                   [ch] | elem ch cs -> ins $ OptionChar cs ch
                                   _                 -> Msg.adlib "not one letter"

      upd (OptionTerms _)   = do terms <- mapM C.treeToFlatTerm trees
                                 ins $ OptionTerms terms
                                                        
      ins oc = Right $ Map.insert name oc opt


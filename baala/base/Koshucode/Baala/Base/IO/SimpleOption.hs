{-# OPTIONS_GHC -Wall #-}

-- | Simple functions for parsing command-line options.
--   This module is based on "System.Console.GetOpt".

module Koshucode.Baala.Base.IO.SimpleOption
 ( -- * Data type
   ShortOption, LongOption,
   SimpleOption,
   SimpleOptions,
   SimpleOptionDescr,
   SimpleOption',

   -- * Option type
   flag, opt, req,
   getFlag, getOpt, getReq,

   -- * Predefined option
   help, version,
   printHelp, helpMessage,
   Ver.showVersion,

   -- * Parsing
   ParseResult', ParseResult,
   parse, parseCommand,
 ) where

import qualified Data.Maybe            as Maybe
import qualified Data.Version          as Ver
import qualified System.Console.GetOpt as Opt
import qualified System.Environment    as Env


-- --------------------------------------------  Data type

-- | Single letter options, like @h@ of @-h@.
type ShortOption = Char

-- | Long name options, like @help@ of @--help@.
type LongOption = String

-- | Simple option.
type SimpleOption = SimpleOption' LongOption

-- | Option list of simple option descriptions.
type SimpleOptions = [SimpleOptionDescr]

type SimpleOptionDescr = Opt.OptDescr SimpleOption

data SimpleOption' a
    = SimpleFlag a
    | SimpleOpt  a (Maybe String)
    | SimpleReq  a String
      deriving (Show, Eq, Ord)


-- --------------------------------------------  Option type

-- | Describe option for flag (switch) type.
flag :: [ShortOption] -> [LongOption] -> String -> SimpleOptionDescr
flag short long = Opt.Option short long $ Opt.NoArg simple where
    simple = SimpleFlag $ head long

-- | Describe option with optional one parameter.
opt :: [ShortOption] -> [LongOption] -> String -> String -> SimpleOptionDescr
opt short long name = Opt.Option short long (Opt.OptArg simple name) where
    simple = SimpleOpt $ head long

-- | Describe option with required one parameter.
req :: [ShortOption] -> [LongOption] -> String -> String -> SimpleOptionDescr
req short long name = Opt.Option short long (Opt.ReqArg simple name) where
    simple = SimpleReq $ head long

-- | Get flag of flag-type option.
getFlag :: [SimpleOption] -> LongOption -> Bool
getFlag opts name = or $ map (getFlag1 name) opts

-- | Get parameter list of opt-type option.
getOpt :: [SimpleOption] -> LongOption -> [String]
getOpt opts name = getOpt1 name `Maybe.mapMaybe` opts

-- | Get parameter list of req-type option.
getReq :: [SimpleOption] -> LongOption -> [String]
getReq opts name = getReq1 name `Maybe.mapMaybe` opts

getFlag1 :: (Eq a) => a -> SimpleOption' a -> Bool
getFlag1 name (SimpleFlag n) = (n == name)
getFlag1 _ _ = False

getOpt1 :: (Eq a) => a -> SimpleOption' a -> Maybe String
getOpt1 name (SimpleOpt n (Just arg)) | (n == name) = Just arg
getOpt1 _ _ = Nothing

getReq1 :: (Eq a) => a -> SimpleOption' a -> Maybe String
getReq1 name (SimpleReq n arg) | (n == name) = Just arg
getReq1 _ _ = Nothing


-- --------------------------------------------  Predefined option

-- | Predefined @-h@ or @--help@ option.
help :: SimpleOptionDescr
help = flag "h" ["help"] "Show help message"

-- | Predefined @-V@ or @--version@ option.
version :: SimpleOptionDescr
version = flag "V" ["version"] "Show version number"


-- --------------------------------------------  Parsing

-- | Result of parsing command-line parameters, consist of
--   (1) error message or (2) options and non-option parameters.
type ParseResult' opt = Either [String] ([opt], [String])

-- | String-type parse result.
type ParseResult = ParseResult' SimpleOption

-- | Parse parameters.
parse :: SimpleOptions -> [String] -> ParseResult
parse opts args = case Opt.getOpt Opt.Permute opts args of
               (opts', args', err) | null err  -> Right (opts', args')
                                   | otherwise -> Left err

-- | Parse command-line parameters.
parseCommand :: SimpleOptions -> IO ParseResult
parseCommand opts = do args <- Env.getArgs
                       return $ parse opts args

-- | Print help message.
printHelp :: [String] -> SimpleOptions -> IO ()
printHelp text opts = do putStr $ helpMessage text opts
                         putStrLn ""

-- | Create help message.
helpMessage :: [String] -> SimpleOptions -> String
helpMessage text = Opt.usageInfo (unlines text ++ "OPTION")


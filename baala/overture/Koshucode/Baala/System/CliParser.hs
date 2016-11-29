{-# OPTIONS_GHC -Wall #-}

-- | Parser for command-line interface parameter.
--   This module is based on "System.Console.GetOpt".

module Koshucode.Baala.System.CliParser
 ( -- * Procedure
   -- ** 1. Define options
   -- $Define
   Option, OptionName, OptionLetter, OptionExplain,
   flag, req, opt,
   -- ** 2. Parse comand line
   -- $Parse
   Parsed, parse, parseCommand,
   -- ** 3. Get parameters
   -- $Get
   Para, getFlag,
   getReq, getReqLast,
   getOpt, getOptLast,

   -- * Predefined options
   -- ** --help
   help, helpMessage, printHelp,
   -- ** --version
   version, Ver.showVersion,
 ) where

import qualified Data.Maybe             as May
import qualified Data.Version           as Ver
import qualified System.Console.GetOpt  as Opt
import qualified System.Environment     as Env


-- ============================================  Procedure

-- ---------------------------------  Define optionss

-- $Define
--
-- Define options as a list of 'Option's.
-- Each 'Option' is created using 'flag', 'req', or 'opt'.
-- There are predefined options 'help' and 'version'.
--
-- > import qualified Koshucode.Baala.System.CliParser as Cli
-- >
-- > options :: [Cli.Option]
-- > options =
-- >   [ Cli.help
-- >   , Cli.version
-- >   , Cli.flag ['f'] ["foo"]        "Echo foo"
-- >   , Cli.req  []    ["baz"] "text" "Echo baz and text"
-- >   , Cli.opt  []    ["bar"] "text" "Echo bar or text"
-- >   ]

-- | Option definition.
type Option = Opt.OptDescr Para

-- | Long name options, like @help@ of @--help@.
type OptionName = String

-- | Single letter options, like @h@ of @-h@.
type OptionLetter = Char

-- | Explanation of option.
type OptionExplain = String

-- | Option definition for flag (switch) type.
--
--   > flag ['f'] ["foo"] "Echo foo"
--
flag :: [OptionLetter] -> [OptionName] -> OptionExplain -> Option
flag short long = Opt.Option short long $ Opt.NoArg p where
    p = CliFlag $ head long

-- | Option definition with required one parameter.
--
--   > req [] ["baz"] "text" "Echo baz and text"
--
req :: [OptionLetter] -> [OptionName] -> String -> OptionExplain -> Option
req short long name = Opt.Option short long $ Opt.ReqArg p name where
    p = CliReq $ head long

-- | Option definition with optional one parameter.
--
--   > opt [] ["bar"] "text" "Echo bar or text"
--
opt :: [OptionLetter] -> [OptionName] -> String -> OptionExplain -> Option
opt short long name = Opt.Option short long $ Opt.OptArg p name where
    p = CliOpt $ head long


-- ---------------------------------  Parse command line

-- $Parse
--
-- Parse command-line parameter with defined options.
--
-- > main :: IO ()
-- > main =
-- >   case Cli.parseCommand options of
-- >     Left errs -> Cli.printHelp errs options
-- >     Right (ps, rest) -> ...

-- | Result of parsing command-line parameters, consist of
--   (1) error message or (2) options and non-option parameters.
type Parsed = Either [String] ([Para], [String])

-- | Parse command-line parameters.
parse :: [Option] -> [String] -> Parsed
parse opts args = case Opt.getOpt Opt.Permute opts args of
               (opts', args', err) | null err  -> Right (opts', args')
                                   | otherwise -> Left err

-- | Parse command-line parameters of the current process.
parseCommand :: [Option] -> IO Parsed
parseCommand opts =
    do args <- Env.getArgs
       return $ parse opts args


-- ---------------------------------  Getting parameters

-- $Get
--
-- Get optional parameters using 'getFlag', 'getReq', or 'getOpt'.
--
-- >     Right (ps, rest)
-- >       | Cli.getFlag ps "help"     -> Cli.printHelp ["USAGE", ...] options
-- >       | Cli.getFlag ps "version"  -> ...
-- >       | ...

-- | Actual optional parameter.
data Para
    = CliFlag OptionName
    | CliReq  OptionName String
    | CliOpt  OptionName (Maybe String)
      deriving (Show, Eq, Ord)

-- | Get flag of 'flag'-type option.
--
--   >>> let Right (o,p) = parse [help, version] ["-h", "foo", "bar"]
--   >>> getFlag o "help"
--   True
--   >>> p
--   ["foo","bar"]
--
getFlag :: [Para] -> OptionName -> Bool
getFlag opts name = or $ map get opts where
    get (CliFlag n) = (n == name)
    get _ = False

-- | Get parameter list of 'req'-type option.
--
--   >>> let Right (o,p) = parse [req [] ["a"] "text" "required text"] ["--a=foo", "bar"]
--   >>> getReq o "a"
--   ["foo"]
--   >>> p
--   ["bar"]
--
getReq :: [Para] -> OptionName -> [String]
getReq opts name = May.mapMaybe get opts where
    get (CliReq n p) | (n == name) = Just p
    get _ = Nothing

-- | Get last parameter of 'req'-type option.
getReqLast :: [Para] -> OptionName -> Maybe String
getReqLast z n = maybeLast $ getReq z n

-- | Get parameter list of 'opt'-type option.
--
--   >>> let Right (o,p) = parse [opt [] ["a"] "text" "optional text"] ["--a", "--a=foo", "bar"]
--   >>> getOpt o "a" "default"
--   ["default","foo"]
--   >>> p
--   ["bar"]
--
getOpt :: [Para] -> OptionName -> String -> [String]
getOpt opts name def = May.mapMaybe get opts where
    get (CliOpt n p) | (n == name) = Just $ May.fromMaybe def p
    get _ = Nothing

-- | Get last parameter of 'opt'-type option.
getOptLast :: [Para] -> OptionName -> String -> Maybe String
getOptLast z n def = maybeLast $ getOpt z n def

-- | Head element of list.
-- maybeHead :: [a] -> Maybe a
-- maybeHead []       = Nothing
-- maybeHead (e : _)  = Just e

-- | Last element of list.
maybeLast :: [a] -> Maybe a
maybeLast []       = Nothing
maybeLast [e]      = Just e
maybeLast (_ : es) = maybeLast es


-- ============================================  Predefined option

-- | Predefined @-h@ or @--help@ option.
--
--   > flag "h" ["help"] "Show help message"
--
help :: Option
help = flag "h" ["help"] "Show help message"

-- | Create help message.
helpMessage :: [String] -> [Option] -> String
helpMessage text = Opt.usageInfo (unlines text ++ "OPTION")

-- | Print help message.
printHelp :: [String] -> [Option] -> IO ()
printHelp text opts = do putStr $ helpMessage text opts
                         putStrLn ""

-- | Predefined @-V@ or @--version@ option.
--
--   > flag "V" ["version"] "Show version number"
--
version :: Option
version = flag "V" ["version"] "Show version number"

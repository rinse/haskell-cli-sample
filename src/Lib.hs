module Lib
    ( someFunc
    ) where

import Options.Applicative

-- source <(sample-exe --bash-completion-script $(which sample-exe))

-- サブコマンド全体
data Subcommand
    = SayCmd SayArgs
    deriving (Show)

-- ここで、全てオプションに対してパーサーが充てられているかどうかが保証されない
-- これはextensibleを使えば解決できる
-- subparser :: Mod CommandFields a -> Parser a
subcommandParser :: Parser Subcommand
subcommandParser = subparser $ modSayCmd

-- command :: String -> ParsreInfo a -> Mod CommandFields a
modSayCmd :: Mod CommandFields Subcommand
modSayCmd = command "say" $ SayCmd <$> sayArgsParser `withInfo` "say info"


data SayArgs = SayArgs
    { hello :: String
    , varbose :: Bool
    } deriving (Show)

-- strOption :: IsString s => Mod OptionFields s -> Parser s
-- switch :: Mod FlagFields Bool -> Parser Bool
-- short :: HasName f => Char -> Mod f a
-- long :: HasName f => String -> Mod f a
-- help :: String -> Mod f a
sayArgsParser :: Parser SayArgs
sayArgsParser = SayArgs
    <$> strOption (long "hello" <> help "Says hello to TARGET")
    <*> switch (long "verbose" <> short 'v' <> help "Shows details")

-- info :: Parser a -> InfoMod a -> ParserInfo a
-- helper :: Parser (a -> a)
-- progDesc :: String -> InfoMod a
withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (helper <*> p) . progDesc


runSubcommand :: Subcommand -> IO ()
runSubcommand (SayCmd args) = sayCmd args
runSubcommand _ = print "oops"

-- say module
sayCmd :: SayArgs -> IO ()
sayCmd args = do
    print "sayCmd is runnsing!"
    print args

-- execParser :: ParserInfo a -> IO a
someFunc :: IO ()
someFunc = do
    let options = subcommandParser `withInfo` "Commit a given subcommand"
    execParser options >>= runSubcommand

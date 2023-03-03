-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified Control.Exception as Exc
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import qualified Logger.Impl
import qualified System.Directory as SD
import qualified System.IO

-- | Default config value.
-- Use in EchoBot.Config and Logger.Impl.Config then file config.conf is not found, or fields are invalid
-- Use the console version of the bot (because the token for the telegram version don't have)
configDefault :: ConfigurationTypes.ConfigDefault
configDefault =
  ConfigurationTypes.ConfigDefault
    { ConfigurationTypes.helpReply = "It is a help message",
      ConfigurationTypes.repeatReply =
        "How-many-repeat? Now it is {count} repetition",
      ConfigurationTypes.repetitionCount = 3,
      ConfigurationTypes.stdError = "File",
      ConfigurationTypes.minLogLevel = "Debug",
      ConfigurationTypes.frontEnd = "Console"
    }

-- | Gets the bot config. In any case it can provide reasonable default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  loadedConf <-
    Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $
        "getBotConfig:OK use default bot config. Did not load config file: "
          ++ show exception
      return
        EchoBot.Config
          { EchoBot.confHelpReply =
              T.pack $ ConfigurationTypes.helpReply configDefault,
            EchoBot.confRepeatReply =
              T.pack $ ConfigurationTypes.repeatReply configDefault,
            EchoBot.confRepetitionCount =
              ConfigurationTypes.repetitionCount configDefault
          }
    Right loadedConf' -> makeBotConfig loadedConf'

-- | Create a bot config from a config file. If a value is invalid, then taken it from the default config
makeBotConfig :: C.Config -> IO EchoBot.Config
makeBotConfig conf = do
  confHelpReply <-
    C.lookupDefault
      (ConfigurationTypes.helpReply configDefault)
      conf
      "config.helpReply" ::
      IO String
  confRepeatReply <-
    C.lookupDefault
      (ConfigurationTypes.repeatReply configDefault)
      conf
      "config.repeatReply" ::
      IO String
  confRepetitionCount <-
    C.lookupDefault
      (ConfigurationTypes.repetitionCount configDefault)
      conf
      "config.repetitionCount" ::
      IO Int
  putStrLn "makeBotConfig: OK "
  return $
    EchoBot.Config
      { EchoBot.confHelpReply = T.pack confHelpReply,
        EchoBot.confRepeatReply = T.pack confRepeatReply,
        EchoBot.confRepetitionCount = confRepetitionCount
      }

-- | Gets the Logger config. In any case it can provide reasonable default values.
getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  loadedConf <-
    Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $
        "getLoggerConfig:OK use default logger config. Did not load config file: "
          ++ show exception
      confFileHandle <-
        validateFileHandle (ConfigurationTypes.stdError configDefault)
      confMinLevel <-
        validateLogLevel (ConfigurationTypes.minLogLevel configDefault)
      return
        Logger.Impl.Config
          { Logger.Impl.confFileHandle = confFileHandle,
            Logger.Impl.confMinLevel = confMinLevel
          }
    Right loadedConf' -> makeLogConfig loadedConf'

makeLogConfig :: C.Config -> IO Logger.Impl.Config
makeLogConfig conf = do
  readStdError <-
    C.lookupDefault
      (ConfigurationTypes.stdError configDefault)
      conf
      "config.stdError" ::
      IO String
  readMinLogLevel <-
    C.lookupDefault
      (ConfigurationTypes.minLogLevel configDefault)
      conf
      "config.minLogLevel" ::
      IO String
  confFileHandle <- validateFileHandle readStdError
  confMinLevel <- validateLogLevel readMinLogLevel
  putStrLn "makeLogConfig: OK"
  return
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = confFileHandle,
        Logger.Impl.confMinLevel = confMinLevel
      }

validateFileHandle :: String -> IO System.IO.Handle
validateFileHandle fileText =
  case fileText of
    "File" -> appendLog "logs.txt"
    "Terminal" -> return System.IO.stderr
    _ -> do
      putStrLn
        "validateFileHandle: stdError is invalid in config.conf file. Use stdError from the default config"
      if ConfigurationTypes.stdError configDefault == "File"
        then appendLog "logs.txt"
        else return System.IO.stderr

-- | appendLog  - check the existence of the file, if it does't  exist, create and append
appendLog :: FilePath -> IO System.IO.Handle
appendLog path = do
  rez <- SD.doesFileExist path
  if rez
    then System.IO.openFile "logs.txt" System.IO.AppendMode
    else do
      putStrLn "Create the file logs.txt"
      System.IO.writeFile "logs.txt" []
      System.IO.openFile "logs.txt" System.IO.AppendMode

validateLogLevel :: String -> IO Logger.Level
validateLogLevel levelText =
  case levelText of
    "Error" -> return Logger.Error
    "Warning" -> return Logger.Warning
    "Info" -> return Logger.Info
    "Debug" -> return Logger.Debug
    _ -> do
      putStrLn
        "validateLogLevel: minLogLevel  is invalid in config.conf file. Use logLevel from the default config"
      validateLogLevelFromConfigDefault
        (ConfigurationTypes.minLogLevel configDefault)

-- | validateLogLevelFromConfigDefault - read values from the default config
validateLogLevelFromConfigDefault :: String -> IO Logger.Level
validateLogLevelFromConfigDefault "Error" = return Logger.Error
validateLogLevelFromConfigDefault "Warning" = return Logger.Warning
validateLogLevelFromConfigDefault "Info" = return Logger.Info
validateLogLevelFromConfigDefault _ = return Logger.Debug

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  loadedConf <-
    Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $
        "getFrontEndType:OK use default FrontEnd type config - Console. Did not load config file: "
          ++ show exception
      return ConfigurationTypes.ConsoleFrontEnd
    Right loadedConf' -> makeFrontEndTypeConfig loadedConf'

makeFrontEndTypeConfig :: C.Config -> IO ConfigurationTypes.FrontEndType
makeFrontEndTypeConfig conf = do
  readFrontEndType <-
    C.lookupDefault
      (ConfigurationTypes.frontEnd configDefault)
      conf
      "config.frontEnd" ::
      IO String
  case readFrontEndType of
    "Console" -> do
      putStrLn "makeFrontEndTypeConfig: OK"
      return ConfigurationTypes.ConsoleFrontEnd
    "Telegram" -> do
      readToken <- C.lookup conf "config.token"
      case readToken of
        Nothing -> do
          putStrLn
            "makeFrontEndTypeConfig: Can not read token! from field config.token! Therefore run console bot"
          return ConfigurationTypes.ConsoleFrontEnd
        Just token -> do
          putStrLn $
            "makeFrontEndTypeConfig: OK, token from config file is " ++ token
          return
            ConfigurationTypes.TelegramFrontEnd
              { ConfigurationTypes.token = token
              }
    _ -> do
      putStrLn
        "makeFrontEndTypeConfig: Invalid FrontEndType.Therefore run console bot"
      return ConfigurationTypes.ConsoleFrontEnd

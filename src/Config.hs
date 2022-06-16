{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
    getToken,
    getRepetitionCount,
    getHelpReply,
    getRepeatReply,
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
import qualified System.IO

-- | Default config value.
-- Use in EchoBot.Config then file config.conf is not found, or fields are invalid
-- Use in Logger.Impl.Config then file config.conf is not found, but fields are invalid
configDefault :: ConfigurationTypes.ConfigDefault
configDefault =
  ConfigurationTypes.ConfigDefault
    { ConfigurationTypes.helpReply = "It is a help message",
      ConfigurationTypes.repeatReply = "How-many-repeat? Now it is {count} repetition",
      ConfigurationTypes.repetitionCount = 3,
      ConfigurationTypes.stdError = "F",
      ConfigurationTypes.minLogLevel = "D"
    }

-- | configFrontEndTypeDefault used then config.conf is not found, or token's field is invalid
configFrontEndTypeDefault :: ConfigurationTypes.FrontEndType
configFrontEndTypeDefault = ConfigurationTypes.ConsoleFrontEnd

-- | Gets the bot config. In any case it can provide reasonable default values.
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  loadedConf <- Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $ "getBotConfig: try to getBotConfig, but did not load config file. Fault:  " ++ show exception
      putStrLn "getBotConfig: use Default Bot Config "
      return
        EchoBot.Config
          { EchoBot.confHelpReply = T.pack $ ConfigurationTypes.helpReply configDefault,
            EchoBot.confRepeatReply = T.pack $ ConfigurationTypes.repeatReply configDefault,
            EchoBot.confRepetitionCount = ConfigurationTypes.repetitionCount configDefault
          }
    Right loadedConf' -> do
      putStrLn "getBotConfig:: try to get from file"
      --C.display loadedConf'
      makeBotConfig loadedConf'

-- | Сreate a bot config from a config file. If a value is invalid, then taked it from the default config
makeBotConfig :: C.Config -> IO EchoBot.Config
makeBotConfig conf = do
  confHelpReply <- C.lookupDefault (ConfigurationTypes.helpReply configDefault) conf "config.helpReply" :: IO String
  confRepeatReply <- C.lookupDefault (ConfigurationTypes.repeatReply configDefault) conf "config.repeatReply" :: IO String
  confRepetitionCount <- C.lookupDefault (ConfigurationTypes.repetitionCount configDefault) conf "config.repetitionCount" :: IO Int
  return $
    EchoBot.Config
      { EchoBot.confHelpReply = T.pack confHelpReply,
        EchoBot.confRepeatReply = T.pack confRepeatReply,
        EchoBot.confRepetitionCount = confRepetitionCount
      }

-- | Gets the Logger config. In any case it can provide reasonable default values.
getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  loadedConf <- Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $ "getLoggerConfig: try to getLoggerConfig. Config file did not load. Fault:  " ++ show exception
      putStrLn "getLoggerConfig: use default Logger Config "
      confFileHandle <- validatefileHandle (ConfigurationTypes.stdError configDefault)
      confMinLevel <- validateLogLevel (ConfigurationTypes.minLogLevel configDefault)
      return
        Logger.Impl.Config
          { Logger.Impl.confFileHandle = confFileHandle,
            Logger.Impl.confMinLevel = confMinLevel
          }
    Right loadedConf' -> do
      putStrLn "getLoggerConfig: try to get from file"
      makeLogConfig loadedConf'

makeLogConfig :: C.Config -> IO Logger.Impl.Config
makeLogConfig conf = do
  readStdError <- C.lookupDefault (ConfigurationTypes.stdError configDefault) conf "config.stdError" :: IO String
  readMinLogLevel <- C.lookupDefault (ConfigurationTypes.minLogLevel configDefault) conf "config.minLogLevel" :: IO String
  confFileHandle <- validatefileHandle readStdError
  confMinLevel <- validateLogLevel readMinLogLevel
  return
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = confFileHandle,
        Logger.Impl.confMinLevel = confMinLevel
      }

validatefileHandle :: String -> IO System.IO.Handle
validatefileHandle fileText =
  case fileText of
    "F" -> do System.IO.openFile "logs" System.IO.AppendMode
    "C" -> return System.IO.stderr
    _ -> do
      putStrLn "validatefileHandle: stdError is invalid in config.conf file"
      return System.IO.stderr

validateLogLevel :: String -> IO Logger.Level
validateLogLevel levelText =
  case levelText of
    "E" -> return Logger.Error
    "W" -> return Logger.Warning
    "I" -> return Logger.Info
    "D" -> return Logger.Debug
    _ -> do
      putStrLn "validateLogLevel: minLogLevel  is invalid in config.conf file"
      return Logger.Info

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  loadedConf <- Exc.try $ C.load [C.Required "config.conf"] :: IO (Either Exc.IOException C.Config)
  case loadedConf of
    Left exception -> do
      putStrLn $ "getFrontEndType: try to getFrontEndType. Config file did not load. Fault:  " ++ show exception
      putStrLn "getFrontEndType: use Default FrontEndType config -  console "
      return configFrontEndTypeDefault
    Right loadedConf' -> do
      putStrLn "getFrontEndType: Function getFrontEndType loaded config from file!"
      --putStrLn $ "getFrontEndType: Config here:"
      --C.display loadedConf'
      makeFrontEndTypeConfig loadedConf'

makeFrontEndTypeConfig :: C.Config -> IO ConfigurationTypes.FrontEndType
makeFrontEndTypeConfig conf = do
  readFrontEndType <- C.lookupDefault (ConfigurationTypes.frontEnd configDefault) conf "config.frontEnd" :: IO String
  case readFrontEndType of
    "C" -> return ConfigurationTypes.ConsoleFrontEnd
    "T" -> do
      readToken <- C.lookup conf "config.token" :: IO (Maybe String)
      case readToken of
        Nothing -> do
          putStrLn "makeFrontEndTypeConfig: Can not read token! from field config.token! Therefore run console bot"
          return ConfigurationTypes.ConsoleFrontEnd
        Just token -> do
          putStrLn $ "makeFrontEndTypeConfig: token from config file is " ++ token
          return ConfigurationTypes.TelegramFrontEnd {ConfigurationTypes.token = token}
    _ -> do
      putStrLn "makeFrontEndTypeConfig: Invalid FrontEndType."
      return ConfigurationTypes.ConsoleFrontEnd

-- | help functions for telegram bot
getToken :: IO String
getToken = ConfigurationTypes.token <$> getFrontEndType

getRepetitionCount :: IO Int
getRepetitionCount = EchoBot.confRepetitionCount <$> getBotConfig

getHelpReply :: IO T.Text
getHelpReply = EchoBot.confHelpReply <$> getBotConfig

getRepeatReply :: IO T.Text
getRepeatReply = EchoBot.confRepeatReply <$> getBotConfig

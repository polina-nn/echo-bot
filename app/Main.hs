{-# LANGUAGE LambdaCase #-}

module Main
  ( main,
  )
where

import qualified Config
import qualified ConfigurationTypes
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram
import qualified FrontEnd.TelegramTypes as TgTypes
import qualified Logger
import qualified Logger.Impl
import System.Exit (die)

main :: IO ()
main = do
  putStrLn "My bot started"
  withLogHandle $ \logHandle -> do
    frontEnd <- Config.getFrontEndType
    case frontEnd of
      ConfigurationTypes.TelegramFrontEnd _ -> do
        botHandle <- makeBotHandleForTelegram logHandle
        runTelegramFrontEnd botHandle
      ConfigurationTypes.ConsoleFrontEnd -> do
        botHandle <- makeBotHandleForPlainText logHandle
        runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: EchoBot.Handle IO TgTypes.Content -> IO ()
runTelegramFrontEnd botHandle' =
  FrontEnd.Telegram.run TgTypes.Handle {TgTypes.hBotHandle = botHandle'}

withLogHandle :: (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle f = do
  config <- Config.getLoggerConfig
  Logger.Impl.withHandle config f

-- | Creates a bot handle. Please note:
--
-- * a handle holds a reference to an 'IORef' with a 'EchoBot.State',
--   so that it can only keep state of a single user. In order to
--   support multiple users in a chat, you should create a new handle
--   for each user and probably keep them in a 'Data.Map' keyed by a
--   user id.
--
-- * 'EchoBot.Handle' is parameterized with the 'Text' type, so that
--   it supports only plain text messages suitable for the console.
--   When implementing Telegram or another multimedia chat support,
--   you should create a similar function, but parameterized with
--   another message type which can represent either text or
--   multimedia messages. You will need to specify different functions
--   @hMessageFromText@ and @hTextFromMessage@.
makeBotHandleForTelegram :: Logger.Handle IO -> IO (EchoBot.Handle IO TgTypes.Content)
makeBotHandleForTelegram logHandle = do
  botConfig <- Config.getBotConfig
  stateRef <- makeStateRef
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = logHandle,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage =
          \case
            TgTypes.ValidMessage text -> Just text
            TgTypes.ErrorMessage text -> Just text
            TgTypes.ErrorAPITelegram text -> Just text
            TgTypes.Sticker text -> Just text,
        EchoBot.hMessageFromText = TgTypes.ValidMessage
      }

makeBotHandleForPlainText :: Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- Config.getBotConfig
  stateRef <- makeStateRef
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = logHandle,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage = Just,
        EchoBot.hMessageFromText = id
      }

makeStateRef :: IO (IORef EchoBot.State)
makeStateRef = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  newIORef initialState

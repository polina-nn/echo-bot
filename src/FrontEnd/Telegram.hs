-- | The telegram front-end is responsible for telegram I/O
module FrontEnd.Telegram
  ( run,
  )
where

import qualified Control.Exception.Safe as EX
import Data.IORef (IORef, newIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramAPI as TgAPI
import qualified FrontEnd.TelegramTypes as TgTypes
import Logger (logDebug, logError, (.<))
import qualified System.Exit as Exit

run :: TgTypes.Handle -> IO ()
run h = do
  _ <- EX.catch (TgAPI.getMeTg h) (handleException h)
  let lastUpdateId = Nothing
  mapRepeats <- newIORef (Map.empty :: TgTypes.TgRepeats)
  mainLoop h lastUpdateId mapRepeats

-- | mainLoop --
-- | I use common pure logic for all types of bots from the module EchoBot.
-- | I use a common handle and a map of the same handles for each chatId. The common handle is needed as an initializing handle template for each user (chat Id)
-- 1. Polling telegrams using "getUpdates" method using timeout and offset by 1.
--    Use TgAPI.getTgUpdates and TgAPI.tgGetLastUpdateAndId  functions  while a non-empty update is received.)
-- 2. If a non-empty update is received find an message or callback (from inline keyboard) with user chart ID (where did the message come from)
--   Use TgAPI.tgGetLastMessageOrCallback function.
-- 3. Then check event from message or callback (TgAPI.checkEvent). List of common events defined at module EchoBot.
-- 4. Then find current Chat Handle (TgAPI.chooseCurrentChatHandle). If the user is new (chat Id), a handle is created for him from the template.
--    If the user called the keyboard, but did not press a button on it, an empty callback comes, so we go into the mainloop
-- 5. In EchoBot.respond use current Chat Handle and return array of [Response Content] (See data Response in the module EchoBot)
-- 6. In TgAPI.sendTgResponse send real messages in telegram
-- 7. And so on
mainLoop ::
  TgTypes.Handle -> -- General Handle for telegram bot: Handle = Handle { hBotHandle :: EchoBot.Handle IO Content, hToken :: Token, hTemplateBotConfig :: EchoBot.Config }
  Maybe TgTypes.UpdateId -> -- Last Update from telegram "getUpdates"
  IORef TgTypes.TgRepeats -> -- Map of the handles for each chatId : TgRepeats = Map.Map ChatId Handle
  IO ()
mainLoop h@TgTypes.Handle {..} lastUpdateId mapRepeats = do
  maybeTgUpdate <-
    EX.catch (TgAPI.getTgUpdates hToken lastUpdateId) (handleException h)
  let lastUpdateAndId' = TgAPI.tgGetLastUpdateAndId maybeTgUpdate
  case lastUpdateAndId' of
    Nothing -> do
      Logger.logDebug (EchoBot.hLogHandle (TgTypes.hBotHandle h)) "mainLoop:  empty update"
      mainLoop h lastUpdateId mapRepeats
    Just (lastUpdate', lastUpdateId') -> do
      let messageOrCallback = TgAPI.tgGetLastMessageOrCallback lastUpdate'
          event = TgAPI.checkEvent messageOrCallback
      maybeCurrentChatHandle <-
        TgAPI.chooseCurrentChatHandle h messageOrCallback mapRepeats
      case maybeCurrentChatHandle of
        Nothing -> mainLoop h (Just lastUpdateId') mapRepeats
        Just currentChatHandle -> do
          response <-
            EchoBot.respond (TgTypes.hBotHandle currentChatHandle) event
          Logger.logDebug (EchoBot.hLogHandle (TgTypes.hBotHandle h)) $ "mainLoop: response  " .< response
          EX.catch
            (TgAPI.sendTgResponse currentChatHandle messageOrCallback response)
            (handleException h)
          Logger.logDebug
            (EchoBot.hLogHandle (TgTypes.hBotHandle h))
            ( T.append "mainLoop: message sent. Update number " $
                T.pack $ show lastUpdateId'
            )
          mainLoop h (Just lastUpdateId') mapRepeats

handleException :: TgTypes.Handle -> EX.SomeException -> IO a
handleException h (EX.SomeException e) = do
  Logger.logError (EchoBot.hLogHandle (TgTypes.hBotHandle h)) $ "handleError catch SomeException: ERROR! " .< e
  Exit.exitFailure

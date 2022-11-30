{-# LANGUAGE OverloadedStrings #-}

-- | The telegram front-end is responsible for telegram I/O
module FrontEnd.Telegram
  ( run,
  )
where

import qualified Config
import Data.IORef (IORef, newIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramAPI as TgAPI
import qualified FrontEnd.TelegramTypes as TgTypes
import qualified Logger

run :: TgTypes.Handle -> IO ()
run h = do
  token <- Config.getToken
  bot <- TgAPI.getMeTg token
  case bot of
    Left getMeTgError ->
      Logger.logError
        (EchoBot.hLogHandle (TgTypes.hBotHandle h))
        (T.append "Telegramm bot can not run" $ T.pack getMeTgError)
    Right _ ->
      Logger.logDebug
        (EchoBot.hLogHandle (TgTypes.hBotHandle h))
        (T.append "run: Telegramm bot is running" $ T.pack $ show bot)
  let lastUpdateId = Nothing
  mapRepeats <- newIORef (Map.empty :: TgTypes.TgRepeats)
  mainLoop h token lastUpdateId mapRepeats

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
  TgTypes.Handle -> -- General Handle for telegram bot: Handle = Handle { hBotHandle :: EchoBot.Handle IO Content}
  TgTypes.Token -> -- Telegramm token
  Maybe TgTypes.UpdateId -> -- Last Update from telegram "getUpdates"
  IORef TgTypes.TgRepeats -> -- Map of the handles for each chatId : TgRepeats = Map.Map ChatId Handle
  IO ()
mainLoop h token lastUpdateId mapRepeats = do
  maybeTgUpdate <- TgAPI.getTgUpdates token lastUpdateId
  let lastUpdateAndId' = TgAPI.tgGetLastUpdateAndId maybeTgUpdate
  case lastUpdateAndId' of
    Nothing -> do
      Logger.logDebug
        (EchoBot.hLogHandle (TgTypes.hBotHandle h))
        (T.pack "mainLoop:  empty update")
      mainLoop h token lastUpdateId mapRepeats
    Just (lastUpdate', lastUpdateId') -> do
      let messageOrCallback = TgAPI.tgGetLastMessageOrCallback lastUpdate'
          event = TgAPI.checkEvent messageOrCallback
      maybeCurrentChatHandle <-
        TgAPI.chooseCurrentChatHandle
          (TgTypes.hBotHandle h)
          messageOrCallback
          mapRepeats
      case maybeCurrentChatHandle of
        Nothing -> mainLoop h token (Just lastUpdateId') mapRepeats
        Just currentChatHandle -> do
          response <-
            EchoBot.respond (TgTypes.hBotHandle currentChatHandle) event
          Logger.logDebug
            (EchoBot.hLogHandle (TgTypes.hBotHandle h))
            (T.append "mainLoop: response  " $ T.pack $ show response)
          TgAPI.sendTgResponse
            (TgTypes.hBotHandle currentChatHandle)
            token
            messageOrCallback
            response
          Logger.logDebug
            (EchoBot.hLogHandle (TgTypes.hBotHandle h))
            ( T.append "mainLoop: message sent. Update number " $
                T.pack $ show lastUpdateId'
            )
          mainLoop h token (Just lastUpdateId') mapRepeats

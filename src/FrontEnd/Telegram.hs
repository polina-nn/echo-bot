{-# LANGUAGE OverloadedStrings #-}

-- | The telegram front-end is responsible for telegram I/O
module FrontEnd.Telegram
  ( run,
    Handle (..),
  )
where

import qualified Config
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramAPI as TgAPI
import qualified FrontEnd.TelegramTypes as TgTypes
import qualified Logger

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run h = do
  token <- Config.getToken
  bot <- TgAPI.getMeTg token
  case bot of
    Left getMeTgError ->
      Logger.logError (EchoBot.hLogHandle (hBotHandle h)) (T.append "Telegramm bot can not run" $ T.pack getMeTgError)
    Right _ ->
      Logger.logDebug (EchoBot.hLogHandle (hBotHandle h)) (T.append "run: Telegramm bot is running" $ T.pack $ show bot)
  let lastUpdateId = Nothing
  mainLoop h token lastUpdateId

mainLoop :: Handle -> TgTypes.Token -> Maybe Int -> IO ()
mainLoop h token lastUpdateId = do
  maybeTgUpdate <- TgAPI.getTgUpdates token lastUpdateId
  let lastUpdateId' = TgAPI.tgGetLastUpdateId maybeTgUpdate :: Maybe Int
  case lastUpdateId' of
    Nothing -> do
      Logger.logDebug
        (EchoBot.hLogHandle (hBotHandle h))
        (T.pack "mainLoop:  empty update")
      mainLoop h token lastUpdateId'
    _ ->
      do
        TgAPI.sendTgAnswer (hBotHandle h) token maybeTgUpdate
        Logger.logDebug
          (EchoBot.hLogHandle (hBotHandle h))
          ( T.append "serverTg: message sent. Update number " $
              T.pack $ show lastUpdateId'
          )
        mainLoop h token lastUpdateId'

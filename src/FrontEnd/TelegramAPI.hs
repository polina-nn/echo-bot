{-# LANGUAGE OverloadedStrings #-}

-- | TelegramAPI has function for http requests to API Telegram  (type IO)
-- and some service functions for return required record fields of TgUpdate, TgMessage
module FrontEnd.TelegramAPI
  ( getMeTg,
    sendTgAnswer,
    getTgUpdates,
    tgGetLastUpdateId,
  )
where

import qualified Config
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramRepeats as TgRepeats
import qualified FrontEnd.TelegramTypes as TgTypes
import qualified Logger
import qualified Network.HTTP.Req as Req

-- | buildRequestParams -- create QueryString for request
buildRequestParams :: (Req.QueryParam p, Monoid p) => [(T.Text, T.Text)] -> p
buildRequestParams [] = mempty
buildRequestParams params = mconcat $ fmap (uncurry (Req.=:)) params

-- | tgGetLastUpdateId -- returns number of last Update, then you call getUpdates
tgGetLastUpdateId :: Maybe [TgTypes.TgUpdate] -> Maybe Int
tgGetLastUpdateId Nothing = Nothing -- Update is empty
tgGetLastUpdateId (Just []) = Nothing
tgGetLastUpdateId (Just results) = Just (tgGetLastUpdateId' (last results)) -- take last element of lastUpdateId
  where
    tgGetLastUpdateId' :: TgTypes.TgUpdate -> Int
    tgGetLastUpdateId' TgTypes.TgUpdate {TgTypes.tgUpdateUpdateId = c} = c

-- | getMessageOrCallbackQuery get a structure that contains either Message or CallbackQuery
-- if structure TgTypes.TgUpdate changes getMessageOrCallbackQuery return error - TgTypes.UpdateMessageOrCallbackQueryError
getMessageOrCallbackQuery :: TgTypes.TgUpdate -> TgTypes.UpdateMessageOrCallbackQuery
getMessageOrCallbackQuery (TgTypes.TgUpdate _ (Just ms) _ _ _ _) = TgTypes.UpdateMessage ms
getMessageOrCallbackQuery (TgTypes.TgUpdate _ _ (Just ms) _ _ _) = TgTypes.UpdateMessage ms
getMessageOrCallbackQuery (TgTypes.TgUpdate _ _ _ (Just ms) _ _) = TgTypes.UpdateMessage ms
getMessageOrCallbackQuery (TgTypes.TgUpdate _ _ _ _ (Just ms) _) = TgTypes.UpdateMessage ms
getMessageOrCallbackQuery (TgTypes.TgUpdate _ _ _ _ _ (Just ms)) = TgTypes.UpdateCallbackQuery ms
getMessageOrCallbackQuery (TgTypes.TgUpdate _ Nothing Nothing Nothing Nothing Nothing) = TgTypes.UpdateMessageOrCallbackQueryError

-- | getMeTg simple method for testing your bot's authentication token
-- If token is invalid, getMeTg send  Exception from http request!
getMeTg :: TgTypes.Token -> IO (Either TgTypes.TgError TgTypes.TgUser)
getMeTg t =
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Req.req
        Req.GET
        (Req.https "api.telegram.org" Req./: T.pack ("bot" ++ t) Req./: "getMe")
        Req.NoReqBody
        Req.jsonResponse
        mempty
    let responsebody = Req.responseBody response :: A.Value
    case Data.Aeson.Types.parseEither A.parseJSON responsebody of
      Left errorMessage -> return $ Left errorMessage
      Right result -> return $ Right $ TgTypes.tgGetMeResponseResult result

-- | getTgUpdates -- get updates. If it return IO Nothig -- last TgUpdate is empty
-- when you call getTgUpdates with (Just lastUpdateId),
-- you call whith lastUpdateId+1, because you are waiting new update.
getTgUpdates :: TgTypes.Token -> Maybe TgTypes.UpdateId -> IO (Maybe [TgTypes.TgUpdate])
getTgUpdates token Nothing = getTgUpdatesHelp token [] -- это  params = mempty
getTgUpdates token (Just lastUpdateId) = getTgUpdatesHelp token [("offset", T.pack $ show $ lastUpdateId + 1), ("timeout", T.pack $ show (50 :: Int))]

getTgUpdatesHelp :: TgTypes.Token -> [(T.Text, T.Text)] -> IO (Maybe [TgTypes.TgUpdate])
getTgUpdatesHelp token params =
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Req.req
        Req.GET
        ( Req.https "api.telegram.org" Req./: T.pack ("bot" ++ token)
            Req./: "getUpdates"
        )
        Req.NoReqBody
        Req.jsonResponse
        (buildRequestParams params)
    return $
      TgTypes.tgGetUpdateResponseBodyResult
        <$> Data.Aeson.Types.parseMaybe
          A.parseJSON
          (Req.responseBody response :: A.Value)

-- | sendTgAnswer call with  Maybe [TgUpdate],
-- but if Maybe [TgUpdate]==Nothing, sendTgAnswer don't call in mainloop!
sendTgAnswer :: EchoBot.Handle IO T.Text -> TgTypes.Token -> Maybe [TgTypes.TgUpdate] -> IO ()
sendTgAnswer h _ Nothing = do
  Logger.logError (EchoBot.hLogHandle h) "sendAnswer: argument TgUpdate ==  Nothing "
  return ()
sendTgAnswer h token (Just tgUpdates) = do
  let lastTgUpdate = last tgUpdates :: TgTypes.TgUpdate -- it is last update
  sendTgAnswerEcho h token (getMessageOrCallbackQuery lastTgUpdate)

-- | sendTgAnswerEcho - help function for sendTgAnswer,
-- depending on the types of messages -  messages, keyboard, or Error message for TgTypes.UpdateMessageOrCallbackQueryError
sendTgAnswerEcho :: EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.UpdateMessageOrCallbackQuery -> IO ()
sendTgAnswerEcho h token (TgTypes.UpdateMessage tgMessage) = sendTgAnswerSomeTypeOfMessage h token tgMessage
sendTgAnswerEcho h token (TgTypes.UpdateCallbackQuery tgCallbackQuery) = sendTgAnswerCallbackQuery h token tgCallbackQuery
sendTgAnswerEcho h _ TgTypes.UpdateMessageOrCallbackQueryError = do
  Logger.logError (EchoBot.hLogHandle h) "sendAnswer: getMessageOrCallbackQuery: Please see TgUpdate! in API telegram "
  return ()

-- | sendTgAnswerSomeTypeOfMessage -- help function for sendTgAnswerEcho for different types of content
-- if it is not stiker or text, bot send message: "Unknown content type!"
sendTgAnswerSomeTypeOfMessage :: EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.TgMessage -> IO ()
sendTgAnswerSomeTypeOfMessage conf token (TgTypes.TgMessage (Just text) _ _ _ _ _ _ mesChat _ _) = sendTgMessage conf token (T.pack text) (TgTypes.tgChatId mesChat :: TgTypes.ChatId)
sendTgAnswerSomeTypeOfMessage conf token (TgTypes.TgMessage _ (Just stiker) _ _ _ _ _ mesChat _ _) = sendTgStiker conf token stiker (TgTypes.tgChatId mesChat :: TgTypes.ChatId)
sendTgAnswerSomeTypeOfMessage conf token (TgTypes.TgMessage _ _ _ _ _ _ _ mesChat _ _) = sendTgMessage conf token (T.pack "Unknown content type!") (TgTypes.tgChatId mesChat)

sendTgStiker :: EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.TgSticker -> TgTypes.ChatId -> IO ()
sendTgStiker h token stiker chatId = do
  Logger.logDebug (EchoBot.hLogHandle h) "sendTgStiker call"
  rep <- TgRepeats.readTgRepeats chatId
  sendTgMessageHelp rep token "sendSticker" [("chat_id", T.pack $ show chatId), ("sticker", T.pack $ TgTypes.tgStickerFileId stiker)]

sendTgMessage :: EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.Message -> TgTypes.ChatId -> IO ()
sendTgMessage h token "/repeat" chatId = do
  Logger.logDebug (EchoBot.hLogHandle h) "sendTgMessage: answer for /repeat comand"
  sendTgKeyboard h token chatId
sendTgMessage h token "/help" chatId = do
  Logger.logDebug (EchoBot.hLogHandle h) "sendTgMessage: answer for /help comand"
  helpMess <- Config.getHelpReply
  sendTgMessageHelp 1 token "sendMessage" [("chat_id", T.pack $ show chatId), ("text", helpMess)]
sendTgMessage h token mess chatId = do
  Logger.logDebug (EchoBot.hLogHandle h) "sendTgMessage: echo answer"
  rep <- TgRepeats.readTgRepeats chatId
  sendTgMessageHelp rep token "sendMessage" [("chat_id", T.pack $ show chatId), ("text", mess)]

----------------------------------------------------------------------------
sendTgMessageHelp ::
  TgRepeats.Repeats -> TgTypes.Token -> T.Text -> [(T.Text, T.Text)] -> IO ()
sendTgMessageHelp repeats token url params =
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Control.Monad.replicateM repeats $
        Req.req
          Req.POST
          (Req.https "api.telegram.org" Req./: T.pack ("bot" ++ token) Req./: url)
          Req.NoReqBody
          Req.jsonResponse
          (buildRequestParams params)
    Control.Monad.IO.Class.liftIO $ print (Req.responseBody $ head response :: A.Value)

sendTgKeyboard ::
  EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.ChatId -> IO ()
sendTgKeyboard h token chatId = do
  Logger.logDebug (EchoBot.hLogHandle h) "sendTgKeyboard: call"
  let keyboard :: TgTypes.TgInlineKeyboardMarkup
      keyboard =
        TgTypes.TgInlineKeyboardMarkup
          [ [TgTypes.TgInlineKeyboardButton "1" "1"],
            [TgTypes.TgInlineKeyboardButton "2" "2"],
            [TgTypes.TgInlineKeyboardButton "3" "3"],
            [TgTypes.TgInlineKeyboardButton "4" "4"],
            [TgTypes.TgInlineKeyboardButton "5" "5"]
          ]
      a = BC.unpack (A.encode keyboard)
  repeatMes <- Config.getRepeatReply
  sendTgMessageHelp 1 token "sendMessage" [("chat_id", T.pack $ show chatId), ("text", repeatMes), ("reply_markup", T.pack a)]

sendTgAnswerCallbackQuery :: EchoBot.Handle IO T.Text -> TgTypes.Token -> TgTypes.TgCallbackQuery -> IO ()
sendTgAnswerCallbackQuery h token callbackQuery =
  -- send answerCallbackQuery, its user sees at the top of the window
  do
    sendTgMessageHelp
      1
      token
      "answerCallbackQuery"
      [ ("callback_query_id", T.pack $ TgTypes.tgCallbackQueryId callbackQuery),
        ("text", T.pack "Your reply has been received")
      ]
    -- remove the keyboard and tell the user the number of selected repetitions
    case TgTypes.tgCallbackQueryMessage callbackQuery :: Maybe TgTypes.TgMessage of
      Nothing -> do
        Logger.logError (EchoBot.hLogHandle h) "sendTgAnswerCallbackQuery: passed empty message to callbackQuery"
        return ()
      Just tgMessage -> do
        let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMessage
            messageId = TgTypes.tgMessageMessageId tgMessage
        case TgTypes.tgCallbackQueryData callbackQuery :: Maybe String of
          Nothing -> do
            Logger.logError (EchoBot.hLogHandle h) "sendTgAnswerCallbackQuery: the user did not select a callbackQuery response"
            return ()
          Just сallbackQueryData -> do
            sendTgMessageHelp
              1
              token
              "editMessageReplyMarkup"
              [ ("chat_id", T.pack $ show chatId),
                ("message_id", T.pack $ show messageId)
              ]
            sendTgMessageHelp
              1
              token
              "sendMessage"
              [ ("chat_id", T.pack $ show chatId),
                ( "text",
                  T.pack $
                    "Answer received! Number of repetitions " ++ сallbackQueryData
                )
              ]
            TgRepeats.writeTgRepeat
              h
              chatId
              (read сallbackQueryData :: TgRepeats.Repeats)
            Logger.logDebug (EchoBot.hLogHandle h) (T.append "sendTgAnswerCallbackQuery: writeTgRepeat:" $ T.pack сallbackQueryData)
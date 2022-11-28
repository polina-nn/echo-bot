{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TelegramAPI has function for http requests to API Telegram  (type IO)
-- and some service functions for return required record fields of TgUpdate, TgMessage
module FrontEnd.TelegramAPI
  ( getMeTg,
    getTgUpdates,
    tgGetLastUpdateAndId,
    checkEvent,
    sendTgResponse,
    tgGetLastMessageOrCallback,
    chooseCurrentChatHandle,
  )
where

import qualified Config
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.IORef (IORef, modifyIORef', readIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramTypes as TgTypes
import GHC.IORef (newIORef)
import qualified Logger
import qualified Network.HTTP.Req as Req
import System.Exit (die)

-- | buildRequestParams -- create QueryString for request
buildRequestParams ::
  (Req.QueryParam p, Monoid p) =>
  [(TgTypes.TgQueryParam, TgTypes.TgValueParam)] ->
  p
buildRequestParams [] = mempty
buildRequestParams params = mconcat $ fmap (uncurry (Req.=:)) params

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
getTgUpdates ::
  TgTypes.Token -> Maybe TgTypes.UpdateId -> IO (Maybe [TgTypes.TgUpdate])
getTgUpdates token Nothing = getTgUpdatesHelp token []
getTgUpdates token (Just lastUpdateId) =
  getTgUpdatesHelp
    token
    [ ("offset", T.pack $ show $ lastUpdateId + 1),
      ("timeout", T.pack $ show (10 :: Int))
    ]

getTgUpdatesHelp ::
  TgTypes.Token ->
  [(TgTypes.TgQueryParam, TgTypes.TgValueParam)] ->
  IO (Maybe [TgTypes.TgUpdate])
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

tgGetLastUpdateAndId ::
  Maybe [TgTypes.TgUpdate] -> Maybe (TgTypes.TgUpdate, Int)
tgGetLastUpdateAndId Nothing = Nothing
tgGetLastUpdateAndId (Just []) = Nothing
tgGetLastUpdateAndId (Just results) =
  Just (last results, tgGetLastUpdateId' (last results))
  where
    tgGetLastUpdateId' :: TgTypes.TgUpdate -> Int
    tgGetLastUpdateId' TgTypes.TgUpdate {TgTypes.tgUpdateUpdateId = c} = c

-- | tgGetLastMessageOrCallback  -  information about the latest message/callback and its chat id
tgGetLastMessageOrCallback :: TgTypes.TgUpdate -> TgTypes.MessageOrCallback
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateMessage = Just mess} =
  TgTypes.Message mess (TgTypes.tgChatId $ TgTypes.tgMessageChat mess)
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateEditedMessage = Just mess} =
  TgTypes.Message mess (TgTypes.tgChatId $ TgTypes.tgMessageChat mess)
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateChannelPost = Just mess} =
  TgTypes.Message mess (TgTypes.tgChatId $ TgTypes.tgMessageChat mess)
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateEditedChannelPost = Just mess} =
  TgTypes.Message mess (TgTypes.tgChatId $ TgTypes.tgMessageChat mess)
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateCallbackQuery = Just callbac} =
  case TgTypes.tgCallbackQueryMessage callbac :: Maybe TgTypes.TgMessage of
    Nothing -> TgTypes.Callback callbac Nothing
    Just tgMessage ->
      TgTypes.Callback
        callbac
        (Just (TgTypes.tgChatId $ TgTypes.tgMessageChat tgMessage))
tgGetLastMessageOrCallback _ = TgTypes.ErrorAPI

-- | checkEvent - create EchoBot.Event from message/callback
checkEvent :: TgTypes.MessageOrCallback -> EchoBot.Event TgTypes.Content
checkEvent (TgTypes.Callback mess _) = checkCallbackQuery mess
  where
    checkCallbackQuery ::
      TgTypes.TgCallbackQuery -> EchoBot.Event TgTypes.Content
    checkCallbackQuery TgTypes.TgCallbackQuery {tgCallbackQueryData = Just rep} =
      EchoBot.SetRepetitionCountEvent (read rep :: Int)
    checkCallbackQuery _ =
      EchoBot.MessageEvent $ TgTypes.ErrorMessage "Unknown content type!"
checkEvent TgTypes.ErrorAPI =
  EchoBot.MessageEvent $ TgTypes.ErrorMessage "Error API Telegram!"
checkEvent (TgTypes.Message mess _) = checkMessage mess
  where
    checkMessage :: TgTypes.TgMessage -> EchoBot.Event TgTypes.Content
    checkMessage TgTypes.TgMessage {tgMessageText = Just me} =
      EchoBot.MessageEvent $ TgTypes.ValidMessage (T.pack me)
    checkMessage TgTypes.TgMessage {tgMessageSticker = Just sticker} =
      checkSticker sticker
    checkMessage _ =
      EchoBot.MessageEvent $ TgTypes.ErrorMessage "Unknown content type!"
    checkSticker :: TgTypes.TgSticker -> EchoBot.Event TgTypes.Content
    checkSticker TgTypes.TgSticker {tgStickerFileId = stringId} =
      EchoBot.MessageEvent $ TgTypes.Sticker (T.pack stringId)

-- | chooseCurrentChatHandle - choose/create handle for received message/callback.
chooseCurrentChatHandle ::
  EchoBot.Handle IO TgTypes.Content ->
  TgTypes.MessageOrCallback ->
  IORef TgTypes.TgRepeats ->
  IO (Maybe TgTypes.Handle)
chooseCurrentChatHandle h (TgTypes.Message _ chat) mapRepeats =
  chooseCurrentChatHandle' h chat mapRepeats
chooseCurrentChatHandle h (TgTypes.Callback _ (Just chat)) mapRepeats =
  chooseCurrentChatHandle' h chat mapRepeats
chooseCurrentChatHandle _ (TgTypes.Callback _ Nothing) _ = return Nothing
chooseCurrentChatHandle _ TgTypes.ErrorAPI _ = return Nothing

-- | chooseCurrentChatHandle' -- choose/create handle for received message/callback. Create handle from template in makeBotHandle
chooseCurrentChatHandle' ::
  EchoBot.Handle IO TgTypes.Content ->
  TgTypes.ChatId ->
  IORef TgTypes.TgRepeats ->
  IO (Maybe TgTypes.Handle)
chooseCurrentChatHandle' h chat mapRepeats = do
  mapRepeats' <- readIORef mapRepeats
  case Map.lookup chat mapRepeats' of
    Nothing -> do
      newHandle <- makeBotHandle h
      state <- EchoBot.hGetState newHandle
      Logger.logDebug (EchoBot.hLogHandle h) $
        T.concat
          [ T.pack
              "chooseCurrentChatHandle: OK! New handle with state from config ",
            T.pack $ show state,
            T.pack "for new chart Id",
            T.pack $ show chat
          ]
      let newHandle' = TgTypes.Handle {hBotHandle = newHandle}
      let fun = Map.insert chat newHandle'
      modifyIORef' mapRepeats fun
      mapRepeats'' <- readIORef mapRepeats
      return $ Just (mapRepeats'' Map.! chat)
    Just val -> do
      state <- EchoBot.hGetState (TgTypes.hBotHandle val)
      Logger.logDebug (EchoBot.hLogHandle h) $
        T.concat
          [ T.pack "chooseCurrentChatHandle: OK! Handle with state  ",
            T.pack $ show state,
            T.pack "for chart Id",
            T.pack $ show chat
          ]
      return $ Just val

-- makeBotHandle -- clean handle whith stateRef from config.
makeBotHandle ::
  EchoBot.Handle IO TgTypes.Content -> IO (EchoBot.Handle IO TgTypes.Content)
makeBotHandle h = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = readIORef stateRef,
        EchoBot.hModifyState' = modifyIORef' stateRef,
        EchoBot.hLogHandle = EchoBot.hLogHandle h,
        EchoBot.hConfig = botConfig,
        EchoBot.hTextFromMessage =
          \case
            TgTypes.ValidMessage text -> Just text
            TgTypes.ErrorMessage text -> Just text
            TgTypes.ErrorAPITelegram text -> Just text
            TgTypes.Sticker text -> Just text,
        EchoBot.hMessageFromText = TgTypes.ValidMessage
      }

sendTgResponse ::
  EchoBot.Handle IO TgTypes.Content ->
  TgTypes.Token ->
  TgTypes.MessageOrCallback ->
  [EchoBot.Response TgTypes.Content] ->
  IO ()
sendTgResponse h token (TgTypes.Message tgMes _) resp@((EchoBot.MessageResponse (TgTypes.ValidMessage val)) : _) =
  sendTgText h (length resp) token tgMes val
sendTgResponse h token (TgTypes.Message tgMes _) ((EchoBot.MessageResponse (TgTypes.ErrorMessage val)) : _) =
  sendTgText h 1 token tgMes val
sendTgResponse h token (TgTypes.Message tgMes _) resp@((EchoBot.MessageResponse (TgTypes.Sticker stikerId)) : _) =
  sendTgStiker h (length resp) token tgMes stikerId
sendTgResponse h token (TgTypes.Message tgMes _) ((EchoBot.MenuResponse title _) : _) =
  sendTgKeyboard h token tgMes title
sendTgResponse h token (TgTypes.Callback call (Just _)) [] =
  sendTgAnswerCallbackQuery h token call
sendTgResponse _ _ (TgTypes.Callback _ Nothing) [] = pure ()
sendTgResponse h _ _ ((EchoBot.MessageResponse (TgTypes.ErrorAPITelegram _)) : _) = do
  Logger.logError (EchoBot.hLogHandle h) "sendTgResponse: BAD! ErrorAPITelegram"
  pure ()
sendTgResponse h _ _ _ = do
  Logger.logError (EchoBot.hLogHandle h) "sendTgResponse: BAD! Logic Error!!!"
  pure ()

sendTgText ::
  EchoBot.Handle IO TgTypes.Content ->
  EchoBot.RepetitionCount ->
  TgTypes.Token ->
  TgTypes.TgMessage ->
  T.Text ->
  IO ()
sendTgText h repeats token tgMes text = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
  sendTgMessage
    h
    repeats
    token
    "sendMessage"
    [("chat_id", T.pack $ show chatId), ("text", text)]

sendTgStiker ::
  EchoBot.Handle IO TgTypes.Content ->
  EchoBot.RepetitionCount ->
  TgTypes.Token ->
  TgTypes.TgMessage ->
  T.Text ->
  IO ()
sendTgStiker h repeats token tgMes stiker = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
  sendTgMessage
    h
    repeats
    token
    "sendSticker"
    [("chat_id", T.pack $ show chatId), ("sticker", stiker)]

sendTgKeyboard ::
  EchoBot.Handle IO TgTypes.Content ->
  TgTypes.Token ->
  TgTypes.TgMessage ->
  EchoBot.Title ->
  IO ()
sendTgKeyboard h token tgMes title = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
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
  sendTgMessage
    h
    1
    token
    "sendMessage"
    [ ("chat_id", T.pack $ show chatId),
      ("text", title),
      ("reply_markup", T.pack a)
    ]

sendTgAnswerCallbackQuery ::
  EchoBot.Handle IO TgTypes.Content ->
  TgTypes.Token ->
  TgTypes.TgCallbackQuery ->
  IO ()
sendTgAnswerCallbackQuery h token callbackQuery = do
  sendTgMessage
    h
    1
    token
    "answerCallbackQuery"
    [ ("callback_query_id", T.pack $ TgTypes.tgCallbackQueryId callbackQuery),
      ("text", T.pack "Your reply has been received")
    ]
  case TgTypes.tgCallbackQueryMessage callbackQuery :: Maybe TgTypes.TgMessage of
    Nothing -> do
      Logger.logError
        (EchoBot.hLogHandle h)
        "sendTgAnswerCallbackQuery: passed empty message to callbackQuery"
      return ()
    Just tgMessage -> do
      let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMessage
          messageId = TgTypes.tgMessageMessageId tgMessage
      case TgTypes.tgCallbackQueryData callbackQuery :: Maybe String of
        Nothing -> do
          Logger.logError
            (EchoBot.hLogHandle h)
            "sendTgAnswerCallbackQuery: the user did not select a callbackQuery response"
          return ()
        Just сallbackQueryData -> do
          sendTgMessage
            h
            1
            token
            "editMessageReplyMarkup"
            [ ("chat_id", T.pack $ show chatId),
              ("message_id", T.pack $ show messageId)
            ]
          sendTgMessage
            h
            1
            token
            "sendMessage"
            [ ("chat_id", T.pack $ show chatId),
              ( "text",
                T.pack $
                  "Answer received! Number of repetitions " ++ сallbackQueryData
              )
            ]
          let repetitionCount =
                read сallbackQueryData :: EchoBot.RepetitionCount
          _ <-
            EchoBot.respond h (EchoBot.SetRepetitionCountEvent repetitionCount)
          pure ()

-- | sendTgMessage -- helper function for sending messages
sendTgMessage ::
  EchoBot.Handle IO TgTypes.Content ->
  EchoBot.RepetitionCount ->
  TgTypes.Token ->
  TgTypes.TgUrl ->
  [(TgTypes.TgQueryParam, TgTypes.TgValueParam)] ->
  IO ()
sendTgMessage h repeats token url params =
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Control.Monad.replicateM repeats $
        Req.req
          Req.POST
          (Req.https "api.telegram.org" Req./: T.pack ("bot" ++ token) Req./: url)
          Req.NoReqBody
          Req.jsonResponse
          (buildRequestParams params)
    Control.Monad.IO.Class.liftIO $ ckeckSendTgResponse h response

ckeckSendTgResponse ::
  EchoBot.Handle IO TgTypes.Content ->
  [Req.JsonResponse TgTypes.TgSendResponse] ->
  IO ()
ckeckSendTgResponse h [] = do
  Logger.logError
    (EchoBot.hLogHandle h)
    "ckeckSendTgResponse: BAD Empty answer "
  return ()
ckeckSendTgResponse h response =
  if all (TgTypes.tgSendResponseOk . Req.responseBody) response
    then do
      Logger.logDebug (EchoBot.hLogHandle h) $
        T.append
          (T.pack "ckeckSendTgResponse: OK! \n")
          (T.concat $ map (T.pack . show . Req.responseBody) response)
      return ()
    else do
      Logger.logError (EchoBot.hLogHandle h) "ckeckSendTgResponse: BAD!"
      return ()

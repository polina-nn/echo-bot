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

import qualified Control.Exception.Safe as EX
import qualified Control.Monad
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Aeson as A
import qualified Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.TelegramException as TgException
import FrontEnd.TelegramTypes (Handle (hToken))
import qualified FrontEnd.TelegramTypes as TgTypes
import Logger (logDebug, logError, (.<))
import qualified Network.HTTP.Req as Req
import System.Exit (die)
import qualified Text.Read

-- | buildRequestParams -- create QueryString for request
buildRequestParams ::
  (Req.QueryParam p, Monoid p) =>
  [(TgTypes.TgQueryParam, TgTypes.TgValueParam)] ->
  p
buildRequestParams [] = mempty
buildRequestParams params = mconcat $ fmap (uncurry (Req.=:)) params

-- | getMeTg simple method for testing your bot's authentication token
-- If token is invalid, getMeTg send  Exception!
getMeTg :: TgTypes.Handle -> IO TgTypes.TgUser
getMeTg TgTypes.Handle {..} =
  EX.handle TgException.rethrowReqException $
    MIO.liftIO $
      Req.runReq Req.defaultHttpConfig $ do
        r <-
          Req.req
            Req.GET
            (Req.https "api.telegram.org" Req./: T.pack ("bot" ++ hToken) Req./: "getMe")
            Req.NoReqBody
            Req.jsonResponse
            mempty
        let bot = (TgTypes.tgGetMeResponseResult . Req.responseBody) r
        MIO.liftIO $ Logger.logDebug (EchoBot.hLogHandle hBotHandle) $ "getMeTg: OK!" .< bot
        return bot

-- | getTgUpdates -- get updates. If it return IO Nothing -- last TgUpdate is empty
-- when you call getTgUpdates with (Just lastUpdateId),
-- you call with lastUpdateId+1, because you are waiting new update.
-- If internet is invalid, getTgUpdates send  Exception!
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
  EX.handle TgException.rethrowReqException $
    MIO.liftIO $
      Req.runReq Req.defaultHttpConfig $ do
        r <-
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
            <$> Data.Aeson.Types.parseMaybe A.parseJSON (Req.responseBody r :: A.Value)

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
tgGetLastMessageOrCallback TgTypes.TgUpdate {TgTypes.tgUpdateCallbackQuery = Just callBack} =
  case TgTypes.tgCallbackQueryMessage callBack :: Maybe TgTypes.TgMessage of
    Nothing -> TgTypes.Callback callBack Nothing
    Just tgMessage ->
      TgTypes.Callback
        callBack
        (Just (TgTypes.tgChatId $ TgTypes.tgMessageChat tgMessage))
tgGetLastMessageOrCallback _ = TgTypes.ErrorAPI

-- | checkEvent - create EchoBot.Event from message/callback
checkEvent :: TgTypes.MessageOrCallback -> EchoBot.Event TgTypes.Content
checkEvent (TgTypes.Callback mess _) = checkCallbackQuery mess
  where
    checkCallbackQuery ::
      TgTypes.TgCallbackQuery -> EchoBot.Event TgTypes.Content
    checkCallbackQuery callBack =
      case checkCallbackFrom1To5 callBack of
        Nothing ->
          EchoBot.MessageEvent $
            TgTypes.ErrorMessage "You entered something, but numbers from 1 to 5 "
        Just number -> EchoBot.SetRepetitionCountEvent number
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

checkCallbackFrom1To5 :: TgTypes.TgCallbackQuery -> Maybe Int
checkCallbackFrom1To5 TgTypes.TgCallbackQuery {tgCallbackQueryData = Nothing} =
  Nothing
checkCallbackFrom1To5 TgTypes.TgCallbackQuery {tgCallbackQueryData = Just val} =
  case Text.Read.readMaybe val of
    Just number ->
      if 1 <= number && number <= 5
        then Just number
        else Nothing
    Nothing -> Nothing

-- | chooseCurrentChatHandle - choose/create handle for received message/callback.
chooseCurrentChatHandle ::
  TgTypes.Handle ->
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
  TgTypes.Handle ->
  TgTypes.ChatId ->
  IORef TgTypes.TgRepeats ->
  IO (Maybe TgTypes.Handle)
chooseCurrentChatHandle' h@TgTypes.Handle {..} chat mapRepeats = do
  mapRepeats' <- readIORef mapRepeats
  case Map.lookup chat mapRepeats' of
    Nothing -> do
      newHandle' <- makeBotHandle h
      let fun = Map.insert chat newHandle'
      modifyIORef' mapRepeats fun
      mapRepeats'' <- readIORef mapRepeats
      state <- EchoBot.hGetState (TgTypes.hBotHandle newHandle')
      Logger.logDebug (EchoBot.hLogHandle hBotHandle) $ "chooseCurrentChatHandle: OK! NEW handle with state from config " .< state <> " for new chart Id " .< chat
      return $ Just (mapRepeats'' Map.! chat)
    Just val -> do
      state <- EchoBot.hGetState (TgTypes.hBotHandle val)
      Logger.logDebug (EchoBot.hLogHandle hBotHandle) $ "chooseCurrentChatHandle: OK! Handle with state  " .< state <> " for chart Id" .< chat
      return $ Just val

makeBotHandle :: TgTypes.Handle -> IO TgTypes.Handle
makeBotHandle TgTypes.Handle {..} = do
  initialState <-
    either (die . T.unpack) pure $ EchoBot.makeState hTemplateBotConfig
  stateRef <- newIORef initialState
  let hBotHandle' =
        EchoBot.Handle
          { EchoBot.hGetState = readIORef stateRef,
            EchoBot.hModifyState' = modifyIORef' stateRef,
            EchoBot.hLogHandle = EchoBot.hLogHandle hBotHandle,
            EchoBot.hConfig = hTemplateBotConfig,
            EchoBot.hTextFromMessage =
              \case
                TgTypes.ValidMessage text -> text
                TgTypes.ErrorMessage text -> text
                TgTypes.ErrorAPITelegram text -> text
                TgTypes.Sticker text -> text,
            EchoBot.hMessageFromText = TgTypes.ValidMessage
          }
  pure
    TgTypes.Handle
      { TgTypes.hBotHandle = hBotHandle',
        TgTypes.hToken = hToken,
        TgTypes.hTemplateBotConfig = hTemplateBotConfig
      }

sendTgResponse ::
  TgTypes.Handle ->
  TgTypes.MessageOrCallback ->
  [EchoBot.Response TgTypes.Content] ->
  IO ()
sendTgResponse h (TgTypes.Message tgMes _) resp@((EchoBot.MessageResponse (TgTypes.ValidMessage val)) : _) =
  sendTgText h (length resp) tgMes val
sendTgResponse h (TgTypes.Message tgMes _) ((EchoBot.MessageResponse (TgTypes.ErrorMessage val)) : _) =
  sendTgText h 1 tgMes val
sendTgResponse h (TgTypes.Message tgMes _) resp@((EchoBot.MessageResponse (TgTypes.Sticker stickerId)) : _) =
  sendTgSticker h (length resp) tgMes stickerId
sendTgResponse h (TgTypes.Message tgMes _) ((EchoBot.MenuResponse title _) : _) =
  sendTgKeyboard h tgMes title
sendTgResponse h (TgTypes.Callback call (Just _)) [] =
  sendTgAnswerCallbackQuery h call
sendTgResponse _ (TgTypes.Callback _ Nothing) [] = pure ()
sendTgResponse TgTypes.Handle {..} _ ((EchoBot.MessageResponse (TgTypes.ErrorAPITelegram _)) : _) = do
  Logger.logError
    (EchoBot.hLogHandle hBotHandle)
    "sendTgResponse: BAD! ErrorAPITelegram"
  pure ()
sendTgResponse TgTypes.Handle {..} _ _ = do
  Logger.logError
    (EchoBot.hLogHandle hBotHandle)
    "sendTgResponse: BAD! Logic Error!!!"
  pure ()

sendTgText ::
  TgTypes.Handle ->
  EchoBot.RepetitionCount ->
  TgTypes.TgMessage ->
  T.Text ->
  IO ()
sendTgText h repeats tgMes text = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
  sendTgMessage
    h
    repeats
    "sendMessage"
    [("chat_id", T.pack $ show chatId), ("text", text)]

sendTgSticker ::
  TgTypes.Handle ->
  EchoBot.RepetitionCount ->
  TgTypes.TgMessage ->
  T.Text ->
  IO ()
sendTgSticker h repeats tgMes sticker = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
  sendTgMessage
    h
    repeats
    "sendSticker"
    [("chat_id", T.pack $ show chatId), ("sticker", sticker)]

sendTgKeyboard :: TgTypes.Handle -> TgTypes.TgMessage -> EchoBot.Title -> IO ()
sendTgKeyboard h tgMes title = do
  let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMes
      keyboard = createKeyboard 5
      a = BC.unpack (A.encode keyboard)
  sendTgMessage
    h
    1
    "sendMessage"
    [ ("chat_id", T.pack $ show chatId),
      ("text", title),
      ("reply_markup", T.pack a)
    ]

createKeyboard :: Int -> TgTypes.TgInlineKeyboardMarkup
createKeyboard n = TgTypes.TgInlineKeyboardMarkup [buttons]
  where
    buttons = map (\x -> TgTypes.TgInlineKeyboardButton (show x) (show x)) [1 .. n]

sendTgAnswerCallbackQuery :: TgTypes.Handle -> TgTypes.TgCallbackQuery -> IO ()
sendTgAnswerCallbackQuery h@TgTypes.Handle {..} callbackQuery = do
  sendTgMessage
    h
    1
    "answerCallbackQuery"
    [ ("callback_query_id", T.pack $ TgTypes.tgCallbackQueryId callbackQuery),
      ("text", T.pack "Your reply has been received")
    ]
  case TgTypes.tgCallbackQueryMessage callbackQuery :: Maybe TgTypes.TgMessage of
    Nothing -> do
      Logger.logError
        (EchoBot.hLogHandle hBotHandle)
        "sendTgAnswerCallbackQuery: passed empty message to callbackQuery"
      return ()
    Just tgMessage -> do
      let chatId = TgTypes.tgChatId $ TgTypes.tgMessageChat tgMessage
          messageId = TgTypes.tgMessageMessageId tgMessage
      case checkCallbackFrom1To5 callbackQuery of
        Nothing -> do
          Logger.logError
            (EchoBot.hLogHandle hBotHandle)
            "sendTgAnswerCallbackQuery: the user did not select a callbackQuery response or he selected not number or not from 1 to 5"
          return ()
        Just number -> do
          sendTgMessage
            h
            1
            "editMessageReplyMarkup"
            [ ("chat_id", T.pack $ show chatId),
              ("message_id", T.pack $ show messageId)
            ]
          sendTgMessage
            h
            1
            "sendMessage"
            [ ("chat_id", T.pack $ show chatId),
              ( "text",
                T.pack $
                  "Answer received! Number of repetitions " ++ show number
              )
            ]
          _ <-
            EchoBot.respond hBotHandle (EchoBot.SetRepetitionCountEvent number)
          pure ()

-- | sendTgMessage -- helper function for sending messages
sendTgMessage ::
  TgTypes.Handle ->
  EchoBot.RepetitionCount ->
  TgTypes.TgUrl ->
  [(TgTypes.TgQueryParam, TgTypes.TgValueParam)] ->
  IO ()
sendTgMessage h@TgTypes.Handle {..} repeats url params =
  EX.handle TgException.rethrowReqException $
    MIO.liftIO $
      Req.runReq Req.defaultHttpConfig $ do
        response <-
          Control.Monad.replicateM repeats $
            Req.req
              Req.POST
              (Req.https "api.telegram.org" Req./: T.pack ("bot" ++ hToken) Req./: url)
              Req.NoReqBody
              Req.jsonResponse
              (buildRequestParams params)
        MIO.liftIO $ checkSendTgResponse h response

checkSendTgResponse ::
  TgTypes.Handle -> [Req.JsonResponse TgTypes.TgSendResponse] -> IO ()
checkSendTgResponse TgTypes.Handle {..} [] = do
  Logger.logError
    (EchoBot.hLogHandle hBotHandle)
    "checkSendTgResponse: BAD Empty answer "
  return ()
checkSendTgResponse TgTypes.Handle {..} response =
  if all (TgTypes.tgSendResponseOk . Req.responseBody) response
    then do
      let textResponses = T.concat $ map (T.pack . show . Req.responseBody) response
      Logger.logDebug (EchoBot.hLogHandle hBotHandle) $ "checkSendTgResponse: OK! \n" .< textResponses
      return ()
    else do
      Logger.logError
        (EchoBot.hLogHandle hBotHandle)
        "checkSendTgResponse: BAD!"
      return ()

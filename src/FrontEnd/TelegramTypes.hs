{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TelegramTypes for http request to  API Telegram
module FrontEnd.TelegramTypes where

import qualified Control.Exception.Safe as EX
import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified GHC.Generics as G
import qualified Network.HTTP.Client as NC
import qualified Network.HTTP.Req as Req

type Token = String

type UpdateId = Int

type ChatId = Int

type Message = T.Text

type TgUrl = T.Text

type TgQueryParam = T.Text

type TgValueParam = T.Text

type TgRepeats = Map.Map ChatId Handle

newtype Handle =
  Handle
    { hBotHandle :: EchoBot.Handle IO Content
    }

data BotException
  -- | ServiceAPIError  -- ERROR while communicating with Telegram services (Token is invalid)
  = ServiceAPIError String
  -- | NetworkError -- Network communication ERROR (Problem whith Internet)
  | NetworkError EX.SomeException

instance EX.Exception BotException

rethrowReqException :: EX.MonadThrow m => Req.HttpException -> m a
rethrowReqException (Req.VanillaHttpException (NC.HttpExceptionRequest _ (NC.StatusCodeException resp _))) =
  EX.throw (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (Req.VanillaHttpException e) =
  EX.throw (NetworkError $ EX.toException e)
rethrowReqException (Req.JsonHttpException s) = EX.throw (ServiceAPIError s)

instance Show BotException where
  show (ServiceAPIError _) =
    "ERROR while communicating with Telegram services. You must check the token in the 'config.conf' file. "
  show (NetworkError _) =
    "Network communication ERROR. You must check your Internet. "

-- | Content - types of messages that the bot supports
data Content
    -- | ValidMessage T.Text -- test message
  = ValidMessage T.Text
  -- | Sticker T.Text -- stiker whith stickerFileName
  | Sticker T.Text
 -- | ErrorMessage T.Text -- unsupported content (file, gif ... ) whith text "Unknown content type!"  
  | ErrorMessage T.Text
 -- | ErrorAPITelegram T.Text -- if an update is received, but all fields are  nothing (error may occur if the telegram's api has been changed). Text "Error API Telegram!" - write in log 
  | ErrorAPITelegram T.Text
  deriving (Show)

-- | MessageOrCallback - type of content in TgUpdate with chart id
data MessageOrCallback
    -- | Message  -- one of Content type
  = Message TgMessage ChatId
    -- | Callback  -- keyboard response (after user press the key - with Just ChatId )
  | Callback TgCallbackQuery (Maybe ChatId)
    -- | ErrorAPI  - perhaps a new message type will be added to the telegram's api (in the future)
  | ErrorAPI
  deriving (Show)

-- | TgGetUpdateResponseBody return by getUpdates
data TgGetUpdateResponseBody =
  TgGetUpdateResponseBody
    -- | status
    { tgGetUpdateResponseBodyOk :: Bool
    -- | Array of incoming updates.
    , tgGetUpdateResponseBodyResult :: [TgUpdate]
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgGetUpdateResponseBody where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 23}

-- | TgGetMeResponse return by getMe
-- | A simple method for testing your bot's authentication token. Returns basic information about the bot in form of a User object.
data TgGetMeResponse =
  TgGetMeResponse
    { tgGetMeResponseOk :: Bool
    , tgGetMeResponseResult :: TgUser
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgGetMeResponse where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 15}

-- | TgSendMessageResponse return by sendTgMessage
newtype TgSendResponse =
  TgSendResponse
    { tgSendResponseOk :: Bool
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgSendResponse where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 14}

-- | if getUpdate not empty, it contain TgMessage or TgCallbackQuery or
-- UpdateMessageOrCallbackQueryError (if in getUpdate will add new fields by Telegram)
data UpdateMessageOrCallbackQuery
  = UpdateMessage TgMessage
  | UpdateCallbackQuery TgCallbackQuery
  | UpdateMessageOrCallbackQueryError
  deriving (Show)

-- | Update. This type represents an incoming update.
-- | At most one of the optional parameters (Maybe Message )can be present in any given update.
data TgUpdate =
  TgUpdate
    -- | The update's unique identifier
    { tgUpdateUpdateId :: Int
    -- | New incoming message of any kind — text, photo, sticker, etc.
    , tgUpdateMessage :: Maybe TgMessage
    -- | New version of a message that is known to the bot and was edited
    , tgUpdateEditedMessage :: Maybe TgMessage
    -- | New incoming channel post of any kind — text, photo, sticker, etc.
    , tgUpdateChannelPost :: Maybe TgMessage
    -- | New version of a channel post that is known to the bot and was edited
    , tgUpdateEditedChannelPost :: Maybe TgMessage
    -- | callback_query  New incoming callback query
    , tgUpdateCallbackQuery :: Maybe TgCallbackQuery
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgUpdate where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

-- | Message. This type represents a message.
data TgMessage =
  TgMessage
    -- | 1 Optional. For text messages, the actual UTF-8 text of the message, 0-4096 characters
    { tgMessageText :: Maybe String
    -- | 2 Optional. Message is a sticker, information about the sticker
    , tgMessageSticker :: Maybe TgSticker
    -- | Unique message identifier inside this chat
    , tgMessageMessageId :: Int
    -- | Optional. Sender, empty for messages sent to channels
    , tgMessageFrom :: Maybe TgUser
    -- | Optional. Sender of the message, sent on behalf of a chat.
    , tgMessageSenderChat :: Maybe TgChat
    -- | Date the message was sent in Unix time
    , tgMessageDate :: Int
    -- | Optional. Date the message was last edited in Unix time.
    , tgMessageEditDate :: Maybe Integer
    -- | Conversation the message belongs to.
    , tgMessageChat :: TgChat
    -- | Optional. For forwarded messages, sender of the original message
    , tgMessageForwardFrom :: Maybe TgUser
    -- | Optional. For messages forwarded from channels or from anonymous administrators, information about the original sender chat
    , tgMessageForwardFromChat :: Maybe TgChat
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgMessage where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 9}

-- | CallbackQuery the response to pressing a button on the keyboard comes in the form JSON
data TgCallbackQuery =
  TgCallbackQuery
    -- | Unique identifier for this query
    { tgCallbackQueryId :: String
    -- | Sender
    , tgCallbackQueryFrom :: TgUser
    -- | Optional. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
    , tgCallbackQueryMessage :: Maybe TgMessage
    -- | 	Optional. Identifier of the message sent via the bot in inline mode, that originated the query.
    , tgCallbackQueryInlineMessageId :: Maybe String
    -- | Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
    , tgCallbackQueryChatInstance :: String
    -- | Optional. Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
    , tgCallbackQueryData :: Maybe String
    -- | Optional. Short name of a Game to be returned, serves as the unique identifier for the game
    , tgCallbackQueryGameShortName :: Maybe String
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgCallbackQuery where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 15}

--  This object represents one button of an inline keyboard. You must use exactly one of the optional fields. Кодирую в формат JSON дл передачи в sendMessage
data TgInlineKeyboardButton =
  TgInlineKeyboardButton
    -- | 	Label text on the button
    { tgInlineKeyboardButtonText :: String
    -- | Optional. Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes.
    -- | In the project, a mandatory parameter, where we will catch the number of repetitions that the user wants
    , tgInlineKeyboardButtonCallbackData :: String
    }
  deriving (Show, G.Generic)

instance A.ToJSON TgInlineKeyboardButton where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 22}

-- | This object represents an inline keyboard that appears right next to the message it belongs to.
-- To JSON for send in sendMessage
newtype TgInlineKeyboardMarkup =
  TgInlineKeyboardMarkup
    { tgInlineKeyboardMarkupInlineKeyboard :: [[TgInlineKeyboardButton]]
    }
  deriving (Show, G.Generic)

instance A.ToJSON TgInlineKeyboardMarkup where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 22}

-- | User . This type represents a Telegram user or bot.
data TgUser =
  TgUser
    -- | Unique identifier for this user or bot
    { tgUserId :: Int
    -- | True, if this user is a bot
    , tgUserIsBot :: Bool
    -- | User's or bot's first name
    , tgUserFirstName :: String
    -- | Optional. User's or bot's last name
    , tgUserLastName :: Maybe String
    -- | Optional. User's or bot's username
    , tgUserUsername :: Maybe String
    -- | Optional. IETF language tag of the user's language
    , tgUserLanguageCode :: Maybe String
    -- | Optional. True, if the bot can be invited to groups. Returned only in getMe.
    , tgUserCanJoinGroups :: Maybe Bool
    -- | Optional. True, if privacy mode is disabled for the bot. Returned only in getMe.
    , tgUserCanReadAllGroupMessages :: Maybe Bool
    -- | Optional. True, if the bot supports inline queries. Returned only in getMe.
    , tgUserSupportsInlineQueries :: Maybe Bool
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgUser where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 6}

-- |  Chat .This type represents a chat.
data TgChat =
  TgChat
    -- | Unique identifier for this chat.
    { tgChatId :: Int
    -- | Type of chat, can be either “private”, “group”, “supergroup” or “channel”
    , tgChatType :: String
    -- | 	Optional. Title, for supergroups, channels and group chats
    , tgChatTitle :: Maybe String
    -- | Optional. Username, for private chats, supergroups and channels if available
    , tgChatUsername :: Maybe String
    -- | Optional. First name of the other party in a private chat
    , tgChatFirstName :: Maybe String
    -- | Optional. Last name of the other party in a private chat
    , tgChatLastName :: Maybe String
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgChat where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 6}

-- TgSticker This object represents a sticker.
data TgSticker =
  TgSticker
    -- | Identifier for this file, which can be used to download or reuse the file
    { tgStickerFileId :: String
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    , tgStickerFileUniqueId :: String
    -- | Sticker width
    , tgStickerWidth :: Int
    -- | Sticker height
    , tgStickerHeight :: Int
    -- |  True, if the sticker is animated
    , tgStickerIsAnimated :: Bool
    -- | 	Optional. Sticker thumbnail in the .WEBP or .JPG format
    , tgStickerThumb :: Maybe TgPhotoSize
    -- | Optional. Emoji associated with the sticker
    , tgStickerEmoji :: Maybe String
    -- | 	Optional. Name of the sticker set to which the sticker belongs
    , tgStickerSetName :: Maybe String
    -- | Optional. For mask stickers, the position where the mask should be placed
    , tgStickerMaskPosition :: Maybe TgMaskPosition
    -- | Optional. File size in bytes
    , tgStickerFileSize :: Maybe Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgSticker where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 9}

-- TgPhotoSize This object represents one size of a photo or a file / sticker thumbnail.
data TgPhotoSize =
  TgPhotoSize
    -- | Identifier for this file, which can be used to download or reuse the file
    { tgPhotoSizeFileId :: String
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    , tgPhotoSizeFileUniqueId :: String
    -- | Photo width
    , tgPhotoSizeWidth :: Int
    -- | Photo height
    , tgPhotoSizeHeight :: Int
    -- | Optional. File size in bytes
    , tgPhotoSizeFileSize :: Maybe Int
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgPhotoSize where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 11}

-- TgMaskPosition This object describes the position on faces where a mask should be placed by default.
data TgMaskPosition =
  TgMaskPosition
    -- | The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
    { tgMaskPositionPoint :: String
    -- | Shift by X-axis measured in widths of the mask scaled to the face size, from left to right.
    , tgMaskPositionXShift :: Float
    -- | Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom.
    , tgMaskPositionYShift :: Float
    -- | Mask scaling coefficient. For example, 2.0 means double size.
    , tgMaskPositionScale :: Float
    }
  deriving (Show, G.Generic)

instance A.FromJSON TgMaskPosition where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 14}

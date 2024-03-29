-- | TelegramTypes for http request to  API Telegram
module FrontEnd.TelegramTypes where

import qualified Data.Aeson as A
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified EchoBot
import qualified GHC.Generics as G

type Token = String

type UpdateId = Int

type ChatId = Int

type Message = T.Text

type TgUrl = T.Text

type TgQueryParam = T.Text

type TgValueParam = T.Text

type TgRepeats = Map.Map ChatId Handle

data Handle = Handle
  { -- | hBotHandle -- keep current state of bot for current chatId
    hBotHandle :: EchoBot.Handle IO Content,
    -- | hToken -- keep bot's token
    hToken :: Token,
    -- | hRepetitionCount -- for initial state of hBotHandle for any chat id. Use it, when create the new member of TgRepeat
    hTemplateBotConfig :: EchoBot.Config
  }

-- | Content - types of messages that the bot supports
data Content
  = -- | ValidMessage T.Text -- test message
    ValidMessage T.Text
  | -- | Sticker T.Text -- sticker with stickerFileName
    Sticker T.Text
  | -- | ErrorMessage T.Text -- unsupported content (file, gif ... ) with text "Unknown content type!"
    ErrorMessage T.Text
  | -- | ErrorAPITelegram T.Text -- if an update is received, but all fields are  nothing (error may occur if the telegram's api has been changed). Text "Error API Telegram!" - write in log
    ErrorAPITelegram T.Text
  deriving (Show)

-- | MessageOrCallback - type of content in TgUpdate with chart id
data MessageOrCallback
  = -- | Message  -- one of Content type
    Message TgMessage ChatId
  | -- | Callback  -- keyboard response (after user press the key - with Just ChatId )
    Callback TgCallbackQuery (Maybe ChatId)
  | -- | ErrorAPI  - perhaps a new message type will be added to the telegram's api (in the future)
    ErrorAPI
  deriving (Show)

-- | TgGetUpdateResponseBody return by getUpdates
data TgGetUpdateResponseBody = TgGetUpdateResponseBody
  { -- | status
    tgGetUpdateResponseBodyOk :: Bool,
    -- | Array of incoming updates.
    tgGetUpdateResponseBodyResult :: [TgUpdate]
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgGetUpdateResponseBody where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 23}

-- | TgGetMeResponse return by getMe
-- | A simple method for testing your bot's authentication token. Returns basic information about the bot in form of a User object.
data TgGetMeResponse = TgGetMeResponse
  { tgGetMeResponseOk :: Bool,
    tgGetMeResponseResult :: TgUser
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgGetMeResponse where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 15}

-- | TgSendMessageResponse return by sendTgMessage
newtype TgSendResponse = TgSendResponse
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
data TgUpdate = TgUpdate
  { -- | The update's unique identifier
    tgUpdateUpdateId :: Int,
    -- | New incoming message of any kind — text, photo, sticker, etc.
    tgUpdateMessage :: Maybe TgMessage,
    -- | New version of a message that is known to the bot and was edited
    tgUpdateEditedMessage :: Maybe TgMessage,
    -- | New incoming channel post of any kind — text, photo, sticker, etc.
    tgUpdateChannelPost :: Maybe TgMessage,
    -- | New version of a channel post that is known to the bot and was edited
    tgUpdateEditedChannelPost :: Maybe TgMessage,
    -- | callback_query  New incoming callback query
    tgUpdateCallbackQuery :: Maybe TgCallbackQuery
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgUpdate where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 8}

-- | Message. This type represents a message.
data TgMessage = TgMessage
  { -- | 1 Optional. For text messages, the actual UTF-8 text of the message, 0-4096 characters
    tgMessageText :: Maybe String,
    -- | 2 Optional. Message is a sticker, information about the sticker
    tgMessageSticker :: Maybe TgSticker,
    -- | Unique message identifier inside this chat
    tgMessageMessageId :: Int,
    -- | Conversation the message belongs to.
    tgMessageChat :: TgChat
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgMessage where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 9}

-- | CallbackQuery the response to pressing a button on the keyboard comes in the form JSON
data TgCallbackQuery = TgCallbackQuery
  { -- | Unique identifier for this query
    tgCallbackQueryId :: String,
    -- | Optional. Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
    tgCallbackQueryMessage :: Maybe TgMessage,
    -- | Optional. Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
    tgCallbackQueryData :: Maybe String
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgCallbackQuery where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 15}

--  This object represents one button of an inline keyboard. You must use exactly one of the optional fields. Encode in JSON format for  sendMessage
data TgInlineKeyboardButton = TgInlineKeyboardButton
  { -- | 	Label text on the button
    tgInlineKeyboardButtonText :: String,
    -- | In the project, a mandatory parameter, where we will catch the number of repetitions that the user wants
    tgInlineKeyboardButtonCallbackData :: String
  }
  deriving (Show, G.Generic)

instance A.ToJSON TgInlineKeyboardButton where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 22}

-- | This object represents an inline keyboard that appears right next to the message it belongs to.
-- To JSON for send in sendMessage
newtype TgInlineKeyboardMarkup = TgInlineKeyboardMarkup
  { tgInlineKeyboardMarkupInlineKeyboard :: [[TgInlineKeyboardButton]]
  }
  deriving (Show, G.Generic)

instance A.ToJSON TgInlineKeyboardMarkup where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 22}

-- | User . This type represents a Telegram user or bot.
data TgUser = TgUser
  { -- | Unique identifier for this user or bot
    tgUserId :: Int,
    -- | True, if this user is a bot
    tgUserIsBot :: Bool,
    -- | User's or bot's first name
    tgUserFirstName :: String
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgUser where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 6}

-- |  Chat .This type represents a chat.
newtype TgChat = TgChat
  { -- | Unique identifier for this chat.
    tgChatId :: Int
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgChat where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 6}

-- TgSticker This object represents a sticker.
newtype TgSticker = TgSticker
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgStickerFileId :: String
  }
  deriving (Show, G.Generic)

instance A.FromJSON TgSticker where
  parseJSON =
    A.genericParseJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 9}

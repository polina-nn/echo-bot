--{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TgResponseTypes for http request to  API Telegram

module FrontEnd.TelegramTypes where
import qualified Data.Aeson as A
import qualified GHC.Generics as G
import qualified Data.Text as T

type Token = String
type UpdateId = Int
type ChatId = Int
type Message = T.Text


type TgError = String
-- | TgGetUpdateResponseBody return by getUpdates
data TgGetUpdateResponseBody = TgGetUpdateResponseBody
  { -- | status
    tgGetUpdateResponseBodyOk :: Bool,
    -- | Array of incoming updates.
    tgGetUpdateResponseBodyResult :: [TgUpdate]
  } deriving (Show, G.Generic)
instance A.FromJSON TgGetUpdateResponseBody where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 23 }

-- | TgGetMeResponse return by getMe
-- | A simple method for testing your bot's authentication token. Returns basic information about the bot in form of a User object.
data TgGetMeResponse = TgGetMeResponse
   { tgGetMeResponseOk:: Bool
   , tgGetMeResponseResult :: TgUser
   } deriving (Show, G.Generic)
instance A.FromJSON TgGetMeResponse where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 15 }

-- | if getUpdate not empty, it contain TgMessage or TgCallbackQuery or 
-- UpdateMessageOrCallbackQueryError (if in getUpdate will add new fields by Telegram)
data UpdateMessageOrCallbackQuery = UpdateMessage  TgMessage | UpdateCallbackQuery  TgCallbackQuery | UpdateMessageOrCallbackQueryError
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

  } deriving (Show, G.Generic)
instance A.FromJSON TgUpdate where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 8 }


-- | Message. This type represents a message.
data TgMessage = TgMessage
  { -- | 1 Optional. For text messages, the actual UTF-8 text of the message, 0-4096 characters
    tgMessageText :: Maybe String,
    -- | 2 Optional. Message is a sticker, information about the sticker
    tgMessageSticker :: Maybe TgSticker,
    -- | 3 Optional. Message is an audio file, information about the file
    --tgMessageAudio :: Maybe TgAudio,
    -- | 4 Optional. Message is a general file, information about the file
    --tgMessageDocument :: Maybe TgDocument,
    -- | 5 Optional. Message is a photo, available sizes of the photo
    --tgMessagePhoto :: Maybe [TgPhotoSize],
    -- | 6 Optional. Message is a video, information about the video
    --tgMessageVideo :: Maybe TgVideo,
    -- | 7 Optional. Message is a video note, information about the video message
    --tgMessageVideoNote :: Maybe TgVideoNote,
    -- | 8 Optional. Message is an animation, information about the animation. For backward compatibility, when this field is set, the document field will also be set
    --tgMessageAnimation :: Maybe TgAnimation,
    -- | 9 Optional. This object represents a voice note.
    --tgMessageVoice :: Maybe TgVoice,
    -- | 10 Optional.Optional. Message is a shared contact, information about the contact
    --tgMessageContact :: Maybe TgContact,
    -- | 11 Optional. Message is a shared location, information about the location
    --tgMessagelocation :: Maybe TgLocation,
    -- | Unique message identifier inside this chat
    tgMessageMessageId :: Int,
    -- | Optional. Sender, empty for messages sent to channels
    tgMessageFrom :: Maybe TgUser,
    -- | Optional. Sender of the message, sent on behalf of a chat.
    tgMessageSenderChat :: Maybe TgChat,
    -- | Date the message was sent in Unix time
    tgMessageDate :: Int,
    -- | Optional. Date the message was last edited in Unix time.
    tgMessageEditDate :: Maybe Integer,
    -- | Conversation the message belongs to.
    tgMessageChat :: TgChat,
    -- | Optional. For forwarded messages, sender of the original message
    tgMessageForwardFrom :: Maybe TgUser,
    -- | Optional. For messages forwarded from channels or from anonymous administrators, information about the original sender chat
    tgMessageForwardFromChat :: Maybe TgChat
    -- | Optional. Caption for the animation, audio, document, photo, video or voice, 0-1024 characters
    --tgMessageCaption :: Maybe String,
    -- | Optional. For replies, the original message. Note that the Message object in this field will not contain further reply_to_message fields even if it itself is a reply.
    --tgMessageReplyToMessage :: Maybe TgMessage
  } deriving (Show, G.Generic)
instance A.FromJSON TgMessage where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 9 }

-- | CallbackQuery the response to pressing a button on the keyboard comes in the form JSON
data TgCallbackQuery = TgCallbackQuery
  { -- | Unique identifier for this query
    tgCallbackQueryId :: String
    -- | Sender
  , tgCallbackQueryFrom:: TgUser
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
  } deriving (Show, G.Generic)
instance A.FromJSON TgCallbackQuery where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 15 }

--  This object represents one button of an inline keyboard. You must use exactly one of the optional fields. Кодирую в формат JSON дл передачи в sendMessage
data TgInlineKeyboardButton = TgInlineKeyboardButton
  { -- |	Label text on the button
    tgInlineKeyboardButtonText :: String
    -- | Optional. Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes.
    -- | In the project, a mandatory parameter, where we will catch the number of repetitions that the user wants
  , tgInlineKeyboardButtonCallbackData :: String
  } deriving (Show, G.Generic)
instance A.ToJSON TgInlineKeyboardButton where
    toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 22}

-- | This object represents an inline keyboard that appears right next to the message it belongs to.
-- To JSON for send in sendMessage
newtype TgInlineKeyboardMarkup = TgInlineKeyboardMarkup
  { tgInlineKeyboardMarkupInlineKeyboard :: [[TgInlineKeyboardButton]]
  } deriving (Show, G.Generic)
instance A.ToJSON TgInlineKeyboardMarkup where
    toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_'. drop 22}

-- | User . This type represents a Telegram user or bot.
data TgUser = TgUser
  { -- | Unique identifier for this user or bot
    tgUserId :: Int,
    -- | True, if this user is a bot
    tgUserIsBot :: Bool,
    -- | User's or bot's first name
    tgUserFirstName :: String,
    -- | Optional. User's or bot's last name
    tgUserLastName :: Maybe String,
    -- | Optional. User's or bot's username
    tgUserUsername :: Maybe String,
    -- | Optional. IETF language tag of the user's language
    tgUserLanguageCode :: Maybe String,
    -- | Optional. True, if the bot can be invited to groups. Returned only in getMe.
    tgUserCanJoinGroups :: Maybe Bool,
    -- | Optional. True, if privacy mode is disabled for the bot. Returned only in getMe.
    tgUserCanReadAllGroupMessages :: Maybe Bool,
    -- | Optional. True, if the bot supports inline queries. Returned only in getMe.
    tgUserSupportsInlineQueries :: Maybe Bool
  }  deriving (Show, G.Generic)
instance A.FromJSON TgUser where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 6 }

-- |  Chat .This type represents a chat.
data TgChat = TgChat
  { -- | Unique identifier for this chat.
    tgChatId :: Int,
    -- | Type of chat, can be either “private”, “group”, “supergroup” or “channel”
    tgChatType :: String,
    -- | 	Optional. Title, for supergroups, channels and group chats
    tgChatTitle :: Maybe String,
    -- | Optional. Username, for private chats, supergroups and channels if available
    tgChatUsername :: Maybe String,
    -- | Optional. First name of the other party in a private chat
    tgChatFirstName :: Maybe String,
    -- | Optional. Last name of the other party in a private chat
    tgChatLastName :: Maybe String
  } deriving (Show, G.Generic)
instance A.FromJSON TgChat where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 6 }

-- TgSticker This object represents a sticker.
data TgSticker = TgSticker
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgStickerFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgStickerFileUniqueId :: String,
    -- | Sticker width
    tgStickerWidth :: Int,
    -- | Sticker height
    tgStickerHeight :: Int,
    -- |  True, if the sticker is animated
    tgStickerIsAnimated :: Bool,
    -- | 	Optional. Sticker thumbnail in the .WEBP or .JPG format
    tgStickerThumb :: Maybe TgPhotoSize,
    -- | Optional. Emoji associated with the sticker
    tgStickerEmoji :: Maybe String,
    -- | 	Optional. Name of the sticker set to which the sticker belongs
    tgStickerSetName :: Maybe String,
    -- | Optional. For mask stickers, the position where the mask should be placed
    tgStickerMaskPosition :: Maybe TgMaskPosition,
    -- | Optional. File size in bytes
    tgStickerFileSize :: Maybe Int
  } deriving (Show, G.Generic)
instance A.FromJSON TgSticker where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 9 }


-- TgPhotoSize This object represents one size of a photo or a file / sticker thumbnail.
data TgPhotoSize =  TgPhotoSize
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgPhotoSizeFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgPhotoSizeFileUniqueId::String,
    -- | Photo width
    tgPhotoSizeWidth :: Int,
    -- | Photo height
    tgPhotoSizeHeight :: Int,
    -- | Optional. File size in bytes
    tgPhotoSizeFileSize :: Maybe Int
  } deriving (Show, G.Generic)
instance A.FromJSON TgPhotoSize where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 11 }

-- TgMaskPosition This object describes the position on faces where a mask should be placed by default.
data TgMaskPosition = TgMaskPosition
  { -- | The part of the face relative to which the mask should be placed. One of “forehead”, “eyes”, “mouth”, or “chin”.
    tgMaskPositionPoint :: String,
    -- | Shift by X-axis measured in widths of the mask scaled to the face size, from left to right.
    tgMaskPositionXShift:: Float,
    -- | Shift by Y-axis measured in heights of the mask scaled to the face size, from top to bottom.
    tgMaskPositionYShift :: Float,
    -- | Mask scaling coefficient. For example, 2.0 means double size.
    tgMaskPositionScale:: Float
  } deriving (Show, G.Generic)
instance A.FromJSON TgMaskPosition where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = A.camelTo2 '_'. drop 14 }

{--
-- TgAudio This object represents an audio file to be treated as music by the Telegram clients.
data TgAudio = TgAudio
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgAudioFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgAudioFileUniqueId :: String,
    -- | Duration of the audio in seconds as defined by sender
    tgAudioDuration :: Int,
    -- | Optional. Performer of the audio as defined by sender or by audio tags
    tgAudioPerformer :: Maybe String,
    -- | Optional. Title of the audio as defined by sender or by audio tags
    tgAudioTitle :: Maybe String,
    -- | Optional. Original filename as defined by sender
    tgAudioFile_name :: Maybe String,
    -- | Optional. MIME type of the file as defined by sender
    tgAudioMimeType :: Maybe String,
    -- | Optional. File size in bytes
    tgAudioFileSize :: Maybe Int,
    -- | Optional. Thumbnail of the album cover to which the music file belongs
    tgAudioThumb :: Maybe TgPhotoSize
  } deriving (Show, Generic)
instance FromJSON TgAudio where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 7 }

-- TgDocument This object represents a general file (as opposed to photos, voice messages and audio files).
data TgDocument = TgDocument
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgDocumentFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgDocumentFileUniqueId :: String,
    -- | Optional. Document thumbnail as defined by sender
    tgDocumentThumb :: Maybe TgPhotoSize,
    -- | Optional. Original filename as defined by sender
    tgDocumentFileName :: Maybe String,
    -- | Optional. MIME type of the file as defined by sender
    tgDocumentMimeType :: Maybe String,
    -- | Optional. File size in bytes
    tgDocumentFileSize :: Maybe Int
  }deriving (Show, Generic)
instance FromJSON TgDocument where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 10 }

data TgVideo = TgVideo
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgVideoFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgVideoFileUniqueId :: String,
    -- | 	Video width as defined by sender
    tgVideoWidth :: Int,
    -- | Video height as defined by sender
    tgVideoHeight :: Int,
    -- | Duration of the video in seconds as defined by sender
    tgVideoDuration :: Int,
    -- | Optional. Video thumbnail
    tgVideoThumb :: Maybe TgPhotoSize,
    -- | Optional. Original filename as defined by sender
    tgVideoFileName :: Maybe String,
    -- | Optional. Mime type of a file as defined by sender
    tgVideoMimeType :: Maybe String,
    -- | Optional. File size in bytes
    tgVideoFileSize :: Int
  }deriving (Show, Generic)
instance FromJSON TgVideo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 7 }

-- TgVideoNote This object represents a video message (available in Telegram apps as of v.4.0).
data TgVideoNote = TgVideoNote
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgVideoNoteFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgVideoNoteFileUniqueId :: String,
    -- | Video width and height (diameter of the video message) as defined by sender
    tgVideoNoteLength :: Int,
    -- | Duration of the video in seconds as defined by sender
    tgVideoNoteDuration :: Int,
    -- | Optional. Video thumbnail
    tgVideoNoteThumb :: Maybe TgPhotoSize,
    -- | Optional. File size in bytes
    tgVideoNoteFileSize :: Maybe Int
  }deriving (Show, Generic)
instance FromJSON TgVideoNote where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 11 }

-- TgAnimation This object represents an animation file (GIF or H.264/MPEG-4 AVC video without sound).
data TgAnimation = TgAnimation
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgAnimationFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgAnimationFileUniqueId :: String,
    -- | Video width as defined by sender
    tgAnimationWidth :: Int,
    -- | Video height as defined by sender
    tgAnimationHeight :: Int,
    -- | Duration of the video in seconds as defined by sender
    tgAnimationDuration :: Int,
    -- | 	Optional. Animation thumbnail as defined by sender
    tgAnimationThumb :: Maybe TgPhotoSize,
    -- |  Optional. Original animation filename as defined by sender
    tgAnimationFileName :: Maybe String,
    -- | Optional. MIME type of the file as defined by sender
    tgAnimationMimeType :: Maybe String,
    -- | Optional. File size in bytes
    tgAnimationFileSize :: Maybe Int
  }deriving (Show, Generic)
instance FromJSON TgAnimation where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 11 }

-- TgVoice This object represents a voice note.
data TgVoice = TgVoice
  { -- | Identifier for this file, which can be used to download or reuse the file
    tgVoiceFileId :: String,
    -- | Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
    tgVoiceFileUniqueId :: String,
    -- | Duration of the audio in seconds as defined by sender
    tgVoiceDuration :: Int,
    -- | Optional. MIME type of the file as defined by sender
    tgVoiceMimeType :: Maybe String,
    -- | Optional. File size in bytes
    tgVoiceFileSize :: Int
  }deriving (Show, Generic)
instance FromJSON TgVoice where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 7 }

-- TgContact This object represents a phone contact.
data TgContact = TgContact
  { -- | Contact's phone number
   tgContactPhoneNumber :: String,
    -- | Contact's first name
   tgContactFirstName :: String,
    -- | Optional. Contact's last name
   tgContactLastName :: Maybe String,
    -- | Contact's user identifier in Telegram. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defect
    tgContactUserId :: Maybe Int,
    -- | Optional. Additional data about the contact in the form of a vCard
    tgContactVcard :: Maybe String
  }deriving (Show, Generic)
instance FromJSON TgContact where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 9 }

-- TgLocation This object represents a point on the map.
data TgLocation = TgLocation
  { -- | Longitude as defined by sender
    tgLocationLongitude :: Float,
    -- | Latitude as defined by sender
    tgLocationLatitude :: Float,
    -- | Optional. The radius of uncertainty for the location, measured in meters; 0-1500
    tgLocationHorizontalAccuracy :: Maybe Float,
    -- | Optional. Time relative to the message sending date, during which the location can be updated; in seconds.
    tgLocationLivePeriod :: Maybe Int,
    -- | Optional. The direction in which user is moving, in degrees; 1-360. For active live locations only.
    tgLocationHeading :: Maybe Int,
    -- | Optional. Maximum distance for proximity alerts about approaching another chat member, in meters. For sent live locations only.
    tgLocationProximityAlertRadius :: Maybe Int
  }deriving (Show, Generic)
instance FromJSON TgLocation where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'. drop 10 }

--}
-- | type for default config and selection FrontEndType
module ConfigurationTypes
  ( FrontEndType (..),
    ConfigDefault (..),
  )
where

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd {token :: String}

data ConfigDefault = ConfigDefault
  { -- | helpReply - helpMsg bot response for /help
    helpReply :: String,
    -- | repeatReply - repQuestion bot response for /repeat
    repeatReply :: String,
    -- | repetitionCount - default amount of times to echo a message
    repetitionCount :: Int,
    -- | stdError - loggin to Terminal or File
    stdError :: String,
    -- | minLogLevel - minimum loggin level. Must be Debug, Info, Warning, Error
    minLogLevel :: String,
    -- | frontEnd - type of bot Console or Telegram
    frontEnd :: String
  }

-- | type for default config,  FrontEndType
module ConfigurationTypes
  ( FrontEndType (..),
    ConfigDefault (..),
  )
where

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd {token :: String}

data ConfigDefault = ConfigDefault
  { helpReply :: String, -- helpMsg bot response for /help
    repeatReply :: String, -- repQuestion bot response for /repeat
    repetitionCount :: Int, -- default amount of times to echo a message
    stdError :: String, -- loggin to Terminal or File
    minLogLevel :: String, -- min loggin level. Must be Debug, Info, Warning, Error
    frontEnd :: String -- type of bot Console, Telegram
  }

-- | type for default config and selection FrontEndType
module ConfigurationTypes
  ( FrontEndType (..),
    ConfigDefault (..),
    FrontEnd (..),
    StdError (..),
    ErrorField (..),
  )
where

import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import qualified Logger
import Text.Read (readMaybe)

-- | FrontEndType use in getFrontEndType in main.hs for (to select the version of the bot to run)
data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd {token :: String}

-- | StdError  - use for choice in config. (I expect to see in the config.conf the words Terminal or File in stdError field)
data StdError = Terminal | File
  deriving (Show, Eq, Ord, Read)

instance C.Configured StdError where
  convert (C.String str) = readMaybe (T.unpack str)
  convert _ = Nothing

-- | FrontEnd   - use for choice in config. (I expect to see in the config.conf the words Console or Telegram in frontEnd field)
data FrontEnd = Console | Telegram
  deriving (Show, Eq, Ord, Read)

instance C.Configured FrontEnd where
  convert (C.String str) = readMaybe (T.unpack str)
  convert _ = Nothing

data ConfigDefault = ConfigDefault
  { -- | helpReply - helpMsg bot response for /help
    helpReply :: String,
    -- | repeatReply - repQuestion bot response for /repeat
    repeatReply :: String,
    -- | repetitionCount - default amount of times to echo a message
    repetitionCount :: Int,
    -- | stdError - log to Terminal or File
    stdError :: StdError,
    -- | minLogLevel - minimum log level. Must be Debug, Info, Warning, Error
    minLogLevel :: Logger.Level
  }

-- | ErrorField   - list of all fields in the config, which have default values.
-- Used this type when displaying an error about the unread value of the config field and for return default value by fromMaybeWithMessage function
data ErrorField = HelpReply | RepeatReply | RepetitionCount | StdError | MinLogLevel | FrontEnd

instance Show ErrorField where
  show HelpReply = "helpReply"
  show RepeatReply = "repeatReply"
  show RepetitionCount = "repetitionCount"
  show StdError = "stdError"
  show MinLogLevel = "minLogLevel"
  show FrontEnd = "frontEnd"

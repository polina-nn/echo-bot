{-# LANGUAGE OverloadedStrings #-}

-- | The pure echo-bot logic module. It doesn't know anything about
-- Telegram, other chat protocols, or any input/output. This is why we
-- can easily test it.
module EchoBot
  ( makeState,
    respond,
    getRepetitionCount,
    Event (..), --(MessageEvent),
    Response (..),
    State (..),
    Handle (..),
    Config (..),
  )
where

-- import Control.Monad (replicateM)

--import qualified Control.Monad.IO.Class
--import qualified Control.Monad.Trans.State
--import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Logger ((.<))
import qualified Logger

-- | The bot dependencies that the caller code should satisfy.
--
-- Its @m@ parameter is a monad type where all actions are performed.
-- Since the bot doesn't know what @m@ is, it cannot use arbitrary IO.
-- Everything it can do with the monad is passed as handle fields,
-- like 'hModifyState\''. For example, @m@ can be the 'IO' or 'State'
-- monad.
--
-- Its @a@ parameter is the message type. We don't impose any
-- restrictions on the message type, hence the bot can support all
-- possible message types, while 'hTextFromMessage' and
-- 'hMessageFromText' are implemented.
data Handle m a = Handle
  { hLogHandle :: Logger.Handle m,
    hConfig :: Config,
    -- | Returns the current 'State'.
    hGetState :: m State,
    -- | Updates the current state with applying the given function.
    -- You may find it similar to the 'modify'` method of the 'State'
    -- monad, as well as 'hGetState' is similar to 'get'.
    hModifyState' :: (State -> State) -> m (), -- вместо (State ->  State) -> m (),

    -- | Returns @Just text@ for a pure text message, so that the bot
    -- could try to parse it as a command. Otherwise it returns
    -- 'Nothing'.
    hTextFromMessage :: a -> Maybe T.Text,
    -- | Constructs a message from a pure text. The bot can use it for
    -- creating a help text message.
    --
    -- Note that instead of adding 'hTextFromMessage' and
    -- 'hMessageFromText' into 'Handle', we might make them into
    -- methods of @Message@ type class. These two variants are very
    -- similar for our goals.
    hMessageFromText :: T.Text -> a
  }

-- | The initial configuration of the bot.
data Config = Config
  { -- | A reply to the @help@ command
    confHelpReply :: T.Text,
    -- | A reply to the @repeat@ command. The string of @{count}@ in
    -- the text will be replaced with the current repetition count, so
    -- that you can use a template string like @"The new repetition
    -- count is {count}."@.
    confRepeatReply :: T.Text,
    -- | The initial repetition count for echoing messages to start
    -- with.
    confRepetitionCount :: Int
  }

-- | An external event that the bot should process and respond to.
-- It's parameterized with a message type.
data Event a
  = -- | The user has sent a message.
    MessageEvent a
  | -- | Set the repetition count. This event is hidden from the
    --   outside code. The caller code cannot construct it, only get
    --   from a 'MenuResponse'.
    SetRepetitionCountEvent RepetitionCount
  deriving (Eq, Show)

-- | The bot response to an event. It's parameterized with a message
-- type.
data Response a
  = -- | A command to output a message to the user.
    MessageResponse a
  | -- | A command to output a menu with the given title and options.
    -- Each option is a pair of:
    --
    -- * a new repetition amount
    -- * an event that the caller code should sent to us if the user
    --   chooses this repetition count
    MenuResponse Title [(RepetitionCount, Event a)]
  deriving (Eq, Show)

type Title = T.Text

type RepetitionCount = Int

-- Обратите внимание, что здесь нам не нужно иметь дело с несколькими пользователями,
-- мы сохраняем количество повторений только для одного пользователя.
-- Пусть код вызывающей стороны отвечает за отслеживание нескольких пользователей и состояний их ботов.

-- | An internal state of the bot.
--
-- Note that we don't have to deal with multiple users here, we only
-- keep the number of repetitions for a single user. Let the caller
-- code be responsible for tracking multiple users and states for
-- their bots.
newtype State = State
  { stRepetitionCount :: RepetitionCount
  }

-- | Creates an initial, default bot state for a user.
makeState :: Config -> Either T.Text State
makeState conf = do
  checkConfig conf
  pure State {stRepetitionCount = confRepetitionCount conf}

checkConfig :: Config -> Either T.Text ()
checkConfig conf =
  if confRepetitionCount conf < 0
    then Left "The repetition count must not be negative"
    else Right ()

getRepetitionCount :: State -> RepetitionCount
getRepetitionCount = stRepetitionCount

-- | Оценивает ответы на переданное событие
-- | Evaluates responses for the passed event.
respond :: Monad m => Handle m a -> Event a -> m [Response a]
respond h (SetRepetitionCountEvent repetitionCount) =
  handleSettingRepetitionCount h repetitionCount
respond h (MessageEvent message)
  | isCommand h "/help" message = handleHelpCommand h
  | isCommand h "/repeat" message = handleRepeatCommand h
  | otherwise = respondWithEchoedMessage h message

--  should output the predefined menu title for /repeat command
--  отправляю клавиатуру из конфига отвечающую на команду /repeat
handleRepeatCommand :: Monad m => Handle m a -> m [Response a]
handleRepeatCommand h = do
  Logger.logInfo (hLogHandle h) "Got the repeat command"
  -- hModifyState' = hModifyState
  currentState <- hGetState h
  let count = stRepetitionCount currentState -- текущие повторы
      title = confRepeatReply $ hConfig h
      menuTitle = T.replace "{count}" (T.pack $ show count) title
      options = map (\n -> (n, SetRepetitionCountEvent n)) [1 .. 5]
  return [MenuResponse menuTitle options]

handleSettingRepetitionCount :: Monad m => Handle m a -> Int -> m [Response a]
handleSettingRepetitionCount h count = do
  Logger.logInfo (hLogHandle h) $ "The user has set the repetition count to " .< count
  hModifyState' h (\_ -> State count)
  return []

{--
  let a = pack $ show count
      text = confRepeatReply $ hConfig h
      resp = append text a
  return [MessageResponse $ hMessageFromText h resp]
--}

-- OK реагирую только на команды "/help" или "/repeat"
isCommand :: Handle m a -> T.Text -> a -> Bool
isCommand h text message =
  let a = hTextFromMessage h message
   in (a == Just "/help" && text == "/help") || (a == Just "/repeat" && text == "/repeat")

-- OK отправляю эхо сообщение а несколько раз
respondWithEchoedMessage :: Monad m => Handle m a -> a -> m [Response a]
respondWithEchoedMessage h message = do
  Logger.logInfo (hLogHandle h) $
    "Echoing user input: пользователь ввел" .< fromMaybe "<multimedia?>" (hTextFromMessage h message)
  state <- hGetState h
  let repetitionCount = stRepetitionCount state
  --initialRepetition = confRepetitionCount $ hConfig h
  return $ replicate repetitionCount $ MessageResponse message

-- OK should output the help text for /help command
-- отправляю фразу из конфига отвечающую на команду /help
handleHelpCommand :: Monad m => Handle m a -> m [Response a]
handleHelpCommand h = do
  Logger.logInfo (hLogHandle h) "Got the help command"
  let helpText = hMessageFromText h $ confHelpReply $ hConfig h
  return [MessageResponse helpText]

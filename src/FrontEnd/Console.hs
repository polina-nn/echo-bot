{-# LANGUAGE OverloadedStrings #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot
import qualified Logger
import qualified Text.Read

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run h = do
  state <- EchoBot.hGetState (hBotHandle h)
  let currentRepetitionCount = EchoBot.getRepetitionCount state
  Logger.logDebug
    (EchoBot.hLogHandle (hBotHandle h))
    ( T.append "Console bot is running. Repetition count by default is " $
        T.pack $ show currentRepetitionCount
    )
  TIO.getLine
    >>= mainloop . (EchoBot.MessageEvent . EchoBot.hMessageFromText (hBotHandle h))
  where
    mainloop :: EchoBot.Event T.Text -> IO ()
    mainloop event = do
      Logger.logDebug (EchoBot.hLogHandle (hBotHandle h)) "mainloop: call"
      responses <- EchoBot.respond (hBotHandle h) event
      mapM_ (printRespond h) responses
      TIO.getLine
        >>= mainloop
          . (EchoBot.MessageEvent . EchoBot.hMessageFromText (hBotHandle h))

printRespond :: Handle -> EchoBot.Response T.Text -> IO ()
printRespond h = help
  where
    help :: EchoBot.Response T.Text -> IO ()
    help (EchoBot.MessageResponse a) = TIO.putStrLn a
    help (EchoBot.MenuResponse title options) = do
      TIO.putStrLn title
      textNumber <- TIO.getLine
      let maybeNumber = Text.Read.readMaybe (T.unpack textNumber) :: (Maybe Int)
      case maybeNumber of
        Just number ->
          if 1 <= number && number <= 5
            then do
              let maybeEvent = Map.lookup number $ Map.fromList options
              _ <- maybe (pure []) (EchoBot.respond (hBotHandle h)) maybeEvent
              return ()
            else do
              Logger.logWarning
                (EchoBot.hLogHandle (hBotHandle h))
                "The user enters a number, but not in the range 1 to 5"
              TIO.putStrLn
                "The user enters a number, but not in the range 1 to 5. Use the old number of repetitions"
              return ()
        Nothing -> do
          Logger.logWarning
            (EchoBot.hLogHandle (hBotHandle h))
            "The user enters not a number"
          TIO.putStrLn
            "The user enters not a number. Use the old number of repetitions"
          return ()

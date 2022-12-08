{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl
  ( withHandle
  , Config(..)
  ) where

import qualified Data.Text as T
import qualified Data.Time as TM
import qualified Logger
import qualified System.IO

data Config =
  Config
    -- | A file handle to output formatted log messages to with
    -- 'System.IO.hPutStrLn' or 'Data.Text.IO.hPutStrLn'. For example,
    -- it might be 'System.IO.stderr' or a handle of a regular open
    -- file.
    { confFileHandle :: System.IO.Handle
    -- | The minimum level of a printable log message. Messages with
    -- lower levels should not be printed.
    , confMinLevel :: Logger.Level
    }

withHandle :: Config -> (Logger.Handle IO -> IO ()) -> IO ()
withHandle config f = f Logger.Handle {Logger.hLowLevelLog = logWith config}

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith сonf logLevel text = do
  let minLevel = confMinLevel сonf
  if logLevel >= minLevel
    then do
      today <- currentDay
      time <- currentTime
      let message =
            show today ++
            " " ++
            time ++
            " [" ++
            show (confMinLevel сonf) ++
            "] " ++ "[" ++ show logLevel ++ "] " ++ T.unpack text 
      System.IO.hPutStrLn (confFileHandle сonf) message
    else pure ()

currentDay :: IO TM.Day
currentDay = fmap TM.utctDay TM.getCurrentTime

currentTime :: IO String
currentTime = do
  now <- TM.getCurrentTime
  timezone <- TM.getCurrentTimeZone
  let (TM.TimeOfDay hour minute sec) =
        TM.localTimeOfDay $ TM.utcToLocalTime timezone now
  return
    (show hour ++
     ":" ++ show minute ++ ":" ++ show (div (fromEnum sec) 1000000000000))

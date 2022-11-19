-- | write TgRepeats = Map.Map TgTypes.ChatId Repeats  in repeats file
-- and read from the file or config (if the file is empty)
module FrontEnd.TelegramRepeats
  ( readTgRepeats,
    writeTgRepeat,
    TgRepeats,
    Repeats,
  )
where

import qualified Config
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified EchoBot
import qualified FrontEnd.TelegramTypes as TgTypes
import qualified Logger

-- | TgRepeats message repetitions. Write repetitions to a file repeats
type TgRepeats = Map.Map TgTypes.ChatId Repeats

type Repeats = Int

-- |  read from the TgRepeats file or from the config if the file is empty
readTgRepeats :: TgTypes.ChatId -> IO Repeats
readTgRepeats chart = do
  repeatsFile <- TI.readFile "repeats"
  if repeatsFile == T.empty
    then do
      putStrLn "readTgRepeats: file repeats is empty. Read from config"
      Config.getRepetitionCount
    else do
      putStrLn "readTgRepeats: read repeats from file repeats"
      getTgRep chart (loadTgRepeat repeatsFile)

-- | read file and return initial amount of TgRepeat
loadTgRepeat :: T.Text -> TgRepeats
loadTgRepeat sx = read (T.unpack (last $ T.lines sx))

writeTgRepeat :: EchoBot.Handle IO T.Text -> TgTypes.ChatId -> Repeats -> IO ()
writeTgRepeat h chart rep = do
  repeatsFile <- TI.readFile "repeats"
  if repeatsFile == T.empty
    then do
      let newMap = Map.fromList [(chart, rep)]
      Logger.logDebug (EchoBot.hLogHandle h) $
        T.pack $
          "writeTgRepeat"
            ++ show newMap
            ++ " write TgRepeat, because file repeats was empty"
            ++ "\n"
      appendFile "repeats" $ show newMap ++ "\n"
    else do
      let oldMap = loadTgRepeat repeatsFile
          newMap = Map.insert chart rep oldMap
      Logger.logDebug (EchoBot.hLogHandle h) $
        T.pack $ "writeTgRepeat " ++ show newMap ++ "write to file" ++ "\n"
      appendFile "repeats" $ show newMap ++ "\n"

-- | getTgRep seach repeats for user (their chatId)
-- read from config, if it could not seach
getTgRep :: TgTypes.ChatId -> TgRepeats -> IO Repeats
getTgRep ch tgRep = maybe Config.getRepetitionCount return (Map.lookup ch tgRep)

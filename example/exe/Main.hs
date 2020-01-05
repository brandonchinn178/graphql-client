import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

import Example

main :: IO ()
main = do
  song <- fromMaybe "Smells Like Teen Spirit" . listToMaybe <$> getArgs
  results <- runApp (searchForSong song)
  mapM_ (putStrLn . showRecording) results

module InputDownloader (runFetchInputToFile) where

import Control.Monad ()
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN, runStdoutLoggingT)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Network.HTTP.Simple
import System.Directory
import System.Environment (getArgs, lookupEnv)

checkFileExistsWithData :: FilePath -> IO Bool
checkFileExistsWithData fp = do
  exists <- doesFileExist fp
  if not exists
    then return False
    else do
      size <- getFileSize fp
      return $ size > 0

fetchInputToFile :: (MonadLogger m, MonadThrow m, MonadIO m) => Int -> Int -> FilePath -> m ()
fetchInputToFile year day filepath = do
  isCached <- liftIO $ checkFileExistsWithData filepath
  token' <- liftIO $ lookupEnv "AOC_TOKEN"
  case (isCached, token') of
    (True, _) -> logDebugN "Input is cached!"
    (False, Nothing) -> logErrorN "Not cached but didn't find session token!"
    (False, Just token) -> do
      let route = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
      baseRequest <- parseRequest route
      let finalRequest = addRequestHeader "cookie" (B.pack token) baseRequest
      {- Send request, retrieve body from response -}
      -- logDebugN $ T.pack $ show finalRequest
      response <- getResponseBody <$> httpBS finalRequest
      {- Write body to the file -}
      logInfoN "Download succesful"
      liftIO $ B.writeFile filepath response

runFetchInputToFile :: (MonadIO m, MonadThrow m) => Int -> Int -> FilePath -> m ()
runFetchInputToFile year day filepath = runStdoutLoggingT (fetchInputToFile year day filepath)

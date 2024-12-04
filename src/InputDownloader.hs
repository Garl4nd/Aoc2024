module InputDownloader (runFetchProblemDataToFiles) where

import Control.Monad (when, void)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Monad.Logger (MonadLogger, logDebugN, logErrorN, logInfoN, runStdoutLoggingT)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding


import Network.HTTP.Simple
    ( parseRequest, addRequestHeader, getResponseBody, httpBS )
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
      if T.isPrefixOf "Puzzle inputs differ by user." $ T.Encoding.decodeUtf8  response  then 
        logErrorN "Couldn't sign in! The input file wasn't saved" 
      else do 
        logInfoN "Input download successful"
        liftIO $ B.writeFile filepath response



fetchDescriptionToFile :: (MonadLogger m, MonadThrow m, MonadIO m) => Int -> Int -> FilePath -> m ()
fetchDescriptionToFile year day filepath = do
  isCached <- liftIO $ checkFileExistsWithData filepath  
  if isCached then logDebugN "Description is cached!"
  else 
    do
      let route = "https://adventofcode.com/" <> show year <> "/day/" <> show day 
      request <- parseRequest route      
      response <- getResponseBody <$> httpBS request
      let modifiedResponse = addCorrectCSS response              
      {- Write body to the file -}
      logInfoN "Description download successful"
      liftIO $ B.writeFile filepath modifiedResponse

addCorrectCSS :: B.ByteString -> B.ByteString 
addCorrectCSS bs = let textLines = T.lines $ T.Encoding.decodeUtf8 bs 
                       cssLine =  T.pack "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>"                         
                       modifiedText = T.unlines $ take 5 textLines ++ [cssLine] ++ drop 5 textLines
                    in T.Encoding.encodeUtf8  modifiedText

runFetchProblemDataToFiles :: (MonadIO m, MonadThrow m) => Int -> Int -> FilePath -> FilePath -> m ()
runFetchProblemDataToFiles year day inputFilepath descriptionFilePath = runStdoutLoggingT (fetchInputToFile year day inputFilepath) >> runStdoutLoggingT (fetchDescriptionToFile year day descriptionFilePath)

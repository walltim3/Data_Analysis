module Main where

import Control.Monad
import Data.List (find)
import Data.Maybe
import Network.HTTP.Conduit
import System.Exit
import System.Environment
import System.IO as SIO
import Text.XML.Light
import Data.ByteString.Lazy.Char8  as UC (ByteString, putStrLn, pack, writeFile, empty)
import Main.Utf8
import Data.List.Split
import System.Directory (createDirectoryIfMissing)
import Control.Exception as X
import Data.Time

readDataFrom fileHandle = 
  do 
    isFileEnd <- hIsEOF fileHandle
    if isFileEnd 
      then
        return ("")
      else
        do
          url <- hGetLine fileHandle
          writeRss $ url
          readDataFrom fileHandle

statusExceptionHandler ::  X.SomeException -> IO UC.ByteString
statusExceptionHandler e = putStr "An error occured during downloading\n" >> (return UC.empty)

writeRss url = 
    do 
      createDirectoryIfMissing True $ "./RSS_FEEDS"
      let name = splitOn "/" url
      time <- getCurrentTime
      let timeStamp = head(splitOn " " (show time)) -- ++ "_" ++ head(tail(splitOn " " (show time)))
      resp <- (simpleHttp url) `X.catch` statusExceptionHandler
      case resp of x | x == UC.empty ->  UC.writeFile ("./RSS_FEEDS/" ++ head(tail(tail(name))) ++ "_" ++ timeStamp ++ ".xml") (UC.pack $ "Фід недоступний!")    
                     | otherwise     ->  UC.writeFile ("./RSS_FEEDS/" ++ head(tail(tail(name))) ++ "_" ++ timeStamp ++ ".xml") (resp)

    
main = withUtf8 $ do
  fileHandle <- openFile "rssFeeds.txt" ReadMode
  readDataFrom fileHandle

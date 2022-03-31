module Main where

import Control.Monad
import Data.List (find)
import Data.Maybe
import Network.HTTP
import Network.Stream
import System.Exit
import System.Environment
import System.IO as SIO
import Text.XML.Light
import Data.ByteString.Char8 as UC (putStrLn, pack, writeFile)
import Main.Utf8
import Data.List.Split
import System.Directory (createDirectoryIfMissing)


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


writeRss url = 
    do 
      resp <- simpleHTTP (getRequest url)
      code <- getResponseCode resp
      if code == (2,0,0)
        then do
          body <- getResponseBody resp
          createDirectoryIfMissing True $ "./RSS_FEEDS"
          UC.writeFile ("./RSS_FEEDS/" ++ last(splitOn "/" url)) (UC.pack $ body)
        else
          UC.writeFile ("./RSS_FEEDS/" ++last(splitOn "/" url)) (UC.pack $ "Фід недоступний!")

    
main = withUtf8 $ do
  fileHandle <- openFile "rssFeeds.txt" ReadMode
  readDataFrom fileHandle

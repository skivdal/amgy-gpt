{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import LLM (askGpt)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles
import System.IO
import System.Environment (lookupEnv)
import Control.Monad.IO.Class (liftIO)
import OpenAI.Client (chmContent, chchMessage, cchText, chrChoices)
import Data.Text (unpack)
import Data.Maybe
import System.Posix.Files

type AmgyAPI =
  "api"    :> "chat" :> ReqBody '[JSON] Conversation :> Post '[JSON] NextMessage :<|>
  "static" :> Raw :<|>
  "chat"   :> Raw :<|>
  Raw -- NOTE: this apparently always matches...

newtype Conversation = Conversation [String]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype NextMessage = NextMessage
  { msg :: String
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

amgyApi :: Proxy AmgyAPI
amgyApi = Proxy

main :: IO ()
main = do
  envAddr <- lookupEnv "SOCKET_ADDR"
  let settings = setServerName ""
        defaultSettings

  case envAddr of
    -- devel mode
    Nothing -> do
      let host = "127.0.0.1"
          port = 3000
          devSettings =
            setHost host $
            setPort port $
            setBeforeMainLoop (putStrLn ("Listening on " ++ show host ++ ":" ++ show port))
            settings

      runSettings devSettings =<< mkApp

    -- prod mode
    Just addr -> do
      let prodSettings =
            setBeforeMainLoop (putStrLn ("Listening on " ++ addr))
            settings

      sock <- socket AF_UNIX Stream 0
      bind sock $ SockAddrUnix addr

      setFileMode addr (unionFileModes (unionFileModes ownerModes groupModes) otherModes)

      listen sock maxListenQueue

      runSettingsSocket prodSettings sock =<< mkApp

      close sock

mkApp :: IO Application
mkApp = return $ serve amgyApi server

server :: Server AmgyAPI
server =
    doChat
    :<|> serveDirectoryWebApp "static"
    :<|> getConvPage
    :<|> getRoot

getRoot :: Tagged Handler Application
getRoot = Tagged $ \_ res -> res $ responseFile status200 [(hContentType, "text/html")] "templates/index.html" Nothing

getConvPage :: Tagged Handler Application
getConvPage = Tagged $ \_ res -> res $ responseFile status200 [(hContentType, "text/html")] "templates/chat.html" Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

doChat :: Conversation -> Handler NextMessage
doChat (Conversation c) = do
  x <- liftIO $ askGpt c
  case x of
    Left err -> do
      liftIO $ print err
      throwError err500
    Right success -> do
      let res = safeHead (chrChoices success) >>= (Just . chchMessage) >>= chmContent >>= (Just . unpack)

      case res of
        Nothing -> do
          liftIO $ print res
          throwError err500
        Just y -> return NextMessage {msg=y}


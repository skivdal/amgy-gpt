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
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles
import System.IO
import Control.Monad.IO.Class (liftIO)
import OpenAI.Client (chmContent, chchMessage, cchText, chrChoices)
import Data.Text (unpack)
import Data.Maybe

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
  let port = 3000
      settings =
        setPort port $
          setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
            defaultSettings
  runSettings settings =<< mkApp

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


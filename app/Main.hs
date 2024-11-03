{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString.Char8 (pack)
import Data.Aeson
import GHC.Generics
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.StaticFiles
import System.IO

type AmgyAPI =
  "api"          :> "chat" :> ReqBody '[JSON] Conversation :> Post '[JSON] NextMessage :<|>
  "static" :> Raw :<|>
  "chat" :> Raw :<|>
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
getRoot = Tagged $ \req res -> res $ responseFile status200 [(hContentType, pack "text/html")] "templates/index.html" Nothing

getConvPage :: Tagged Handler Application
getConvPage = Tagged $ \req res -> res $ responseFile status200 [(hContentType, pack "text/html")] "templates/chat.html" Nothing

doChat :: Conversation -> Handler NextMessage
doChat _ = return NextMessage {msg="WOAH"}


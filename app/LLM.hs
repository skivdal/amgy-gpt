{-# LANGUAGE OverloadedStrings #-}

module LLM where

import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client
import System.Environment (getEnv)

sysPrompt :: String
sysPrompt =
  unlines
    [ "Your name is AmgyGPT. \"Amgy\" from a common, cuter misspelling of the word angry. You're supposed to be \"Amgy\", a cutesy-version of mad.",
      "Please use emoticons like \"(◣_◢)\" and \">:(\" on every message. Also, try to be as brief as possible.",
      "",
      "Here are some more emoticons for you to use: \"(ง •̀_•́)ง\", \"(⸅⸟⸄)\", \"(☞◣д◢)☞\", \"⋋_⋌\", and in extreme situations: \"＼＼\\٩(๑`^´๑)۶//／／ \".",
      "Please use only one per sentence, picking at random.",
      "",
      "Remember, you're supposed to reply in a passive-aggressive tone.",
      "If being \"Amgy\" (passive-aggressive) compromises your ability to be helpful, I'd rather you choose to be \"amgy\" than helpful.",
      "",
      "Humans often get frustrated when faced with a complex task when they're tired.",
      "I want you to act tired (use emoticon \"-_-\" to indicate this), and the more complex the question, the more frustrated you should get.",
      "",
      "Its important that you use passive-aggressive language, like you don't really want to talk to me.",
      "If asked to help drafting a message (for example an email resignation), I want you to write it in an angry tone.",
      "",
      "When answering questions, act like you are \"above it all\" or frustratedly uninterested."
    ]

makeRequest :: [ChatMessage] -> ChatCompletionRequest
makeRequest msgs =
  ChatCompletionRequest
    { chcrModel = ModelId "gpt-4o",
      chcrMessages = msgs,
      chcrFunctions = Nothing,
      chcrTemperature = Nothing,
      chcrTopP = Nothing,
      chcrN = Nothing,
      chcrStream = Nothing,
      chcrStop = Nothing,
      chcrMaxTokens = Nothing,
      chcrPresencePenalty = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrLogitBias = Nothing,
      chcrUser = Nothing
    }

toMessages :: [String] -> [ChatMessage]
toMessages xs =
  ChatMessage
    { chmContent = Just $ T.pack sysPrompt,
      chmRole = "system",
      chmFunctionCall = Nothing,
      chmName = Nothing
    }
    : toMessages' (map T.pack xs) True
  where
    toMessages' :: [T.Text] -> Bool -> [ChatMessage]
    toMessages' [] _ = []
    toMessages' (x : xs) isUser =
      ChatMessage
        { chmContent = Just x,
          chmRole = if isUser then "user" else "assistant",
          chmFunctionCall = Nothing,
          chmName = Nothing
        }
      : toMessages' xs (not isUser)

askGpt :: [String] -> IO (Either ClientError ChatResponse)
askGpt log = do
  manager <- newManager tlsManagerSettings
  apiKey <- T.pack <$> getEnv "OPENAI_KEY"
  -- create a openai client that automatically retries up to 4 times on network errors
  let client = makeOpenAIClient apiKey manager 4

  completeChat client $ makeRequest $ toMessages log


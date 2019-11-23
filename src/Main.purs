module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (readJSON)
import TelegramBot (connect, onMessage, sendMessage)

type Config = {token :: String}

main :: Effect Unit
main = do
  c <- map readJSON (readTextFile UTF8 "./config.json")
  case c of
    Left e ->
      log $ "config.json is malformed: " <> show e
    Right (config :: Config) -> do

      bot <- connect config.token
      onMessage bot callback
      where
        callback m
          | Right message <- runExcept m = logShow message
          | otherwise = pure unit

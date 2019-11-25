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

{-

Commands we want to support:
- /add AMOUNT REASON: records an expense for AMOUNT and REASON on the current user. Might return balance
- /balance: returns the amount of expenses for both in the current period
- /payout: prints out how much someone should pay to the other and resets the balance

Details:
- we'll record transactions on a file and calculate the balance on the fly
- we'll record an "audit log" of commands that are issued

-}


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

module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (fromNonEmpty)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Foldable as Array
import Data.List (List, toUnfoldable)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Number as Numbers
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (readJSON)
import TelegramBot (connect, onMessage, sendMessage)
import Text.Parsing.StringParser (ParseError(..), fail, runParser)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, satisfy, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, many, many1, optionMaybe)

type Config =
  { token :: String
  , usernames :: Array String
  }

type Transaction =
  { amount :: Number
  , reason :: String
  , issuer :: String
  , date :: Int
  }

data Command
  -- | records an expense for AMOUNT and REASON on the current user.
  = Add Transaction
  -- | returns the amount of expenses for both in the current period
  | Balance
  -- | prints out how much someone should pay to the other and resets balance
  | Payout

instance showCommand :: Show Command where
  show Balance = "Balance"
  show Payout = "Payout"
  show (Add transaction) = "Add " <> show transaction

{-

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
          | Right message <- runExcept m
          , Just from <- message.from
          , Just username <- from.username
          , Just user <- Array.find (username == _) config.usernames
          , Just text <- message.text = case parseCommand text user message.date of
            Right command -> logShow command
            Left errors -> logShow errors
          | otherwise = pure unit

parseCommand :: String -> String -> Int -> Either ParseError Command
parseCommand text username timestamp = do
  runParser parser text
  where
    parser
      = parseBalance
      <|> parsePayout
      <|> parseAdd
      <|> fail ("ERROR: could not parse command: " <> text)

    parseBalance = string "/balance" *> pure Balance

    parsePayout = string "/payout" *> pure Payout

    parseAdd = do
      void $ string  "/add"
      skipSpaces
      amountString <- map (fromCharArray <<< toUnfoldable <<< List.fromFoldable)
        $ many1 (choice [anyDigit, satisfy (_ == '.')])
      amount <- case Numbers.fromString amountString of
        Just n -> pure n
        Nothing -> fail ("ERROR: could not parse amount " <> amountString)
      skipSpaces
      reason <- map (fromCharArray <<< toUnfoldable) $ many anyChar
      pure $ Add {amount, reason, issuer: username, date: timestamp}

module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Foldable as Array
import Data.List (List, toUnfoldable)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number as Numbers
import Data.Number.Format (toString)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Simple.JSON (readJSON, writeJSON)
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

TODO:
- read transaction file to calculate balance
- answer to add command confirming the addition
- answer to balance command
- write audit log of commands

-}

transactionFile =  "./transactions.json"

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
          , Just text <- message.text = runMessage text user message.date
          | otherwise = pure unit

runMessage :: String -> String -> Int -> Effect Unit
runMessage text user timestamp = case parseCommand text user timestamp of
  Right command -> do
    log $ "Running command: " <> show command
    runCommand command
  Left errors -> logShow errors

withTransactions :: (Array Transaction -> Effect Unit) -> Effect Unit
withTransactions action = do
  whenM (map not $ exists transactionFile) do
    log "Transaction file doesn't exist, creating.."
    writeTextFile UTF8 transactionFile "[]"
  maybeTransactions <- map readJSON (readTextFile UTF8 transactionFile)
  case maybeTransactions of
    Left e -> log $ "ERROR: transactions.json is malformed: " <> show e
    Right transactions -> action transactions

runCommand :: Command -> Effect Unit
runCommand Balance = withTransactions \transactions -> do
  let groupedTransactions = Array.groupBy (\a b -> a.issuer == b.issuer)
                              $ Array.sortWith (_.issuer) transactions

  totals <- for groupedTransactions \transactions -> do
    let issuer = _.issuer $ NonEmpty.head transactions
    let total = Array.foldr (+) 0.0 $ map _.amount transactions
    pure { issuer, total }

  log $ "Current balance: " <> show totals
  pure unit
runCommand Payout = pure unit
runCommand (Add transaction) = withTransactions \transactions -> do
  log "Writing new transaction to file.."
  let newTransactions = Array.cons transaction transactions
  writeTextFile UTF8 transactionFile $ writeJSON newTransactions

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

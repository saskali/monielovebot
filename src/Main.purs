module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number as Numbers
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Simple.JSON (readJSON, writeJSON)
import TelegramBot as Telegram
import Text.Parsing.StringParser (ParseError, fail, runParser)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, satisfy, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, many, many1)

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

-- | This holds the reference to our current bot/chat around the app,
-- | so we can send messages back
type TelegramCoords =
  { bot :: Telegram.Bot
  , chatID :: Int
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
- answer to add command confirming the addition
- answer to balance command
- write audit log of commands

-}

transactionFile :: String
transactionFile =  "./transactions.json"

main :: Effect Unit
main = do
  c <- map readJSON (readTextFile UTF8 "./config.json")
  case c of
    Left e ->
      log $ "config.json is malformed: " <> show e
    Right (config :: Config) -> do

      bot <- Telegram.connect config.token
      Telegram.onMessage bot $ callback bot
      where
        callback bot m
          | Right message <- runExcept m
          , Just from <- message.from
          , Just username <- from.username
          , Just user <- Array.find (username == _) config.usernames
          , Just text <- message.text = case parseCommand text user message.date of
            Right command -> do
              log $ "Running command: " <> show command
              runCommand { bot, chatID: message.chat.id } command
            Left errors -> logShow errors
          | otherwise = pure unit

withTransactions :: (Array Transaction -> Effect Unit) -> Effect Unit
withTransactions action = do
  whenM (map not $ exists transactionFile) do
    log "Transaction file doesn't exist, creating.."
    writeTextFile UTF8 transactionFile "[]"
  maybeTransactions <- map readJSON (readTextFile UTF8 transactionFile)
  case maybeTransactions of
    Left e -> log $ "ERROR: transactions.json is malformed: " <> show e
    Right transactions -> action transactions

runCommand :: TelegramCoords -> Command -> Effect Unit
runCommand telegram command = withTransactions \transactions -> do
  let send = Telegram.sendMessage telegram.bot telegram.chatID
  case command of
    Balance -> do
      let groupedTransactions = Array.groupBy (\a b -> a.issuer == b.issuer)
                                $ Array.sortWith (_.issuer) transactions

      totals <- for groupedTransactions \userTransactions -> do
        let issuer = _.issuer $ NonEmpty.head userTransactions
        let total = Array.foldr (+) 0.0 $ map _.amount userTransactions
        pure { issuer, total }
      let formatBalance { issuer, total } = "* " <> issuer <> ": " <> show total <> "\n"

      send $ "Current balance:\n" <> Array.fold (map formatBalance totals)

    Payout -> pure unit

    Add transaction -> do
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
      amountString <- map (fromCharArray <<< List.toUnfoldable <<< List.fromFoldable)
        $ many1 (choice [anyDigit, satisfy (_ == '.')])
      amount <- case Numbers.fromString amountString of
        Just n -> pure n
        Nothing -> fail ("ERROR: could not parse amount " <> amountString)
      skipSpaces
      reason <- map (fromCharArray <<< List.toUnfoldable) $ many anyChar
      pure $ Add {amount, reason, issuer: username, date: timestamp}

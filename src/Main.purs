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
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
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
- answer to balance command
- write audit log of commands

-}

transactionFile :: String
transactionFile =  "./transactions.json"

main :: Effect Unit
main = do
  c <- map readJSON (FS.readTextFile UTF8 "./config.json")
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
  whenM (map not $ FS.exists transactionFile) do
    log "Transaction file doesn't exist, creating.."
    FS.writeTextFile UTF8 transactionFile "[]"
  maybeTransactions <- map readJSON (FS.readTextFile UTF8 transactionFile)
  case maybeTransactions of
    Left e -> log $ "ERROR: transactions.json is malformed: " <> show e
    Right transactions -> action transactions

runCommand :: TelegramCoords -> Command -> Effect Unit
runCommand telegram command = withTransactions \transactions -> do
  let send msg = Telegram.sendMessageWithOptions telegram.bot telegram.chatID msg { parse_mode: "Markdown" }
  let getUserBalance userTransactions =
        { issuer: _.issuer $ NonEmpty.head userTransactions
        , total: Array.foldr (+) 0.0 $ map _.amount userTransactions
        }
  let getBalances
        = map getUserBalance
        <<< Array.groupBy (\a b -> a.issuer == b.issuer)
        <<< Array.sortWith (_.issuer)

  let formatBalance { issuer, total } = "- " <> issuer <> ": " <> show total <> "\n"
  let differenceText = case Array.sortWith (_.total) (getBalances transactions) of
        [leastSpender, mostSpender] ->
          let diff = mostSpender.total - leastSpender.total
          in "`" <> leastSpender.issuer <> "` spent " <> show diff <> " more than `" <> mostSpender.issuer <> "`"
        _ -> "Stuff's broken yo"

  case command of
    Balance ->
      send $ "Current balance:\n" <> Array.fold (map formatBalance $ getBalances transactions)

    Payout ->
      case Array.sortWith (_.total) (getBalances transactions) of
        [leastSpender, mostSpender] -> do
          let diff = mostSpender.total - leastSpender.total
          send $ "Current balance:\n" <> Array.fold (map formatBalance $ getBalances transactions)
          send $ "`" <> leastSpender.issuer <> "` owes " <> show (diff / 2.0) <> " to `" <> mostSpender.issuer <> "`"
          FS.unlink transactionFile
        _ -> send "Stuff's broken yo"

    Add transaction -> do
      log "Writing new transaction to file.."
      let newTransactions = Array.cons transaction transactions
      FS.writeTextFile UTF8 transactionFile $ writeJSON newTransactions
      send $ "You have added " <> show transaction.amount <> " for " <> show transaction.reason <> "\n" <> differenceText

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

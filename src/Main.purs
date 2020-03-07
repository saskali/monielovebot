module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Number as Numbers
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils as String
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Simple.JSON as Json
import TelegramBot as Telegram
import Text.Parsing.StringParser (ParseError, fail, runParser)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, satisfy, skipSpaces, string)
import Text.Parsing.StringParser.Combinators (choice, many, many1)

type Config =
  { token :: String
  , usernames :: Array String
  }

-- | An entry of the "transaction log"
type Transaction =
  { amount :: Number
  , reason :: String
  , issuer :: String
  , date :: Int
  }

-- | An entry of the "command log"
type Entry =
  { command :: Command
  , timestamp :: Int
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
  -- | prints out log of all transactions
  | Log


derive instance genericCommand :: Generic Command _

instance showCommand :: Show Command where
  show = genericShow

instance writeForeignCommand :: Json.WriteForeign Command where
  writeImpl = genericSumToVariant >>> Json.writeImpl

instance readForeignCommand :: Json.ReadForeign Command where
  readImpl = map variantToGenericSum <<< Json.readImpl


{-

TODO:
- answer to balance command

-}

commandLog :: String
commandLog = "commandlog.jsonl"


main :: Effect Unit
main = do
  c <- map Json.readJSON (FS.readTextFile UTF8 "./config.json")
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
              runCommand { bot, chatID: message.chat.id } { command, timestamp: message.date }
            Left errors -> logShow errors
          | otherwise = pure unit


-- | Read and parse the Json-Lines log of commands into memory
readJsonlFile :: forall a. Json.ReadForeign a => String -> Effect (Array a)
readJsonlFile filename = do
  lines <- map String.lines $ FS.readTextFile UTF8 filename
  pure $ Array.mapMaybe parseLine lines
  where
    parseLine line = case Json.readJSON line of
      Left _ -> Nothing
      Right e -> e


runCommand :: TelegramCoords -> Entry -> Effect Unit
runCommand telegram entry = do
  -- First, let's print the command out
  log ("New command: " <> show entry.command)
  -- Then append it to the audit log
  FS.appendTextFile UTF8 commandLog (Json.writeJSON entry <> "\n")
  -- At this point we're safe to read all the entries from the command log
  entries <- readJsonlFile commandLog
  let transactions = transactionsFromLog entries

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
          in "`" <> mostSpender.issuer <> "` spent " <> show diff <> " more than `" <> leastSpender.issuer <> "`"
        _ -> "Stuff's broken yo"

  case entry.command of
    Balance ->
      send $ "Current balance:\n" <> Array.fold (map formatBalance $ getBalances transactions)

    Payout ->
      case Array.sortWith (_.total) (getBalances transactions) of
        [leastSpender, mostSpender] -> do
          let diff = mostSpender.total - leastSpender.total
          send $ "Current balance:\n" <> Array.fold (map formatBalance $ getBalances transactions)
          send $ "`" <> leastSpender.issuer <> "` owes " <> show (diff / 2.0) <> " to `" <> mostSpender.issuer <> "`"
          FS.rename commandLog (commandLog <> ".bak")
        _ -> send "Stuff's broken yo"

    Add transaction -> do
      send $ "You have added " <> show transaction.amount <> " for " <> show transaction.reason <> "\n" <> differenceText

    Log -> do
      log "printing log of transactions"
      send $ formatLog transactions

formatLog :: Array Transaction -> String
formatLog transactions = foldMap formatTransaction transactions
  where
    formatTransaction :: Transaction -> String
    formatTransaction {issuer, amount, reason, date}
      = "- " <> issuer <> " spent " <> show amount <> "€ for " <> reason <> " on the " <> show date <> "\n"


transactionsFromLog :: Array Entry -> Array Transaction
transactionsFromLog = Array.mapMaybe fromEntry
  where
    fromEntry { command } = case command of
      Add transaction -> Just transaction
      _ -> Nothing


parseCommand :: String -> String -> Int -> Either ParseError Command
parseCommand text username timestamp = do
  runParser parser text
  where
    parser
      = parseBalance
      <|> parsePayout
      <|> parseAdd
      <|> parseLog
      <|> fail ("ERROR: could not parse command: " <> text)

    parseBalance = string "/balance" *> pure Balance

    parsePayout = string "/payout" *> pure Payout

    parseLog = string "/log" *> pure Log

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

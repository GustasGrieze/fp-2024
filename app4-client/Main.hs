{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Control.Monad.State (State, runState, get, put)
import Network.Wreq (post, responseBody, Response)
import System.Environment (getArgs)
import Control.Lens ((^.))
import qualified Lib2
import qualified Data.ByteString.Lazy.Char8 as BL

-- | DSL for card-related commands
-- Only viewDeck returns a String result. All other commands return ().
data Command next
  = ViewDeckCmd (String -> next)
  | AddDeckCmd Lib2.Deck next
  | DeleteDeckCmd next
  | CountDeckCmd next
  | DrawCardCmd next
  | ShuffleDeckCmd next
  deriving (Functor)

type CardDSL = Free Command

-- Smart constructors for the DSL
viewDeck :: CardDSL String
viewDeck = liftF (ViewDeckCmd id)

addDeck :: Lib2.Deck -> CardDSL ()
addDeck deck = liftF (AddDeckCmd deck ())

deleteDeck :: CardDSL ()
deleteDeck = liftF (DeleteDeckCmd ())

countDeck :: CardDSL ()
countDeck = liftF (CountDeckCmd ())

drawCard :: CardDSL ()
drawCard = liftF (DrawCardCmd ())

shuffleDeck :: CardDSL ()
shuffleDeck = liftF (ShuffleDeckCmd ())

-- Post helper that takes a String and sends as ByteString
postCmd :: String -> IO (Response BL.ByteString)
postCmd cmd = post "http://localhost:3000" (BL.pack cmd)

-- SINGLE REQUEST PER COMMAND INTERPRETER
-- Each command is sent immediately as its own request.
runHttpSingle :: CardDSL a -> IO a
runHttpSingle (Pure a) = return a
runHttpSingle (Free (ViewDeckCmd next)) = do
  putStrLn "Sending request: view"
  resp <- postCmd "view"
  let res = BL.unpack (resp ^. responseBody)
  runHttpSingle (next res)

runHttpSingle (Free (AddDeckCmd deck next)) = do
  let cmd = "add " ++ show deck
  putStrLn ("Sending request: " ++ cmd)
  _ <- postCmd cmd
  runHttpSingle next

runHttpSingle (Free (DeleteDeckCmd next)) = do
  putStrLn "Sending request: delete"
  _ <- postCmd "delete"
  runHttpSingle next

runHttpSingle (Free (CountDeckCmd next)) = do
  putStrLn "Sending request: count"
  _ <- postCmd "count"
  runHttpSingle next

runHttpSingle (Free (DrawCardCmd next)) = do
  putStrLn "Sending request: draw"
  _ <- postCmd "draw"
  runHttpSingle next

runHttpSingle (Free (ShuffleDeckCmd next)) = do
  putStrLn "Sending request: shuffle"
  _ <- postCmd "shuffle"
  runHttpSingle next

-- BATCHING INTERPRETER
-- Commands that do not produce immediate output are batched together.
-- 'viewDeck' flushes the batch and then requests its result separately.
runHttpBatch :: CardDSL a -> IO a
runHttpBatch = runHttpBatch' []

-- Helper for batching: flush batch to server
flushBatch :: [String] -> IO ()
flushBatch [] = return ()
flushBatch cmds = do
  putStrLn "Sending batch request:"
  mapM_ putStrLn cmds
  _ <- postCmd (unlines cmds)
  return ()

runHttpBatch' :: [String] -> CardDSL a -> IO a
runHttpBatch' acc (Pure a) = do
  flushBatch acc
  return a

runHttpBatch' acc (Free (ViewDeckCmd next)) = do
  -- Flush batch first, since we need immediate response for 'view'
  flushBatch acc
  putStrLn "Sending request: view"
  resp <- postCmd "view"
  let res = BL.unpack (resp ^. responseBody)
  runHttpBatch' [] (next res)

runHttpBatch' acc (Free (AddDeckCmd deck next)) =
  runHttpBatch' (acc ++ ["add " ++ show deck]) next

runHttpBatch' acc (Free (DeleteDeckCmd next)) =
  runHttpBatch' (acc ++ ["delete"]) next

runHttpBatch' acc (Free (CountDeckCmd next)) =
  runHttpBatch' (acc ++ ["count"]) next

runHttpBatch' acc (Free (DrawCardCmd next)) =
  runHttpBatch' (acc ++ ["draw"]) next

runHttpBatch' acc (Free (ShuffleDeckCmd next)) =
  runHttpBatch' (acc ++ ["shuffle"]) next

-- IN-MEMORY INTERPRETER
-- Executes commands against an in-memory state without network calls.
type InMemory = State Lib2.State

runInMemory :: CardDSL a -> InMemory a
runInMemory (Pure a) = return a
runInMemory (Free (ViewDeckCmd next)) = do
  st <- get
  case Lib2.stateTransition st Lib2.ViewDeck of
    Left err -> runInMemory (next err)
    Right (msg, st') -> put st' >> runInMemory (next (maybe "" id msg))

runInMemory (Free (AddDeckCmd deck next)) = do
  st <- get
  case Lib2.stateTransition st (Lib2.AddDeck deck) of
    Left _ -> runInMemory next
    Right (_, st') -> put st' >> runInMemory next

runInMemory (Free (DeleteDeckCmd next)) = do
  st <- get
  -- Delete changes state, no immediate output needed
  case Lib2.stateTransition st Lib2.DeleteDeck of
    Left _ -> runInMemory next
    Right (_, st') -> put st' >> runInMemory next

runInMemory (Free (CountDeckCmd next)) = do
  st <- get
  -- Count changes state by possibly returning a message, but we ignore output here
  case Lib2.stateTransition st Lib2.CountDeck of
    Left _ -> runInMemory next
    Right (_, st') -> put st' >> runInMemory next

runInMemory (Free (DrawCardCmd next)) = do
  st <- get
  case Lib2.stateTransition st Lib2.DrawCard of
    Left _ -> runInMemory next
    Right (_, st') -> put st' >> runInMemory next

runInMemory (Free (ShuffleDeckCmd next)) = do
  st <- get
  case Lib2.stateTransition st Lib2.ShuffleDeck of
    Left _ -> runInMemory next
    Right (_, st') -> put st' >> runInMemory next

-- Example DSL program
exampleProgram :: CardDSL ()
exampleProgram = do
  let card1 = Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts
  let card2 = Lib2.Card Lib2.Ace Lib2.Spades
  let deck = Lib2.Deck card1 (Lib2.SingleCard card2)
  addDeck deck
  countDeck
  shuffleDeck
  drawCard
  deleteDeck
  _ <- viewDeck
  return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["single"] -> do
      putStrLn "Running with HTTP single request per command:"
      _ <- runHttpSingle exampleProgram
      return ()
    ["batch"] -> do
      putStrLn "Running with HTTP batch requests:"
      _ <- runHttpBatch exampleProgram
      return ()
    ["memory"] -> do
      putStrLn "Running with in-memory interpreter for testing:"
      let (_, finalState) = runState (runInMemory exampleProgram) Lib2.emptyState
      putStrLn "Final in-memory state:"
      print finalState
      return ()
    _ -> putStrLn "Usage: stack run fp2024-four-client [single|batch|memory]"

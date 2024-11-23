{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Parser(..),
    Statements(..),
    Command(..),
    ProgramState(..)
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad (forever)

import qualified Lib2
import Parser

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = forever $ do
    op <- readChan chan
    case op of
        Save content respondChan -> do
            writeFile "state.txt" content
            writeChan respondChan ()
        Load respondChan -> do
            content <- readFile "state.txt"
            writeChan respondChan content

data ProgramState = ProgramState {
  deckState :: Lib2.State,
  commandHistory :: [Lib2.Query]
}

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = showQuery q
  show (Batch qs) = "START\n" ++ concatMap ((++ ";\n") . showQuery) qs ++ "FINISH\n"

showQuery :: Lib2.Query -> String
showQuery Lib2.ViewDeck = "view"
showQuery (Lib2.AddDeck deck) = "add " ++ show deck
showQuery Lib2.DeleteDeck = "delete"
showQuery Lib2.CountDeck = "count"
showQuery Lib2.DrawCard = "draw"
showQuery Lib2.ShuffleDeck = "shuffle"

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

parseCommand :: String -> Either String (Command, String)
parseCommand = runParser command

command :: Parser Command
command =
  (do
    StatementCommand <$> parseStatements
  )
  <|> parseSave
  <|> parseLoad

parseSave :: Parser Command
parseSave = do
  _ <- parseString "save"
  return SaveCommand

parseLoad :: Parser Command
parseLoad = do
  _ <- parseString "load"
  return LoadCommand

parseStatements :: Parser Statements
parseStatements =
  (do
    _ <- parseString "START\n"
    qs <- many (do
                  q <- Lib2.parseQuery'
                  _ <- parseString ";\n"
                  return q
                )
    _ <- parseString "FINISH\n"
    return (Batch qs)
  )
  <|> (do
    Single <$> Lib2.parseQuery'
  )

marshallState :: ProgramState -> Statements
marshallState (ProgramState _ commands) =
  if null commands
    then Single Lib2.DeleteDeck
    else Batch commands

renderStatements :: Statements -> String
renderStatements = show

stateTransition :: TVar ProgramState -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar SaveCommand ioChan = do
  currentProgramState <- readTVarIO stateVar
  resultChan <- newChan
  writeChan ioChan (Save (renderStatements $ marshallState currentProgramState) resultChan)
  _ <- readChan resultChan
  return $ Right $ Just "State saved."

stateTransition stateVar LoadCommand ioChan = do
  resultChan <- newChan
  writeChan ioChan (Load resultChan)
  dataString <- readChan resultChan
  case runParser parseStatements dataString of
    Left parseErr -> return $ Left $ "Load failed:\n" ++ parseErr
    Right (parsedCmds, _) -> atomically $ do
      writeTVar stateVar (ProgramState Lib2.emptyState [])
      processStatements stateVar parsedCmds

stateTransition stateVar (StatementCommand cmds) _ = atomically $ processStatements stateVar cmds

processQueries :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
processQueries state [] = Right (Nothing, state)
processQueries state (q:qs) = case Lib2.stateTransition state q of
  Left err -> Left err
  Right (msg, newState) ->
    case processQueries newState qs of
      Left err' -> Left err'
      Right (msg', finalState) ->
        let combinedMsg = case (msg, msg') of
                            (Just m1, Just m2) -> Just (m1 ++ "\n" ++ m2)
                            (Just m1, Nothing) -> Just m1
                            (Nothing, Just m2) -> Just m2
                            (Nothing, Nothing) -> Nothing
        in Right (combinedMsg, finalState)

processStatements :: TVar ProgramState -> Statements -> STM (Either String (Maybe String))
processStatements stateVar (Batch cmds) = do
  ProgramState currentState cmdHistory <- readTVar stateVar
  case processQueries currentState cmds of
    Left err -> return $ Left err
    Right (msg, updatedState) -> do
      let newCmdHistory = cmdHistory ++ cmds
      writeTVar stateVar (ProgramState updatedState newCmdHistory)
      return $ Right msg

processStatements stateVar (Single cmd) = do
  ProgramState currentState cmdHistory <- readTVar stateVar
  case Lib2.stateTransition currentState cmd of
    Left err -> return $ Left err
    Right (msg, updatedState) -> do
      let newCmdHistory = cmdHistory ++ [cmd]
      writeTVar stateVar (ProgramState updatedState newCmdHistory)
      return $ Right msg

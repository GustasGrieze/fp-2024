{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Parsers (
    Number(..),
    Rank(..),
    Suit(..),
    Card(..),
    Deck(..),
    Query(..),
    Parser,
    parse,
    parseString,
    parseWord,
    parseSuit,
    parseNumber,
    parseRank,
    parseCard,
    parseDeck,
    mergeDecks,
    parseView,
    parseDelete,
    parseAddDeck,
    parseCount,
    parseDraw,
    parseShuffle
) where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.Char as C
import qualified Data.List as L

data Number = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  deriving (Show, Eq)

data Rank = RankNumber Number | Jack | Queen | King | Ace
  deriving (Eq)

instance Show Rank where
  show (RankNumber number) = show number
  show Jack                = "Jack"
  show Queen               = "Queen"
  show King                = "King"
  show Ace                 = "Ace"

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq)

data Card = Card Rank Suit | Joker
  deriving (Eq)

instance Show Card where
  show (Card rank suit) = show rank ++ " of " ++ show suit
  show Joker            = "Joker"

data Deck = SingleCard Card | Deck Card Deck
  deriving (Eq)

instance Show Deck where
  show (Deck card deck) = show card ++ ", " ++ show deck
  show (SingleCard card) = show card

data Query = ViewDeck | AddDeck Deck | DeleteDeck | CountDeck | DrawCard | ShuffleDeck
  deriving(Show,Eq)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

parseString :: String -> Parser String
parseString value = do
    input <- lift get
    let len = length value
    if take len input == value
        then do
            lift $ put (drop len input)
            return value
        else
            throwE $ "Expected '" ++ value ++ "' but got: " ++ input

parseWord :: String -> [(String, a)] -> Parser a
parseWord typeName wordList = do
    input <- lift get
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    if not (null letters)
        then case L.lookup letters wordList of
            Just value -> do
                lift $ put rest
                return value
            Nothing -> throwE $ "'" ++ letters ++ "' is not a " ++ typeName ++ "."
        else throwE $ "Expected a " ++ typeName ++ ", but input is empty or not starting with a letter."

parseSuit :: Parser Suit
parseSuit = parseWord "Suit" [
    ("Hearts", Hearts),
    ("Diamonds", Diamonds),
    ("Clubs", Clubs),
    ("Spades", Spades)
  ]

parseNumber :: Parser Number
parseNumber = parseWord "Number" [
    ("Two", Two),
    ("Three", Three),
    ("Four", Four),
    ("Five", Five),
    ("Six", Six),
    ("Seven", Seven),
    ("Eight", Eight),
    ("Nine", Nine),
    ("Ten", Ten)
  ]

parseRank :: Parser Rank
parseRank = faceCard <|> (RankNumber <$> parseNumber)
  where
    faceCard :: Parser Rank
    faceCard = parseWord "FaceCard" [
        ("Jack", Jack),
        ("Queen", Queen),
        ("King", King),
        ("Ace", Ace)
      ]

parseCard :: Parser Card
parseCard =
    (do
        r <- parseRank
        _ <- parseString " of "
        Card r <$> parseSuit)
    <|> parseWord "Joker" [("Joker", Joker)]

parseDeck :: Parser Deck
parseDeck = do
    card <- parseCard
    (do
        _ <- parseString ", "
        mergeDecks (SingleCard card) <$> parseDeck)
      <|> return (SingleCard card)

mergeDecks :: Deck -> Deck -> Deck
mergeDecks (SingleCard card) existingDeck = Deck card existingDeck
mergeDecks (Deck card restOfDeck) existingDeck = Deck card (mergeDecks restOfDeck existingDeck)


parseView :: Parser Query
parseView = do
    _ <- parseString "view"
    return ViewDeck

parseDelete :: Parser Query
parseDelete = do
    _ <- parseString "delete"
    return DeleteDeck

parseAddDeck :: Parser Query
parseAddDeck = do
    _ <- parseString "add "
    AddDeck <$> parseDeck

parseCount :: Parser Query
parseCount = do
    _ <- parseString "count"
    return CountDeck

parseDraw :: Parser Query
parseDraw = do
    _ <- parseString "draw"
    return DrawCard

parseShuffle :: Parser Query
parseShuffle = do
    _ <- parseString "shuffle"
    return ShuffleDeck

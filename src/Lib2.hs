{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      Deck(..),
      Card(..),
      Rank(..),
      Suit(..),
      Number(..)
    )
where

import Control.Applicative (Alternative((<|>)))
import Parsers

data State = State (Maybe Deck)
  deriving(Eq,Show)

emptyState :: State
emptyState = State Nothing

-- | Count the number of cards in a deck
countCards :: Deck -> Int
countCards (SingleCard _) = 1
countCards (Deck _ rest) = 1 + countCards rest

-- | Remove the top card from the deck
removeTopCard :: Deck -> (Card, Maybe Deck)
removeTopCard (SingleCard card) = (card, Nothing)
removeTopCard (Deck card rest) = (card, Just rest)

-- | Convert deck to list
deckToList :: Deck -> [Card]
deckToList (SingleCard card) = [card]
deckToList (Deck card rest) = card : deckToList rest

-- | Convert list to deck
listToDeck :: [Card] -> Deck
listToDeck [card] = SingleCard card
listToDeck (card:cards) = Deck card (listToDeck cards)
listToDeck [] = error "Cannot create deck from empty list"

-- | Shuffle deck by interleaving
interleave :: [Card] -> [Card] -> [Card]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x : y : interleave xs ys

-- | Shuffle operation
shuffleDeck :: Deck -> Deck
shuffleDeck deck =
  let cardsList = deckToList deck
      half = length cardsList `div` 2
      (firstHalf, secondHalf) = splitAt half cardsList
      shuffledList = interleave firstHalf secondHalf
  in listToDeck shuffledList

-- | State transitions
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State maybeDeck) query = case query of
    ViewDeck ->
        case maybeDeck of
            Just deck -> Right (Just (show deck), State maybeDeck)
            Nothing -> Right (Just "The deck is empty.", State maybeDeck)
    AddDeck newDeck ->
        let updatedDeck = case maybeDeck of
                Just existingDeck -> mergeDecks newDeck existingDeck
                Nothing -> newDeck
            message = case newDeck of
                SingleCard _ -> "Card added."
                Deck _ _ -> "Deck added."
        in Right (Just message, State (Just updatedDeck))
    DeleteDeck ->
        Right (Just "Deck deleted.", State Nothing)
    CountDeck ->
        case maybeDeck of
            Just deck -> Right (Just ("Number of cards in the deck: " ++ show (countCards deck)), State maybeDeck)
            Nothing -> Right (Just "The deck is empty.", State maybeDeck)
    DrawCard ->
        case maybeDeck of
            Just deck ->
                let (card, newDeck) = removeTopCard deck
                    message = "You drew: " ++ show card
                in Right (Just message, State newDeck)
            Nothing -> Right (Just "The deck is empty.", State Nothing)
    ShuffleDeck ->
        case maybeDeck of
            Just deck ->
                let shuffledDeck = shuffleDeck deck
                in Right (Just "Deck shuffled.", State (Just shuffledDeck))
            Nothing -> Right (Just "The deck is empty. Cannot shuffle an empty deck.", State Nothing)

parseQuery :: String -> Either String Query
parseQuery input =
    let (result, _) = parse (parseView <|> parseDelete <|> parseAddDeck <|> parseCount <|> parseDraw <|> parseShuffle) input
    in result

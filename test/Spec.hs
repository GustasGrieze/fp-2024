{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Test.Tasty.QuickCheck as QC
import Data.List
import Data.Ord


import Test.Tasty
import Control.Monad (when)

import Lib2

import qualified Parser as P
import Data.Maybe (fromJust)
import qualified Data.Char as C
import qualified Data.List as L
import Lib3 (ProgramState(..), marshallState, renderStatements, parseStatements, runParser, Statements(..))
import qualified Lib3

main :: IO ()
main =  defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    testCase "Parse 'view' query" $
      Lib2.parseQuery "view" @?= Right Lib2.ViewDeck,

    testCase "Parse 'add Ace of Spades' query" $
      Lib2.parseQuery "add Ace of Spades" @?= Right (Lib2.AddDeck (Lib2.SingleCard (Lib2.Card Lib2.Ace Lib2.Spades))),

    testCase "Parse 'delete' query" $
      Lib2.parseQuery "delete" @?= Right Lib2.DeleteDeck,

    testCase "Parse 'Ace of Spades' card" $
      Lib2.parseCard "Ace of Spades" @?= Right (Lib2.Card Lib2.Ace Lib2.Spades, ""),

    testCase "Parse 'Joker' card" $
      Lib2.parseCard "Joker" @?= Right (Lib2.Joker, ""),

    testCase "Parse 'Two of Hearts' card" $
      Lib2.parseCard "Two of Hearts" @?= Right (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts, ""),

    testCase "Parse deck of cards 'Ace of Spades, Two of Hearts'" $
      Lib2.parseDeck "Ace of Spades, Two of Hearts" @?= Right (Lib2.Deck (Lib2.Card Lib2.Ace Lib2.Spades) (Lib2.SingleCard (Lib2.Card (Lib2.RankNumber Lib2.Two) Lib2.Hearts)), ""),

    testCase "Parse 'count' query" $
      Lib2.parseQuery "count" @?= Right Lib2.CountDeck,

    testCase "Parse 'draw' query" $
      Lib2.parseQuery "draw" @?= Right Lib2.DrawCard,

    testCase "Parse 'draw' query" $
      Lib2.parseQuery "draw" @?= Right Lib2.DrawCard,

    testCase "State transition for 'draw' when deck is empty" $ do
      let state = Lib2.emptyState
      let result = Lib2.stateTransition state Lib2.DrawCard
      result @?= Right (Just "The deck is empty.", state),

     testCase "Parse 'shuffle' query" $
      Lib2.parseQuery "shuffle" @?= Right Lib2.ShuffleDeck,

    testCase "State transition for 'shuffle' when deck is empty" $ do
      let state = Lib2.emptyState
      let result = Lib2.stateTransition state Lib2.ShuffleDeck
      result @?= Right (Just "The deck is empty. Cannot shuffle an empty deck.", state),

    testCase "Parse 'count' query with new parser" $
      P.runParser Lib2.parseQuery' "count" @?= Right (Lib2.CountDeck, ""),

    testCase "Parse 'draw' query with new parser" $
      P.runParser Lib2.parseQuery' "draw" @?= Right (Lib2.DrawCard, ""),

    testCase "Parse 'shuffle' query with new parser" $
      P.runParser Lib2.parseQuery' "shuffle" @?= Right (Lib2.ShuffleDeck, "")

  ]

instance Arbitrary Number where
    arbitrary = elements [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten]

instance Arbitrary Rank where
    arbitrary = oneof [ RankNumber <$> arbitrary
                     , elements [Jack, Queen, King, Ace]
                     ]

instance Arbitrary Suit where
    arbitrary = elements [Hearts, Diamonds, Clubs, Spades]

instance Arbitrary Card where
    arbitrary = oneof [ Card <$> arbitrary <*> arbitrary
                      , pure Joker
                      ]

instance Arbitrary Deck where
    arbitrary = oneof [ SingleCard <$> arbitrary
                     , Deck <$> arbitrary <*> arbitrary
                     ]

instance Arbitrary Query where
    arbitrary = oneof [ pure ViewDeck
                      , AddDeck <$> arbitrary
                      , pure DeleteDeck
                      , pure CountDeck
                      , pure DrawCard
                      , pure ShuffleDeck
                      ]

instance Arbitrary Statements where
    arbitrary = oneof [ Single <$> arbitrary
                     , Batch <$> arbitrary
                     ]

-- Add Show instance for ProgramState
instance Show ProgramState where
  show (ProgramState state history) =
    "ProgramState { deckState = " ++ show state ++
    ", commandHistory = " ++ show history ++ " }"

instance Arbitrary ProgramState where
    arbitrary = do
        cmds <- listOf arbitrary :: Gen [Lib2.Query]
        let initialState = Lib2.emptyState
        let resultState = foldl applyQuery (Right (initialState, [])) cmds
        case resultState of
            Left _ -> arbitrary
            Right (finalState, history) -> return $ ProgramState finalState history
      where
        applyQuery :: Either String (Lib2.State, [Lib2.Query]) -> Lib2.Query -> Either String (Lib2.State, [Lib2.Query])
        applyQuery (Left err) _ = Left err
        applyQuery (Right (state, history)) q =
            case Lib2.stateTransition state q of
                Left err -> Left err
                Right (_, newState) -> Right (newState, history ++ [q])

propertyTests :: TestTree
propertyTests = testGroup "Random Property Tests"
  [
    testProperty "renderStatements and parseStatements are inverses" $
      \stmt ->
        let rendered = renderStatements stmt
            parsed = runParser parseStatements rendered
        in case parsed of
             Left _ -> False
             Right (parsedStmt, _) -> stmt == parsedStmt,

    testProperty "Saving and loading preserves the state" $
      \programState ->
        let -- Marshal and render the program state
            stmts = marshallState programState
            rendered = renderStatements stmts
            parsed = runParser parseStatements rendered
        in case parsed of
             Left _ -> False
             Right (parsedStmts, _) ->
               let initialState = Lib2.emptyState
                   reconstructedState = executeStatements initialState parsedStmts
               in deckState programState == reconstructedState
  ]

executeStatements :: Lib2.State -> Statements -> Lib2.State
executeStatements initialState (Single cmd) =
    case Lib2.stateTransition initialState cmd of
        Right (_, newState) -> newState
        Left _ -> initialState
executeStatements initialState (Batch cmds) =
    foldl executeQuery initialState cmds
  where
    executeQuery state cmd =
        case Lib2.stateTransition state cmd of
            Right (_, newState) -> newState
            Left _ -> state
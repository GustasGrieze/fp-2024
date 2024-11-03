{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Lib2 qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

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
      result @?= Right (Just "The deck is empty. Cannot shuffle an empty deck.", state)
  ]
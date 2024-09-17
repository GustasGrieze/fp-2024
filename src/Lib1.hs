module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ -- Actions
    "add",
    "remove",
    "list",
    "exit",
    "quit",
    -- Entities
    "book",
    "collection",
    -- Prepositions
    "to",
    "from",
    "in",
    -- Special Keywords
    "all",
    "contents"
  ]
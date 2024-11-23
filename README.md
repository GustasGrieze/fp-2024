# Deck of cards fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

### Batch Queries
From Lib3 it is possible to provide queries as a batch.
After loading the program, you can run it using `stack run fp2024-three`, then write the following commands:
```
:paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| START
| add Ace of Hearts;
| add King of Hearts;
| add Queen of Hearts;
| add Jack of Hearts;
| add Ten of Hearts;
| view;
| count;
| draw;
| view;
| add Ten of Hearts;
| view;
| shuffle;
| view;
| FINISH
| 
Card added.
Card added.
Card added.
Card added.
Card added.
Ten of Hearts, Jack of Hearts, Queen of Hearts, King of Hearts, Ace of Hearts
Number of cards in the deck: 5
You drew: Ten of Hearts
Jack of Hearts, Queen of Hearts, King of Hearts, Ace of Hearts
Card added.
Ten of Hearts, Jack of Hearts, Queen of Hearts, King of Hearts, Ace of Hearts
Deck shuffled.
Ten of Hearts, Queen of Hearts, Jack of Hearts, King of Hearts, Ace of Hearts
```

### Persistence
load and save commands
```
add Queen of Hearts
Card added.
>>> add King of Hearts
Card added.
>>> view
King of Hearts, Queen of Hearts
>>> count
Number of cards in the deck: 2
>>> save
State saved.
```
```
delete
Deck deleted.
>>> view
The deck is empty.
>>> load
Card added.
Card added.
King of Hearts, Queen of Hearts
Number of cards in the deck: 2
>>> view
King of Hearts, Queen of Hearts
```
-- | Pure game logic for "Appease Belcebú".
--
-- Nothing in this module does IO: every function takes a 'GameState' and
-- returns a new one, plus the lines of text to show the player. That makes
-- the rules easy to read, test and play with in GHCi.
module Game
  ( GameState (..)
  , Command (..)
  , Outcome (..)
  , Mood (..)
  , maxChaos
  , winTurn
  , newGame
  , parseCommand
  , step
  , mood
  , statusLine
  , helpText
  ) where

import Data.Char (toLower)

-- * State

data GameState = GameState
  { turn      :: Int  -- ^ turns survived so far
  , chaos     :: Int  -- ^ 0 .. maxChaos; at maxChaos you lose
  , tuna      :: Int  -- ^ cans of tuna in your pocket
  , laserUses :: Int  -- ^ how many times the laser pointer has been used
  , seed      :: Int  -- ^ state of the random number generator
  } deriving (Show)

data Command
  = FeedTuna
  | Pet
  | Laser
  | OpenFridge
  | GitCommit
  | GitPushForce
  | Status
  | Help
  | Quit
  | Unknown String
  deriving (Eq, Show)

data Outcome = Continue | Won | Lost
  deriving (Eq, Show)

data Mood = Sleepy | Grumpy | Scheming | Apocalyptic
  deriving (Eq, Show)

maxChaos, winTurn :: Int
maxChaos = 10
winTurn  = 15

newGame :: Int -> GameState
newGame s = GameState
  { turn      = 0
  , chaos     = 3
  , tuna      = 2
  , laserUses = 0
  , seed      = s `mod` lcgModulus
  }

-- * Randomness
--
-- A tiny linear congruential generator, so we only need 'base'.

lcgModulus :: Int
lcgModulus = 2147483648 -- 2^31

nextSeed :: Int -> Int
nextSeed s = (1103515245 * s + 12345) `mod` lcgModulus

-- | Roll a number from 0 to 99 and advance the generator.
rollPercent :: GameState -> (Int, GameState)
rollPercent st =
  let s' = nextSeed (seed st)
  in ((s' `div` 65536) `mod` 100, st { seed = s' })

-- * Parsing

parseCommand :: String -> Command
parseCommand input = case unwords (words (map toLower input)) of
  "feed tuna"        -> FeedTuna
  "feed"             -> FeedTuna
  "pet"              -> Pet
  "pet belcebú"      -> Pet
  "laser"            -> Laser
  "open fridge"      -> OpenFridge
  "fridge"           -> OpenFridge
  "git commit"       -> GitCommit
  "git push --force" -> GitPushForce
  "git push -f"      -> GitPushForce
  "status"           -> Status
  "help"             -> Help
  "?"                -> Help
  "quit"             -> Quit
  "exit"             -> Quit
  other              -> Unknown other

-- * Rules

-- | Play one command. Returns the messages to print, the new state, and
-- whether the game goes on. 'Status', 'Help' and 'Quit' don't spend a turn
-- ('Main' handles them before calling this).
step :: Command -> GameState -> ([String], GameState, Outcome)
step cmd st
  | not (spendsTurn cmd) = ([], st, Continue)
  | otherwise =
      let (actionMsgs, st1) = act cmd st
          (eventMsgs, st2)  = randomEvent st1
          st3 = st2 { turn = turn st2 + 1 }
          restless = "Belcebú grows restless. " ++ showDelta 1
          final = addChaos 1 st3
          outcome
            | chaos final >= maxChaos = Lost
            | turn final >= winTurn   = Won
            | otherwise               = Continue
      in (actionMsgs ++ eventMsgs ++ [restless], final, outcome)

spendsTurn :: Command -> Bool
spendsTurn c = c `notElem` [Status, Help, Quit]

-- | What the player's action does.
act :: Command -> GameState -> ([String], GameState)
act FeedTuna st
  | tuna st > 0 =
      ( [ "You open a can of tuna. He eats it. He does not thank you."
        , showDelta (-3) ]
      , addChaos (-3) st { tuna = tuna st - 1 } )
  | otherwise =
      ( [ "You pat your pockets. No tuna. He knows. He always knows."
        , showDelta 1 ]
      , addChaos 1 st )
act Pet st =
  let (roll, st') = rollPercent st
  in if roll < 60
       then ( [ "He purrs like a hard drive spinning up in 2004."
              , showDelta (-1) ]
            , addChaos (-1) st' )
       else ( [ "He bites you. Lovingly. Infernally."
              , showDelta 2 ]
            , addChaos 2 st' )
act Laser st =
  let worth = max 0 (2 - laserUses st)
      st'   = st { laserUses = laserUses st + 1 }
      msg = case worth of
        2 -> "The red dot! He chases it across the walls, the ceiling, the Abyss."
        1 -> "He chases the dot again, but with visibly less enthusiasm."
        _ -> "He looks at the dot. He looks at you. He knows it's you."
  in ([msg, showDelta (negate worth)], addChaos (negate worth) st')
act OpenFridge st =
  ( [ "You open the fridge and find a can of tuna."
    , "Two glowing CI-red eyes appear behind you. He heard it."
    , "Tuna +1. " ++ showDelta 1 ]
  , addChaos 1 st { tuna = tuna st + 1 } )
act GitCommit st =
  ( [ "You commit your work. He walks across the keyboard first."
    , "Your commit message is now: meowmeowmeow"
    , showDelta 1 ]
  , addChaos 1 st )
act GitPushForce st =
  ( [ "git push --force"
    , "The Abyss stirs. Somewhere, the Infernal Lord of Merge Conflicts smiles."
    , "Belcebú purrs. This is the worst possible sign."
    , showDelta 4 ]
  , addChaos 4 st )
act (Unknown "") st =
  (["You hesitate. Belcebú stares at you, unimpressed."], st)
act (Unknown other) st =
  ( [ "You try to '" ++ other ++ "'. Belcebú stares at you, unimpressed."
    , "(Type 'help' for things that might actually work.)" ]
  , st )
act _ st = ([], st)

-- | Something happens on its own every turn.
randomEvent :: GameState -> ([String], GameState)
randomEvent st =
  let (roll, st') = rollPercent st
  in if roll < 25
       then (["He knocks your mug off the desk, maintaining eye contact. " ++ showDelta 1], addChaos 1 st')
     else if roll < 50
       then (["He naps on the laptop's air vent. The fans scream. He is at peace. " ++ showDelta (-1)], addChaos (-1) st')
     else if roll < 75
       then (["Somewhere, a CI pipeline fails. His eyes glow a little brighter."], st')
     else ([], st')

addChaos :: Int -> GameState -> GameState
addChaos n st = st { chaos = max 0 (min maxChaos (chaos st + n)) }

showDelta :: Int -> String
showDelta n
  | n > 0     = "(Chaos +" ++ show n ++ ")"
  | n < 0     = "(Chaos " ++ show n ++ ")"
  | otherwise = "(Chaos unchanged)"

-- * Display

mood :: GameState -> Mood
mood st
  | c <= 2    = Sleepy
  | c <= 5    = Grumpy
  | c <= 8    = Scheming
  | otherwise = Apocalyptic
  where c = chaos st

statusLine :: GameState -> String
statusLine st =
  "Turn " ++ show (turn st) ++ "/" ++ show winTurn
    ++ "   Mood: " ++ show (mood st)
    ++ "   Tuna: " ++ show (tuna st)
    ++ "   Chaos: [" ++ bar ++ "] " ++ show (chaos st) ++ "/" ++ show maxChaos
  where
    bar = replicate (chaos st) '#' ++ replicate (maxChaos - chaos st) '.'

helpText :: String
helpText = unlines
  [ "Things you can do:"
  , "  feed tuna         Calms him down a lot. Needs tuna."
  , "  pet               Usually he purrs. Usually."
  , "  laser             A great distraction, until he gets bored of it."
  , "  open fridge       Get more tuna. He will hear it."
  , "  git commit        Save your work. He has opinions."
  , "  git push --force  Why would you do this."
  , "  status            Show the current situation (free)."
  , "  help              Show this list (free)."
  , "  quit              Run away."
  , ""
  , "Survive " ++ show winTurn ++ " turns before his chaos reaches "
      ++ show maxChaos ++ ". Chaos rises by 1 every turn on its own."
  ]

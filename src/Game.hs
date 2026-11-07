-- | Pure game logic for "Appease Belcebú".
--
-- Nothing in this module does IO: every function takes a 'GameState' and
-- returns a new one, plus the lines of text to show the player. That makes
-- the rules easy to read, test and play with in GHCi.
module Game
  ( GameState (..)
  , Level (..)
  , Rules (..)
  , Command (..)
  , Outcome (..)
  , Mood (..)
  , maxChaos
  , rulesFor
  , newGame
  , startGame
  , parseCommand
  , step
  , mood
  , catName
  , statusLine
  , helpText
  ) where

import Data.Char (toLower)

-- * State

-- | Which cat you are trying to appease.
data Level = BelcebuLevel | BehemothLevel
  deriving (Eq, Show)

data GameState = GameState
  { level     :: Level
  , turn      :: Int  -- ^ turns survived so far
  , chaos     :: Int  -- ^ 0 .. maxChaos; at maxChaos you lose
  , tuna      :: Int  -- ^ cans of tuna in your pocket
  , laserUses :: Int  -- ^ how many times the laser pointer has been used
  , seed      :: Int  -- ^ state of the random number generator
  } deriving (Show)

-- | The numbers that make one level harder than another.
data Rules = Rules
  { winTurns   :: Int  -- ^ turns to survive
  , startChaos :: Int
  , startTuna  :: Int
  , tunaCalm   :: Int  -- ^ how much chaos a can of tuna removes
  , purrChance :: Int  -- ^ % chance that petting goes well
  }

rulesFor :: Level -> Rules
rulesFor BelcebuLevel  = Rules { winTurns = 15, startChaos = 3, startTuna = 2, tunaCalm = 3, purrChance = 60 }
rulesFor BehemothLevel = Rules { winTurns = 20, startChaos = 5, startTuna = 1, tunaCalm = 2, purrChance = 40 }

rules :: GameState -> Rules
rules = rulesFor . level

data Command
  = FeedTuna
  | Pet
  | Laser
  | OpenFridge
  | PlayChess
  | GitCommit
  | GitPushForce
  | FixPrimus      -- ^ the secret command
  | Status
  | Help
  | Quit
  | Unknown String
  deriving (Eq, Show)

data Outcome = Continue | Won | Lost | BehemothArrives
  deriving (Eq, Show)

data Mood = Sleepy | Grumpy | Scheming | Apocalyptic
  deriving (Eq, Show)

maxChaos :: Int
maxChaos = 10

newGame :: Level -> Int -> GameState
newGame lvl s = GameState
  { level     = lvl
  , turn      = 0
  , chaos     = startChaos r
  , tuna      = startTuna r
  , laserUses = 0
  , seed      = s `mod` lcgModulus
  }
  where r = rulesFor lvl

-- | Start a game from a random seed. One time in twenty, Behemoth shows up
-- instead of Belcebú.
startGame :: Int -> GameState
startGame s =
  let (roll, st) = rollPercent (newGame BelcebuLevel s)
  in if roll < 5 then newGame BehemothLevel (seed st) else st

catName :: GameState -> String
catName st = case level st of
  BelcebuLevel  -> "Belcebú"
  BehemothLevel -> "Behemoth"

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
  "laser"            -> Laser
  "open fridge"      -> OpenFridge
  "fridge"           -> OpenFridge
  "play chess"       -> PlayChess
  "chess"            -> PlayChess
  "git commit"       -> GitCommit
  "git push --force" -> GitPushForce
  "git push -f"      -> GitPushForce
  "fix the primus"   -> FixPrimus
  "fix primus"       -> FixPrimus
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
  | cmd == FixPrimus && level st == BelcebuLevel =
      ([], newGame BehemothLevel (seed st), BehemothArrives)
  | otherwise =
      let (actionMsgs, st1) = act cmd st
          (eventMsgs, st2)  = randomEvent st1
          st3 = st2 { turn = turn st2 + 1 }
          restless = catName st ++ " grows restless. " ++ showDelta 1
          final = addChaos 1 st3
          outcome
            | chaos final >= maxChaos         = Lost
            | turn final >= winTurns (rules st) = Won
            | otherwise                       = Continue
      in (actionMsgs ++ eventMsgs ++ [restless], final, outcome)

spendsTurn :: Command -> Bool
spendsTurn c = c `notElem` [Status, Help, Quit]

-- | What the player's action does.
act :: Command -> GameState -> ([String], GameState)
act FeedTuna st
  | tuna st > 0 =
      let calm = tunaCalm (rules st)
          msg = case level st of
            BelcebuLevel  -> "You open a can of tuna. He eats it. He does not thank you."
            BehemothLevel -> "Behemoth eats the tuna, then complains at length that it isn't fresh enough."
      in ([msg, showDelta (negate calm)], addChaos (negate calm) st { tuna = tuna st - 1 })
  | otherwise =
      ( [ "You pat your pockets. No tuna. He knows. He always knows."
        , showDelta 1 ]
      , addChaos 1 st )
act Pet st =
  let (roll, st') = rollPercent st
  in if roll < purrChance (rules st)
       then ( [ case level st of
                  BelcebuLevel  -> "He purrs like a hard drive spinning up in 2004."
                  BehemothLevel -> "Behemoth purrs. It sounds like a samovar about to explode."
              , showDelta (-1) ]
            , addChaos (-1) st' )
       else ( [ case level st of
                  BelcebuLevel  -> "He bites you. Lovingly. Infernally."
                  BehemothLevel -> "\"Do not touch me,\" says Behemoth, out loud, and bites you."
              , showDelta 2 ]
            , addChaos 2 st' )
act Laser st = case level st of
  BehemothLevel ->
    ( [ "Behemoth regards the red dot with contempt."
      , "\"I am not some common house cat.\""
      , showDelta 1 ]
    , addChaos 1 st )
  BelcebuLevel ->
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
act PlayChess st = case level st of
  BelcebuLevel ->
    ( [ "You set up a chess board. Belcebú knocks the pieces off, one by one."
      , showDelta 0 ]
    , st )
  BehemothLevel ->
    let (roll, st') = rollPercent st
    in if roll < 60
         then ( [ "Behemoth plays chess. His knight moves like a bishop. His king hides under the sofa."
                , "He declares victory. You do not argue."
                , showDelta (-3) ]
              , addChaos (-3) st' )
         else ( [ "Behemoth is losing. He flips the board and claims it was an earthquake."
                , showDelta 2 ]
              , addChaos 2 st' )
act GitCommit st =
  ( [ "You commit your work. He walks across the keyboard first."
    , "Your commit message is now: meowmeowmeow"
    , showDelta 1 ]
  , addChaos 1 st )
act GitPushForce st =
  ( [ "git push --force"
    , "The Abyss stirs. Somewhere, the Infernal Lord of Merge Conflicts smiles."
    , catName st ++ " purrs. This is the worst possible sign."
    , showDelta 4 ]
  , addChaos 4 st )
act FixPrimus st =
  ( [ "\"Leave it,\" says Behemoth, waving a wrench. \"I'm already fixing it.\""
    , showDelta 0 ]
  , st )
act (Unknown "") st =
  (["You hesitate. " ++ catName st ++ " stares at you, unimpressed."], st)
act (Unknown other) st =
  ( [ "You try to '" ++ other ++ "'. " ++ catName st ++ " stares at you, unimpressed."
    , "(Type 'help' for things that might actually work.)" ]
  , st )
act _ st = ([], st)

-- | Something happens on its own every turn.
randomEvent :: GameState -> ([String], GameState)
randomEvent st =
  let (roll, st') = rollPercent st
  in case level st of
       BelcebuLevel
         | roll < 25 -> (["He knocks your mug off the desk, maintaining eye contact. " ++ showDelta 1], addChaos 1 st')
         | roll < 50 -> (["He naps on the laptop's air vent. The fans scream. He is at peace. " ++ showDelta (-1)], addChaos (-1) st')
         | roll < 75 -> (["Somewhere, a CI pipeline fails. His eyes glow a little brighter."], st')
         | roll < 80 -> (["You smell kerosene. Somewhere, someone is fixing a primus."], st')
         | otherwise -> ([], st')
       BehemothLevel
         | roll < 20 -> (["Behemoth sets the curtains on fire with the primus. \"An improvement,\" he says. " ++ showDelta 1], addChaos 1 st')
         | roll < 45 -> (["Behemoth dozes off mid-sentence. " ++ showDelta (-1)], addChaos (-1) st')
         | roll < 70 -> (["Behemoth reads your commit history aloud and laughs."], st')
         | otherwise -> ([], st')

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
  catName st ++ " | Turn " ++ show (turn st) ++ "/" ++ show (winTurns (rules st))
    ++ "   Mood: " ++ show (mood st)
    ++ "   Tuna: " ++ show (tuna st)
    ++ "   Chaos: [" ++ bar ++ "] " ++ show (chaos st) ++ "/" ++ show maxChaos
  where
    bar = replicate (chaos st) '#' ++ replicate (maxChaos - chaos st) '.'

helpText :: GameState -> String
helpText st = unlines $
  [ "Things you can do:"
  , "  feed tuna         Calms him down" ++ tunaNote ++ ". Needs tuna."
  , "  pet               " ++ petNote
  , "  laser             " ++ laserNote
  , "  open fridge       Get more tuna. He will hear it."
  ]
  ++ chessLine ++
  [ "  git commit        Save your work. He has opinions."
  , "  git push --force  Why would you do this."
  , "  status            Show the current situation (free)."
  , "  help              Show this list (free)."
  , "  quit              Run away."
  , ""
  , "Survive " ++ show (winTurns (rules st)) ++ " turns before his chaos reaches "
      ++ show maxChaos ++ ". Chaos rises by 1 every turn on its own."
  ]
  where
    (tunaNote, petNote, laserNote, chessLine) = case level st of
      BelcebuLevel ->
        ( " a lot"
        , "Usually he purrs. Usually."
        , "A great distraction, until he gets bored of it."
        , [] )
      BehemothLevel ->
        ( " a little"
        , "He does not like being touched. Mostly."
        , "Useless. He is not some common house cat."
        , ["  play chess        He cheats. Let him win. If he loses, duck."] )

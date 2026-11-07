-- | The IO side of "Appease Belcebú": reads commands, prints what happens.
module Main (main) where

import Game
import System.CPUTime (getCPUTime)
import System.IO

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  t <- getCPUTime
  let st = startGame (fromIntegral (t `mod` 2147483648))
  putStr $ case level st of
    BelcebuLevel  -> belcebuIntro
    BehemothLevel -> behemothIntro
  putStrLn (statusLine st)
  loop st

loop :: GameState -> IO ()
loop st = do
  putStr "\n> "
  eof <- isEOF
  if eof
    then putStrLn ("\nYou close the terminal. " ++ catName st ++ " will remember this.")
    else do
      line <- getLine
      case parseCommand line of
        Quit   -> putStrLn ("You back away slowly. " ++ catName st ++ " yawns. You were never a threat.")
        Help   -> putStr (helpText st) >> loop st
        Status -> putStrLn (statusLine st) >> loop st
        cmd    -> do
          let (msgs, st', outcome) = step cmd st
          mapM_ putStrLn msgs
          case outcome of
            Continue        -> putStrLn ("\n" ++ statusLine st') >> loop st'
            BehemothArrives -> do
              putStr behemothTakeover
              putStrLn (statusLine st')
              loop st'
            Won  -> putStr (winText st')
            Lost -> putStr (loseText st')

belcebuIntro :: String
belcebuIntro = unlines
  [ ""
  , "            /\\_/\\"
  , "           ( o.o )      APPEASE BELCEBÚ"
  , "            > ^ <       a minigame of feline damnation"
  , ""
  , "The terminal flickers. The cursor stops blinking. It just stares."
  , "Belcebú, Acolyte Third Class of the Infernal Lord of Merge Conflicts,"
  , "is sitting on top of the fridge. His eyes glow CI-red."
  , ""
  , "Keep him calm for " ++ show (winTurns (rulesFor BelcebuLevel))
      ++ " turns. If his chaos reaches " ++ show maxChaos
      ++ ", he force-pushes over your repo."
  , "Type 'help' to see what you can do."
  , ""
  ]

behemothArt :: [String]
behemothArt =
  [ "          /\\_____/\\"
  , "         (  O   O  )    APPEASE BEHEMOTH"
  , "         (  =\\_/=  )    *** HARD LEVEL UNLOCKED ***"
  , "          )       ("
  , "         (_(_| |_)_)"
  ]

behemothRules :: [String]
behemothRules =
  [ "Survive " ++ show (winTurns (rulesFor BehemothLevel)) ++ " turns. Tuna barely works on him,"
  , "the laser offends him, and he wants to play chess. Type 'help'."
  , ""
  ]

behemothIntro :: String
behemothIntro = unlines $
  [ "" ] ++ behemothArt ++
  [ ""
  , "The terminal flickers. The fridge is empty. Belcebú is nowhere to be seen."
  , "Instead, a black cat the size of a hog sits on your desk chair, upright,"
  , "holding a primus stove and a wrench. \"You were expecting someone else?\""
  , "He is Behemoth, and he is the devil's favorite."
  , ""
  ] ++ behemothRules

behemothTakeover :: String
behemothTakeover = unlines $
  [ "You kneel by the old primus stove and start fiddling with it."
  , "There is a smell of kerosene. Then a loud BANG."
  , ""
  , "Belcebú is shoved off the fridge by a paw the size of a frying pan."
  , "A black cat the size of a hog climbs out of the smoke, upright,"
  , "and takes the wrench from your hands. \"Amateur. Allow me.\""
  , ""
  ] ++ behemothArt ++
  [ ""
  , "Belcebú hides behind the router. Your progress is gone. Good luck."
  ] ++ behemothRules

winText :: GameState -> String
winText st = unlines $ case level st of
  BelcebuLevel ->
    [ ""
    , "Belcebú curls into a ball the size of a beanbag chair,"
    , "right on top of the laptop's air vent, and falls asleep."
    , "The fans spin louder."
    , ""
    , "The cursor started blinking again."
    , ""
    , "*** YOU WIN. Your repo survives. For now. ***"
    ]
  BehemothLevel ->
    [ ""
    , "The primus finally hisses into a clean blue flame."
    , "Behemoth admires his work, mutters \"I told you so,\""
    , "and falls asleep beside it, purring like a distant thunderstorm."
    , "Belcebú peeks out from behind the router, deeply impressed with you."
    , ""
    , "*** YOU APPEASED BEHEMOTH. Almost nobody does that. ***"
    ]

loseText :: GameState -> String
loseText st = unlines $
  [ ""
  , catName st ++ " leaps onto the keyboard. His paws move with infernal precision:"
  , ""
  , "    $ git push --force origin master"
  , "    + 4815162...0000000 master -> master (forced update)"
  , ""
  ] ++ case level st of
    BelcebuLevel ->
      [ "Your history is gone. The Abyss is pleased. He is purring."
      , ""
      , "*** GAME OVER. Type faster next time, mortal. ***"
      ]
    BehemothLevel ->
      [ "Then, to \"fix\" things, he points the primus at the server rack."
      , "Everything is on fire. Behemoth says he had nothing to do with it."
      , ""
      , "*** GAME OVER. Behemoth remains unappeased. ***"
      ]

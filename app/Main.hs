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
  let st = newGame (fromIntegral (t `mod` 2147483648))
  putStr intro
  putStrLn (statusLine st)
  loop st

loop :: GameState -> IO ()
loop st = do
  putStr "\n> "
  eof <- isEOF
  if eof
    then putStrLn "\nYou close the terminal. Belcebú will remember this."
    else do
      line <- getLine
      case parseCommand line of
        Quit   -> putStrLn "You back away slowly. Belcebú yawns. You were never a threat."
        Help   -> putStr helpText >> loop st
        Status -> putStrLn (statusLine st) >> loop st
        cmd    -> do
          let (msgs, st', outcome) = step cmd st
          mapM_ putStrLn msgs
          case outcome of
            Continue -> putStrLn ("\n" ++ statusLine st') >> loop st'
            Won      -> putStr winText
            Lost     -> putStr loseText

intro :: String
intro = unlines
  [ ""
  , "            /\\_/\\"
  , "           ( o.o )      APPEASE BELCEBÚ"
  , "            > ^ <       a minigame of feline damnation"
  , ""
  , "The terminal flickers. The cursor stops blinking. It just stares."
  , "Belcebú, Acolyte Third Class of the Infernal Lord of Merge Conflicts,"
  , "is sitting on top of the fridge. His eyes glow CI-red."
  , ""
  , "Keep him calm for " ++ show winTurn ++ " turns. If his chaos reaches "
      ++ show maxChaos ++ ", he force-pushes over your repo."
  , "Type 'help' to see what you can do."
  , ""
  ]

winText :: String
winText = unlines
  [ ""
  , "Belcebú curls into a ball the size of a beanbag chair,"
  , "right on top of the laptop's air vent, and falls asleep."
  , "The fans spin louder."
  , ""
  , "The cursor started blinking again."
  , ""
  , "*** YOU WIN. Your repo survives. For now. ***"
  ]

loseText :: String
loseText = unlines
  [ ""
  , "Belcebú leaps onto the keyboard. His paws move with infernal precision:"
  , ""
  , "    $ git push --force origin master"
  , "    + 4815162...0000000 master -> master (forced update)"
  , ""
  , "Your history is gone. The Abyss is pleased. He is purring."
  , ""
  , "*** GAME OVER. Type faster next time, mortal. ***"
  ]

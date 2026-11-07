# Appease Belcebú

```
            /\_/\
           ( o.o )      APPEASE BELCEBÚ
            > ^ <       a minigame of feline damnation
```

A small turn-based text adventure in Haskell, using only the standard library.

**Belcebú** is a black cat the size of a sofa: Acolyte Third Class of the Infernal Lord of Merge Conflicts, Keeper of the Uncommitted Changes, and Devourer of Semicolons. His eyes glow the exact red of a failing CI pipeline. He is sitting on top of your fridge, and he is getting restless.

Keep him calm for 15 turns. Feed him tuna, pet him (risky), and wave a laser pointer around. If his chaos meter reaches 10, he jumps on the keyboard and runs `git push --force` over your repo.

## How to play

You need GHC and cabal. The easiest way to get them is [GHCup](https://www.haskell.org/ghcup/).

```bash
git clone https://github.com/S4815162342/appease-belcebu
cd appease-belcebu
cabal run belcebu
```

Type `help` in the game to see what you can do. `status` shows the situation, and `quit` lets you run away.

```
> feed tuna
You open a can of tuna. He eats it. He does not thank you.
(Chaos -3)
He knocks your mug off the desk, maintaining eye contact. (Chaos +1)
Belcebú grows restless. (Chaos +1)

Belcebú | Turn 1/15   Mood: Sleepy   Tuna: 1   Chaos: [##........] 2/10
```

*Rumor has it Belcebú isn't the only cat in this house. If you ever smell kerosene, pay attention.*

## How the code is organized

| File | What's in it |
|---|---|
| `src/Game.hs` | The rules: game state, commands, random events. All pure functions, so you can play with them in GHCi. |
| `app/Main.hs` | The input loop and all the story text. |
| `belcebu.cabal` | The build. It depends only on `base`. |

There is no `random` dependency: the game uses a tiny linear congruential generator seeded from the CPU clock.

To poke at the rules interactively:

```bash
ghci -isrc src/Game.hs
```

```haskell
ghci> let (msgs, st, outcome) = step FeedTuna (newGame BelcebuLevel 42)
ghci> mapM_ putStrLn msgs
```

## License

[MIT](LICENSE)

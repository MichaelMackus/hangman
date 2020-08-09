import System.Environment
import System.IO
import System.Random

main :: IO ()
main = hangman

default_dictionary = "words"
usage = unlines ["Usage: hangman [DICTIONARY_FILE]",
                 "",
                 "NOTE: DICTIONARY_FILE defaults to " ++ default_dictionary]

hangman :: IO ()
hangman = do args  <- getArgs
             let arg = if length args > 0 then args !! 0 else default_dictionary
             if arg == "-h" || arg == "--help" then putStr usage
             else do
                 putStrLn "Think of a word (blank for random): "
                 word <- sgetLine arg
                 putStrLn "Try to guess it:"
                 play word 0

{-

The action sgetLine reads a line of text from the keyboard, echoing each
character as a dash to keep the word secret. If there is no text inputted, it
reads a random line from the local dictionary.

-}

sgetLine :: String -> IO String
sgetLine dictionary_file = do input <- getInputOrDict
                              putStrLn (map (const '-') input)
                              return input
    where getInputOrDict =  do
              input <- getInput
              if input == "" then do
                  dict  <- lines <$> readFile dictionary_file
                  index <- getStdRandom (randomR (0, length dict))
                  return (dict !! index)
              else
                  return input
          getInput = do x <- getCh
                        if x == '\n' then
                           do return []
                        else
                           do xs <- getInput
                              return (x:xs)

{-

The action getCh reads a single character from the
keyboard, without echoing it to the screen:

-}

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

{-

The function play is the main loop, which requests
and processes the guesses until the game ends.

-}

hangman_phases = [ unlines ["  +---+",
                            "  |   |",
                            "      |",
                            "      |",
                            "      |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            "      |",
                            "      |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            "  |   |",
                            "      |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            " /|   |",
                            "      |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            " /|\\  |",
                            "      |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            " /|\\  |",
                            " /    |",
                            "      |",
                            "========="],
                   unlines ["  +---+",
                            "  |   |",
                            "  O   |",
                            " /|\\  |",
                            " / \\  |",
                            "      |"] ]

play :: String -> Int -> IO ()
play word failures =
   do putStr (hangman_phases !! failures)
      if failures == (length hangman_phases - 1) then putStrLn ("Uh oh! You lose. The word was " ++ word)
      else do
           guess <- take (length word) <$> getLine
           putStrLn ("You guessed: " ++ guess)
           if guess == word then
               putStrLn "You got it!"
           else do
               putStrLn (match word guess)
               play word (failures + 1)

{-

The function match indicates which characters in
one string occur in a second string:

For example, where match word guess :

> match "haskell" "pascal"
"-as--ll"

Note “haskell” matches l twice in “pascal”:

-}

match :: String -> String -> String
match xs ys =
   [if elem x ys then x else '-' | x <- xs]

{-

Example:

> hangman 

Think of a word: 
----

Try to guess it:
? asd
--s-

? gas
--s-

? was
--s-

? ter
te-t

? test
You got it!

-}

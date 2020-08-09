import Control.Monad.State
import Data.List
import Data.Maybe
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
                title <- readFile "title.txt"
                putStr title
                putStrLn ""
                putStrLn "Type QUIT in all caps to quit the game"
                putStrLn ""
                game arg
    where game dict = do putStrLn "Think of a word (blank for random): "
                         word <- sgetLine dict
                         putStr (phases !! 0)
                         putStrLn "Try to guess it:"
                         runStateT (play word) mempty
                         putStr "Play again? (Y/n) "
                         hFlush stdout
                         yn <- getChar
                         if yn /= 'n' then game dict
                         else return ()

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

phases = [ unlines ["  +---+",
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

play :: String -> StateT (String, String) IO ()
play word =
   do (s, failures) <- get
      guess <- liftIO $ take (length word) <$> getLine
      when (guess == "QUIT") (fail "User quit")
      let s' = match word (guess ++ s)
          mismatches = notmatching word guess
          failures' = failures ++ mismatches
          max_failures = length phases - 1
          phase = phases !! min max_failures (length failures')
      put (s', failures')
      liftIO (displayHangman phase failures')
      if word == s' && length failures' <= max_failures then
          liftIO $ putStrLn ("You got it! The word was: " ++ word)
      else
          if length failures' >= max_failures then
              liftIO $ putStrLn ("Uh oh! You lose. The word was: " ++ word)
          else do
              liftIO (putStrLn s')
              play word

displayHangman :: String -> String -> IO ()
displayHangman phase failures = do
    let ls = lines phase
    putStr $ ls !! 0 ++ "\t\tIncorrect guesses:\n"
    putStr $ ls !! 1 ++ "\n"
    putStr $ ls !! 2 ++ "\t\t" ++ nub failures ++ "\n"
    putStr $ unlines (drop 3 ls)

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

Return non matching chars.

-}
notmatching :: String -> String -> String
notmatching xs ys = catMaybes $ map (f xs) ys
    where f xs '-' = Nothing
          f xs x   = if x `elem` xs then Nothing else Just x


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

import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Language.Hunspell
import System.Environment
import System.IO
import System.Random

main :: IO ()
main = hangman

default_words = "words"
usage = unlines ["Usage: hangman [--words=RANDOM_WORDS_LIST] [HUNSPELL_AFF_FILE] [HUNSPELL_DIC_FILE]",
                 "",
                 "NOTE: RANDOM_WORDS_LIST defaults to " ++ default_words,
                 "Spellchecker requires hunspell. Pass the path to the AFF file & DIC files, and spellchecking will be enabled."]

data Config = Config (Maybe SpellChecker) String | Help

hangman :: IO ()
hangman = parseArgs >>= handleArgs
    where handleArgs (Help)       = putStr usage
          handleArgs conf         =  do
                title <- readFile "title.txt"
                putStr title
                putStrLn ""
                putStrLn "Type QUIT in all caps to quit the game"
                putStrLn ""
                game conf
          game (Config sc wordsf) = do
                word <- askForWord sc wordsf
                putStrLn ""
                putStrLn "Try to guess it:"
                runStateT (play "" word) mempty
                putStr "Play again? (Y/n) "
                hFlush stdout
                yes <- askYN True
                if yes then game (Config sc wordsf)
                else return ()
          thinkOfWord = "Think of a word (blank for random): "
          askForWord (Nothing) wordsf = do
                putStrLn thinkOfWord
                sgetLine wordsf
          askForWord (Just sc) wordsf = do
                putStrLn thinkOfWord
                word <- sgetLine wordsf
                r <- spell sc word
                if r then return word
                else do
                    putStrLn "Spell-check error - did you spell that correctly? (y/N) "
                    yes <- askYN False
                    if not yes then askForWord (Just sc) wordsf
                    else return word

          askYN def = (listToMaybe . map toLower <$> getLine) >>= return . maybe def (=='y')

parseArgs :: IO Config
parseArgs = do args <- getArgs
               let words = maybe default_words id (listToMaybe . catMaybes $ map getWords args)
               s <- getSpellChecker args
               return (Config s words)
    where getWords = stripPrefix "--words="
          getSpellChecker          = getSpellChecker' . filter (isNothing . stripPrefix "--words=")
          getSpellChecker' (a:b:_) = Just <$> createSpellChecker a b
          getSpellChecker'  xs     = return Nothing
{-

The action sgetLine reads a line of text from the keyboard, echoing each
character as a dash to keep the word secret. If there is no text inputted, it
reads a random line from the local dictionary.

-}

sgetLine :: String -> IO String
sgetLine words_file = do
              input <- getInput
              if input == "" then do
                  dict  <- lines <$> readFile words_file
                  index <- getStdRandom (randomR (0, length dict))
                  return (dict !! index)
              else
                  return input
    where getInput = do x <- getCh
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

play :: (MonadIO m, MonadFail m) => String -> String -> StateT (String, String) m ()
play guess word =
   do (s, failures) <- get
      when (guess == "QUIT") (fail "User quit")
      let s' = match word $ map toLower (guess ++ s)
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
              guess <- liftIO getLine
              play guess word

displayHangman :: String -> String -> IO ()
displayHangman phase failures =
    if length failures == 0 then putStr phase
    else do let ls = lines phase
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
   [if elem (toLower x) (map toLower ys) then x else '-' | x <- xs]

{-

Return non matching chars.

-}
notmatching :: String -> String -> String
notmatching xs ys = catMaybes $ map (f $ map toLower xs) ys
    where f xs '-' = Nothing
          f xs x   = if toLower x `elem` xs then Nothing else Just x


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

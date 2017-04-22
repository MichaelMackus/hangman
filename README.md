# Hangman-Game-Haskell
Implementation of the game Hangman in Haskell.

At the start of the game, one player secretly enters a word. Another player then tries to deduce the word via series of guesses. For each guess, we indicate which letters in the secret word occur in the guess, and the game ends when the guess is correct.

# Rules
Consider the following version of hangman:

- One player secretly types in a word.

- The other player tries to deduce the word, by entering a sequence of guesses.

- For each guess, the computer indicates which letters in the secret word occur in the guess.

- The game ends when the guess is correct. We adopt a top down approach to implementing

# Example
```
*Main> hangman 
Think of a word: 
------
Try to guess it:
? s
s-----
? test
se--et
? cross
s-cr--
? secret
You got it!
```
# Reference
This game is an example which illustrates the basics of IO programming.

The materials were gathered from the book "Programming in Haskell" http://www.cs.nott.ac.uk/~pszgmh/pih.html

package com.dallinhuff.evilhangman

import GuessResult.{Invalid, Solved, Lost, Next}

/**
 * An immutable snapshot of an evil hangman game
 *
 * @param words all words that the "secret word" could possibly be
 * @param pattern the word the user is trying to fill in
 * @param guessesLeft the number of incorrect guesses the user can make before losing
 * @param guessesMade a set of guesses the user has already made
 */
case class EvilHangmanGame(
  words: Set[String],
  pattern: String,
  guessesLeft: Int,
  guessesMade: Set[Char]
):
  def guess(guess: Char): GuessResult =
    if guessesMade.contains(guess) then
      Invalid(s"You already guessed $guess!")
    else
      bestPattern(guess) match
        case (pattern, _) if !pattern.contains('-') =>
          Solved(pattern)
        case (pattern, words) =>
          val nGuess = pattern match
            case this.pattern => guessesLeft - 1
            case _ => guessesLeft
          nGuess match
            case 0 => Lost(words.head)
            case _ => Next(EvilHangmanGame(words, pattern, nGuess, guessesMade + guess))

  private def bestPattern(char: Char): (String, Set[String]) =
    words
      .groupBy(
        _.zip(pattern)
          .map(t => if t(0) == char then t(0) else t(1))
          .mkString
      )
      .reduceLeft:
        case (best @ (bestKey, bestWords), curr @ (currKey, currWords)) =>
          currWords.size compare bestWords.size match
            // pick the option that limits the number of possible words the least
            case 1 => curr
            case -1 => best
            case 0 =>
              currKey.count(_ == char) compare bestKey.count(_ == char) match
                // if it's a tie, pick the option that has the fewest occurrences of char
                case -1 => curr
                case 1 => best
                case 0 =>
                  // if it's still a tie, pick the option that has the last first occurrence of char
                  currKey.indexOf(char) compare bestKey.indexOf(char) match
                    case 1 => curr
                    case _ => best


  private lazy val incorrectGuesses: List[Char] =
    guessesMade.filterNot(pattern.contains).toList.sorted

  override def toString: String =
    s"You have $guessesLeft ${if guessesLeft == 1 then "guess" else "guesses"} left\n" ++
    s"Word: $pattern\n" ++
    s"Incorrect guesses: ${incorrectGuesses.mkString(", ")}"

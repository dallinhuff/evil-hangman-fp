package com.dallinhuff.evilhangman

/**
 * An immutable snapshot of an evil hangman game
 * @param words all words that the "secret word" could possibly be
 * @param pattern the word the user is trying to fill in
 * @param guessesLeft the number of incorrect guesses the user can make before losing
 * @param guessesMade a set of guesses the user has already made
 */
case class EvilHangmanGame(words: Set[String], pattern: String, guessesLeft: Int, guessesMade: Set[Char]):

  /**
   * make a guess with the current game state
   * @param guess the letter to guess
   * @return the result of making that guess (an error string, an end-game reporting string, or a new game state)
   */
  def guess(guess: Char): Either[String, Either[Either[String, String], EvilHangmanGame]] =
    if guessesMade.contains(guess) then
      Left(s"You already guessed $guess!")
    else
      val (pattern, words) = bestPattern(guess)
      if !pattern.contains('-') then
        Right(Left(Right(pattern)))
      else
        val nGuess = if pattern == this.pattern then guessesLeft - 1 else guessesLeft
        if nGuess == 0 then Right(Left(Left(words.head)))
        else Right(Right(EvilHangmanGame(words, pattern, nGuess, guessesMade + guess)))

  /**
   * given a guess, find the best new pattern and the
   * members of words that match that pattern
   * @param char the guess to create pattern groups from
   * @return the pattern that will maximize our chances of beating the user
   *         and all of the words left that match this pattern
   */
  private def bestPattern(char: Char): (String, Set[String]) =
    words
      .groupBy(_.zip(pattern).map(t => if t(0) == char then t(0) else t(1)).mkString)
      .reduceLeft:
        case (best @ (bestKey, bestWords), curr @ (currKey, currWords)) =>
          if currWords.size > bestWords.size then curr
          else if currWords.size == bestWords.size then
            if currKey.count(_ == char) > bestKey.count(_ == char) then curr
            else best
          else best

  /**
   * a string representation of the current game state
   */
  override def toString: String =
    s"You have $guessesLeft ${if guessesLeft == 1 then "guess" else "guesses"} left\n" ++
    s"Word: $pattern\n" ++
    s"Incorrect guesses: ${guessesMade.filterNot(pattern.contains).toList.sorted.mkString(", ")}"

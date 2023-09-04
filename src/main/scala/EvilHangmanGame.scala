package com.dallinhuff.evilhangman

/**
 * An immutable snapshot of a game of evil hangman
 * @param pattern the word with blanks the user is trying to fill out
 * @param guessesLeft the number of incorrect guesses the user has left before they lose
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
      val (pattern, words) = biggestPartition(guess)
      if !pattern.contains('-') then
        Right(Left(Right(pattern)))
      else
        val nGuess = if pattern == this.pattern then guessesLeft - 1 else guessesLeft
        if nGuess == 0 then
          Right(Left(Left(words.head)))
        else
          Right(Right(EvilHangmanGame(words, pattern, nGuess, guessesMade + guess)))

  /**
   * given a guess, find the best new pattern and the
   * members of words that match that pattern
   * @param char the guess to partition words with
   * @return the best-case entry from partitions that will
   *         maximize our chances of beating the user
   */
  private def biggestPartition(char: Char): (String, Set[String]) =
    val parts = partitions(char)
    parts.foldLeft(parts.head):
      case ((bestKey, bestWords), (currKey, currWords)) =>
        if currWords.size > bestWords.size then (currKey, currWords)
        else if currWords.size == bestWords.size then
          if currKey.count(_ == char) > bestKey.count(_ == char) then
            (currKey, currWords)
          else
            (bestKey, bestWords)
        else (bestKey, bestWords)

  /**
   * @param char the guess to partition words with
   * @return a Map of possible new patterns to members of words that match them
   */
  private def partitions(char: Char): Map[String, Set[String]] =
    def getKey(word: String): String = word
      .zip(pattern)
      .map:
        case (c, _) if c == char => c
        case (_, c) => c
      .mkString
    words.groupMap(getKey)(word => word)

  /**
   * a string representation of the current game state
   */
  override def toString: String =
    val guessStr = if guessesLeft == 1 then "guess" else "guesses"
    s"You have $guessesLeft $guessStr left\n" ++
    s"Word: $pattern\n" ++
    s"Incorrect guesses: ${guessesMade.filterNot(pattern.contains).toList.sorted.mkString(", ")}"


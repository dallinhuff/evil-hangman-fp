package com.dallinhuff.evilhangman

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.Try

object EvilHangmanApp:

  /**
   * create a HangmanGame from the supplied command line arguments (or provide defaults)
   * and play the game until the user wins/loses
   * If the supplied arguments are bad or an error occurs when loading the dictionary file,
   * print an the error
   */
  def main(args: Array[String]): Unit =
    val (filename, length, guess) = args match
      case Array(f, l, n) => (f, l.toInt, n.toInt)
      case Array(f, l) => (f, l.toInt, 6)
      case Array(f) => (f, 7, 6)
      case _ => ("dict.txt", 7, 6)

    create(filename, length, guess) match
      case Left(errorMsg) => println(errorMsg)
      case Right(game) => play(game)

  /**
   * attempt to create an instance of HangmanGame with the given CLI arguments
   * @param filename the path to the file to use as the dictionary of possible words
   * @param length the number of letters to have in the hangman word
   * @param guess the number of incorrect guesses the user can make before losing
   * @return an either with the game instance or a string explaining why creation failed
   */
  private def create(filename: String, length: Int, guess: Int): Either[String, EvilHangmanGame] =
    val source = Source.fromFile(filename)
    Try[EvilHangmanGame]:
      val words = for
        line <- source.getLines()
        word <- line.split("\\s+") if word.length == length && word.nonEmpty
      yield word.toLowerCase

      if words.isEmpty then
        throw new IllegalArgumentException(s"Not enough words of length $length.")
      else
        EvilHangmanGame(words.toSet, "-".repeat(length), guess, Set[Char]())
    .fold(
      err =>
        source.close()
        Left(s"Couldn't create game from dictionary: $filename\n${err.getMessage}"),
      game =>
        source.close()
        Right(game)
    )

  /**
   * print the current game status and take the user's next guess
   * print result of guess and repeat until the word is guessed
   * or the user runs out of guesses
   * @param game the current HangmanGame
   */
  @tailrec
  private def play(game: EvilHangmanGame): Unit =
    println()
    println(game)
    val next = nextGuess
    game.guess(next) match
      case Left(err) =>
        println(s"\n$err")
        play(game)
      case Right(Left(Left(solution))) =>
        println(s"\nSorry, there are no $next's.\n")
        println(s"You lose! The word was $solution.")
      case Right(Left(Right(solution))) =>
        println(s"You win! The word was $solution.")
      case Right(Right(newGame)) =>
        if newGame.pattern == game.pattern then
          println(s"\nSorry, there are no $next's.")
        play(newGame)

  /**
   * get the next (valid) guess from stdin
   * @return a single lowercase letter to use as the next guess
   */
  @tailrec
  private def nextGuess: Char =
    print("Next guess: ")
    val line = readLine().trim
    if line.length != 1 || !line.charAt(0).isLetter then
      println("Invalid guess! enter a single letter a-z")
      nextGuess
    else line.charAt(0).toLower

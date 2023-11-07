package com.dallinhuff.evilhangman

import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import GuessResult.{Invalid, Lost, Next, Solved}

import scala.io.Source

object EvilHangmanApp extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    val filename = args.headOption.getOrElse("dict.txt")
    val length = args.lift(1).getOrElse("7").toInt
    val guess = args.lift(2).getOrElse("6").toInt

    for
      wordsE <- parseDictionary[IO](filename, length)
      exitCode <- wordsE.fold(
        err =>
          IO.println(s"Couldn't create game from dictionary: $filename\n$err") >>
          IO.pure(ExitCode.Error),
        dict => play(EvilHangmanGame(dict, "-" * length, guess, Set.empty[Char]))
      )
    yield exitCode

  /**
   * attempt to create a set of words from a dictionary file
   * @param filename the name of the file to attempt to use as a dictionary
   * @param length the length of the word to use in the hangman game
   * @tparam F the effect to use
   * @return either an error string or a set of strings to use as a game dictionary
   */
  private def parseDictionary[F[_] : Sync](filename: String, length: Int): F[Either[String, Set[String]]] =
    Resource.fromAutoCloseable(Sync[F].blocking(Source.fromFile(filename))).use: f =>
      val words = for
        line <- f.getLines()
        word <- line.split("\\s+") if word.length == length && word.nonEmpty
      yield word.toLowerCase

      Sync[F].delay(Either.cond(words.nonEmpty, Set.from(words), s"Not enough words of length $length."))

  private def play(game: EvilHangmanGame): IO[ExitCode] =
    IO.defer:
      for
        _ <- IO.print("\u001b[2J\u001b[H\n")
        _ <- IO.println(game)
        next <- nextGuess
        result <- game.guess(next) match
          case Invalid(err) =>
            IO.println(s"\n$err") >>
            play(game)
          case Lost(solution) =>
            IO.println(s"\nSorry, there are no $next's.\n") >>
            IO.println(s"You lose! The word was $solution.") >>
            IO.pure(ExitCode.Success)
          case Solved(solution) =>
            IO.println(s"You win! The word was $solution.") >>
            IO.pure(ExitCode.Error)
          case Next(newGame @ EvilHangmanGame(_, p, _, _)) =>
            p match
              case game.pattern =>
                IO.println(s"\nSorry, there are no $next's.") >>
                play(newGame)
              case _ => play(newGame)
      yield result

  private def nextGuess: IO[Char] =
    IO.defer:
      for
        _ <- IO.print("Next guess: ")
        line <- IO.readLine
        trimmed <- IO.pure(line.trim)
        letter <- IO(Option.when(trimmed.length == 1 && trimmed.charAt(0).isLetter)(trimmed))
        result <- letter match
          case Some(l) => IO.pure(l.head.toLower)
          case None => IO.println("Invalid guess! enter a single letter a-z") >> nextGuess
      yield result

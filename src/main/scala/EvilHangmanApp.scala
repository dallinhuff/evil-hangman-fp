package com.dallinhuff.evilhangman

import GuessResult.{Invalid, Lost, Next, Solved}

import cats.effect.{ExitCode, IO, Resource, Sync}
import cats.syntax.all.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

import scala.io.Source

/**
 * The CLI-app with all effectful/IO logic necessary to play EvilHangmanGame
 */
object EvilHangmanApp extends CommandIOApp(
  name = "evil-hangman",
  header = "A command-line evil hangman game",
  version = "0.1.0"
):

  override def main: Opts[IO[ExitCode]] =
    val opts = (
      Opts.option[String]("dictionary", "Name of the dictionary file", "d")
        .withDefault("dict.txt"),
      Opts.option[Int]("length", "Length of the word to use", "l")
        .withDefault(7),
      Opts.option[Int]("guess", "Number of guesses allowed", "g")
        .withDefault(6)
    ).tupled

    opts.map:
      case (filename, length, guess) =>
        for
          wordsE <- parseDictionary[IO](filename, length)
          exitCode <- wordsE.fold(
            err =>
              IO.println(s"Couldn't create game from dictionary: $filename\n$err") >>
              IO.pure(ExitCode.Error),
            dict => play(EvilHangmanGame(dict, "-" * length, guess, Set.empty[Char]))
          )
        yield exitCode
  
  private def parseDictionary[F[_] : Sync](name: String, length: Int): F[Either[String, Set[String]]] =
    Resource.fromAutoCloseable(Sync[F].blocking(Source.fromFile(name))).use: f =>
      val words = for
        line <- f.getLines()
        word <- line.split("\\s+") if word.length == length && word.nonEmpty
      yield word.toLowerCase

      Sync[F].delay(Either.cond(
        words.nonEmpty,
        Set.from(words),
        s"Not enough words of length $length."
      ))

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
            IO.println(s"\nYou win! The word was $solution.") >>
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
        letter <- IO(
          Option.when(trimmed.length == 1 && trimmed.charAt(0).isLetter)(trimmed)
        )
        result <- letter match
          case Some(l) => IO.pure(l.head.toLower)
          case None =>
            IO.println("Invalid guess! enter a single letter a-z") >>
            nextGuess
      yield result

package com.dallinhuff.evilhangman

enum GuessResult:
  case Invalid(msg: String)
  case Solved(solution: String)
  case Lost(solution: String)
  case Next(game: EvilHangmanGame)
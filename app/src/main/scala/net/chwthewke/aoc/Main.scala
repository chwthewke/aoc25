package net.chwthewke.aoc

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp {

  override def run( args: List[String] ): IO[ExitCode] =
    IO.println( s"${buildinfo.AdventOfCode.name} ${buildinfo.AdventOfCode.version}" ) *>
      IO.println( s"The answer is ${Library.function}" ).as( ExitCode.Success )

}

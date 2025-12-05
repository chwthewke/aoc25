package net.chwthewke.aoc

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp:

  private val puzzles: Vector[Puzzle.Any] = Vector( Puzzle1, Puzzle2, Puzzle3, Puzzle4, Puzzle5 )

  private case class Args( number: Int, useSample: Boolean, runBonus: Boolean )

  private object Args:
    def unapply( s: String ): Option[Args] =
      val ( t, useSample ) = ( s.stripSuffix( "?" ), s.endsWith( "?" ) )
      val ( u, runBonus )  = ( t.stripSuffix( "*" ), t.endsWith( "*" ) )
      u.toIntOption.map( Args( _, useSample, runBonus ) )

  private def getPuzzle( number: Int ): Either[IO[Unit], Puzzle.Any] =
    puzzles
      .find( _.puzzleNumber == number )
      .toRight( IO.println( s"No puzzle found with number $number" ) )

  override def run( args: List[String] ): IO[ExitCode] =
    args match
      case Args( puzzle ) :: Nil =>
        getPuzzle( puzzle.number )
          .map( Runner.run( _, puzzle.runBonus, puzzle.useSample ) )
          .merge
          .as( ExitCode.Success )
      case _ => IO.println( "Invalid arguments" ).as( ExitCode.Error )

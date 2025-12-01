package net.chwthewke.aoc

import cats.Id
import cats.Show
import cats.arrow.FunctionK
import cats.effect.Clock
import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import cats.~>

import Loader.Source
import Shell.Eff
import Shell.Read

object Runner:
  private val clock: Clock[IO]     = Clock[IO]
  private val console: Console[IO] = Console[IO]

  private def nat[F[_], E]( eff: Eff[F, E] ): F ~> IO =
    eff match
      case Eff.EitherEff =>
        new FunctionK[Either[String, *], IO]:
          override def apply[A]( fa: Either[String, A] ): IO[A] =
            fa.leftMap( new RuntimeException( _ ) ).liftTo[IO]
      case Eff.IOEff => FunctionK.id[IO]
      case Eff.IdEff =>
        new FunctionK[Id, IO]:
          override def apply[A]( fa: A ): IO[A] = IO.delay( fa )

  private def onInput[F[_], I, O]( run: I => F[O], read: Read[I] )( input: Input ): F[O] =
    read match
      case Read.All     => run( input.raw )
      case Read.Lines   => run( input.lines )
      case Read.Trimmed => run( input.trimmed )

  def run( puzzle: Puzzle.Any, runBonus: Boolean, useSample: Boolean ): IO[Unit] =
    puzzle match
      case p: Puzzle[_, _, _, _] => runPuzzle( p, runBonus, useSample )

  def runPuzzle[F[_], I, O, E]( puzzle: Puzzle[F, I, O, E], runBonus: Boolean, useSample: Boolean ): IO[Unit] =
    val task: puzzle.shell.In => puzzle.shell.Effect[puzzle.shell.Out] =
      if ( runBonus ) puzzle.runBonus else puzzle.run

    val inputName: String =
      puzzle.puzzleNumber.toString
        + ( if ( runBonus && puzzle.shell.hasBonusInput ) "-bonus" else "" )
        + ".txt"

    val source: Source = if ( useSample ) Source.Samples else Source.Inputs

    for
      t0     <- clock.monotonic
      input  <- Loader.load[IO]( source, inputName )
      t1     <- clock.monotonic
      output <- nat( puzzle.shell.eff )( onInput( task, puzzle.shell.read )( input ) )
      t2     <- clock.monotonic
      _      <- console.errorln(
             show"[${( t2 - t0 ).toMillis}ms] [setup=${( t1 - t0 ).toMillis}ms] [run=${( t2 - t1 ).toMillis}ms]"
           )
      _ <- console.println( output )( using puzzle.shell.show )
    yield ()

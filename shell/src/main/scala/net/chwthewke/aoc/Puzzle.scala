package net.chwthewke.aoc

abstract class Puzzle[F[_], I, O, E]( val puzzleNumber: Int, val shell: Shell.Aux[F, I, O, E] ) extends Puzzle.Any:
  protected final def raise[A]( e: shell.Error ): shell.Effect[A] = shell.eff.raise( e )

  def run( in: shell.In ): shell.Effect[shell.Out]
  def runBonus( in: shell.In ): shell.Effect[shell.Out]

object Puzzle:
  sealed abstract class Any:
    def puzzleNumber: Int

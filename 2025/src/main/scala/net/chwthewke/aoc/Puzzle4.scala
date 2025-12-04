package net.chwthewke.aoc

import cats.syntax.all.*
import scala.annotation.tailrec

object Puzzle4 extends Puzzle( 4, Shell.default ):

  case class Pos( x: Int, y: Int ):
    def neighbours8: Vector[Pos] =
      ( for
        dx <- -1 to 1
        dy <- -1 to 1 if dx != 0 || dy != 0
      yield Pos( x + dx, y + dy ) ).toVector

    override def toString: String = s"($x, $y)"

  case class Grid( width: Int, height: Int, rolls: Set[Pos] ):
    def neighbours8( p: Pos ): Vector[Pos] =
      p.neighbours8.filter:
        case Pos( x, y ) =>
          x >= 0 && x < width && y >= 0 && y < height

    def access( p: Pos ): Boolean =
      rolls( p ) && neighbours8( p ).count( rolls.contains ) < 4

    def count( pred: Pos => Boolean ): Int =
      ( 0 until width ).foldLeft( 0 ):
        case ( acc1, x ) =>
          ( 0 until height ).foldLeft( acc1 ):
            case ( acc2, y ) =>
              val pos = Pos( x, y )
              if pred( pos )
              then acc2 + 1
              else acc2

    def removeAccessible: ( Grid, Int ) =
      val next = copy( rolls = rolls.filterNot( access ) )
      ( next, rolls.size - next.rolls.size )

  object Grid:
    def parse( lines: Vector[String] ): Grid =
      val h     = lines.length
      val w     = lines.map( _.length ).max
      val rolls = lines.zipWithIndex.foldMap:
        case ( l, y ) =>
          l.zipWithIndex.toVector
            .mapFilter:
              case ( c, x ) => Option.when( c == '@' )( Set( Pos( x, y ) ) )
            .combineAll
      Grid( w, h, rolls )

  override def run( in: Vector[String] ): Int =
    val grid = Grid.parse( in )
    grid.count( p => grid.access( p ) )

  override def runBonus( in: Vector[String] ): Int =
    val grid = Grid.parse( in )
    @tailrec
    def go( g: Grid, acc: Int ): Int =
      val ( next, removed ) = g.removeAccessible
      if removed == 0
      then acc
      else go( next, acc + removed )
    go( grid, 0 )

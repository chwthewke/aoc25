package net.chwthewke.aoc

import cats.Monoid
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Rfc5234
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Puzzle5 extends Puzzle( 5, Shell.either.reads( Shell.Read.All ).returns[Long] ):

  object parsers:
    val eol: Parser[Unit]          = Parser.oneOf( List( Rfc5234.cr *> Rfc5234.lf, Rfc5234.lf ) )
    val interval: Parser[Interval] =
      (
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption ),
        Parser.char( '-' ).void,
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption )
      ).mapN( ( f, _, t ) => Interval( f, t ) )
    val intervals: Parser[Intervals] =
      interval.repSep( eol ).map( xs => Intervals( xs.toList ) )
    val inventory: Parser[Inventory] =
      (
        intervals <* eol <* eol,
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption ).repSep( eol ).map( _.toList.toVector ) <* eol.rep0
      ).mapN( Inventory( _, _ ) )

  case class Interval( from: Long, to: Long ):
    def includes( other: Interval ): Boolean = from <= other.from && to >= other.to
    def touches( other: Interval ): Boolean  = !( from > other.to + 1 || other.from > to + 1 )
    def contains( x: Long ): Boolean         = x >= from && x <= to
    def length: Long                         = to - from + 1

  object Interval:
    given Show[Interval] = Show.show:
      case Interval( from, to ) => s"[$from, $to]"
    given Order[Interval] = Order.by:
      case Interval( from, to ) => ( from, to )

  case class Intervals( contents: SortedSet[Interval] ):
    def incl( interval: Interval ): Intervals =
      Intervals( Intervals.addLoop( contents, interval ) )
    def contains( x: Long ): Boolean =
      contents.maxBefore( Interval( x + 1, x + 1 ) ).exists( _.contains( x ) )
    def size: Long = contents.foldMap( _.length )

  object Intervals:
    def apply( intervals: Iterable[Interval] ): Intervals =
      intervals.foldLeft( Intervals( SortedSet.empty[Interval] ) )( _.incl( _ ) )

    given Show[Intervals] = Show.show:
      case Intervals( contents ) => contents.mkString_( ", " )

    given Monoid[Intervals]:
      override def empty: Intervals = Intervals( SortedSet.empty[Interval] )

      override def combine( x: Intervals, y: Intervals ): Intervals =
        Intervals(
          y.contents.foldLeft( x.contents ): ( acc, interval ) =>
            acc.incl( interval )
        )

    @tailrec
    private def addLoop( intervals: SortedSet[Interval], interval: Interval ): SortedSet[Interval] =
      val ub = interval.to + 2
      intervals
        .maxBefore( Interval( ub, ub ) )
        .filter( interval.touches ) match
        case None             => intervals + interval
        case Some( touching ) =>
          if ( touching.includes( interval ) ) intervals
          else addLoop( intervals - touching, Interval( interval.from min touching.from, interval.to max touching.to ) )

  case class Inventory( fresh: Intervals, ingredients: Vector[Long] )

  object Inventory:
    given Show[Inventory] = Show.show:
      case Inventory( fresh, ingredients ) =>
        show"""Fresh: $fresh
              |Ingredients: ${ingredients.mkString( ", " )}
              |""".stripMargin

  override def run( in: String ): Either[String, Long] =
    parsers.inventory
      .parseAll( in )
      .leftMap( err => s"Parse error: ${err.show}" )
      .map: inv =>
        inv.ingredients.count( inv.fresh.contains ).toLong

  override def runBonus( in: String ): Either[String, Long] =
    parsers.inventory
      .parseAll( in )
      .leftMap( err => s"Parse error: ${err.show}" )
      .map: inv =>
        inv.fresh.size

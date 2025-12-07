package net.chwthewke.aoc

import cats.Monoid
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

object Puzzle7 extends Puzzle( 7, Shell.either.returns[Long] ):
  case class Pos( x: Int, y: Int )

  object Pos:
    given Order[Pos] = Order.by( p => ( p.y, p.x ) )

  enum Cell:
    case Empty
    case Splitter
    case Start

  case class Grid( height: Int, width: Int, start: Pos, splitters: SortedSet[Pos] ):
    def neighbours( p: Pos ): Vector[Pos] =
      if ( splitters.contains( p ) )
        Vector(
          Option.when( p.x > 0 )( Pos( p.x - 1, p.y ) ),
          Option.when( p.x < width - 1 )( Pos( p.x + 1, p.y ) )
        ).flattenOption
      else
        Option.when( p.y < height - 1 )( Pos( p.x, p.y + 1 ) ).toVector

    def splittersVisited: Int =
      def go( open: Vector[Pos], seen: Set[Pos] ): Int =
        if ( open.isEmpty ) seen.intersect( splitters ).size
        else if ( seen( open.head ) ) go( open.tail, seen )
        else
          val next = neighbours( open.head )
          go( open.tail ++ next, seen + open.head )

      go( Vector( start ), Set.empty )

    val rows: Vector[Vector[Cell]] =
      ( 0 until height ).toVector.map: y =>
        ( 0 until width ).toVector.map: x =>
          val p = Pos( x, y )
          if ( p == start ) Cell.Start
          else if ( splitters( p ) ) Cell.Splitter
          else Cell.Empty

    def timelines: Either[String, Long] =
      @tailrec
      def go( accBelow: Vector[Long], remRows: Vector[Vector[Cell]] ): Either[String, Long] =
        if ( remRows.isEmpty ) Left( "no start" )
        else
          val next: Either[Long, Vector[Long]] = remRows.head.zipWithIndex.traverse:
            case ( c, x ) =>
              if ( c == Cell.Start ) Left( accBelow( x ) )
              else if ( c == Cell.Empty ) Right( accBelow( x ) )
              else Right( Vector( accBelow.lift( x - 1 ), accBelow.lift( x + 1 ) ).flattenOption.sum )
          next match
            case Left( startTimelines ) => Right( startTimelines )
            case Right( rowTimelines )  => go( rowTimelines, remRows.tail )
      go( Vector.fill( width )( 1L ), rows.reverse )

  object Grid:
    private case class Acc( width: Option[Int], start: Option[Pos], splitters: SortedSet[Pos] )
    private object Acc:
      given Monoid[Acc]:
        override def empty: Acc = Acc( none, none, SortedSet.empty )

        override def combine( x: Acc, y: Acc ): Acc =
          Acc( x.width.orElse( y.width ), x.start.orElse( y.start ), x.splitters ++ y.splitters )

    def parse( in: Vector[String] ): Either[String, Grid] =
      val Acc( w, s, p ) =
        in.zipWithIndex.foldMap:
          case ( row, y ) =>
            row.zipWithIndex.toVector
              .foldMap:
                case ( c, x ) =>
                  Acc(
                    none,
                    Option.when( c == 'S' )( Pos( x, y ) ),
                    Option.when( c == '^' )( Pos( x, y ) ).to( SortedSet )
                  )
              .copy( width = row.length.some )
      ( w, s ).mapN( Grid( in.length, _, _, p ) ).toRight( "parse error" )

  override def run( in: Vector[String] ): Either[String, Long] =
    Grid.parse( in ).map( _.splittersVisited.toLong )

  override def runBonus( in: Vector[String] ): Either[String, Long] =
    Grid.parse( in ).flatMap( _.timelines )

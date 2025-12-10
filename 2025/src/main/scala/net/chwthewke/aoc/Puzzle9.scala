package net.chwthewke.aoc

import alleycats.std.iterable.*
import cats.Monoid
import cats.Order
import cats.Order.catsKernelOrderingForOrder
import cats.Show
import cats.data.Ior
import cats.data.NonEmptyVector
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object Puzzle9 extends Puzzle( 9, Shell.either.returns[Long] ):
  object parsers:
    val pos: Parser[Pos] =
      (
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption ) <* Parser.char( ',' ),
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
      ).mapN( Pos( _, _ ) )

  case class Pos( x: Int, y: Int ):
    def rect( other: Pos ): Long =
      ( 1 + ( x - other.x ).abs ).toLong * ( 1 + ( y - other.y ).abs ).toLong

    def pathTo( other: Pos ): Option[Iterable[Pos]] =
      Option
        .when( x == other.x )( interior( y, other.y ).map( Pos( x, _ ) ) )
        .orElse( Option.when( y == other.y )( interior( x, other.x ).map( Pos( _, y ) ) ) )

  object Pos:
    given Show[Pos] = Show.show:
      case Pos( x, y ) => s"($x,$y)"

  // OA.OB
  def scalar( o: Pos, a: Pos, b: Pos ): Int =
    ( a.x - o.x ) * ( b.x - o.x ) + ( a.y - o.y ) * ( b.y - o.y )

  def parseInput( in: Vector[String] ): Either[String, Vector[Pos]] =
    in.traverse( line => parsers.pos.parseAll( line ).leftMap( err => s"Parse error in $line: ${err.show}" ) )

  private def interior( a: Int, b: Int ): Seq[Int] =
    if ( a < b ) ( a + 1 ) until b
    else if ( b < a ) ( b + 1 ) until a
    else Vector.empty

  enum Dir:
    case Up
    case Down
    case Left
    case Right

  object Dir:
    def path( a: Pos, b: Pos ): Option[Dir] =
      if ( a.x == b.x )
        val c = b.y.compareTo( a.y )
        if ( c > 0 ) Down.some
        else if ( c < 0 ) Up.some
        else None
      else if ( a.y == b.y )
        val c = b.x.compareTo( a.x )
        if ( c > 0 ) Right.some
        else if ( c < 0 ) Left.some
        else None
      else None

  case class Interval( from: Int, to: Int ):
    def includes( other: Interval ): Boolean = from <= other.from && to >= other.to

    def touches( other: Interval ): Boolean = !( from > other.to + 1 || other.from > to + 1 )

    def contains( x: Int ): Boolean = x >= from && x <= to

    def length: Int = to - from + 1

  object Interval:
    given Show[Interval] = Show.show:
      case Interval( from, to ) => s"[$from, $to]"

    given Order[Interval] = Order.by:
      case Interval( from, to ) => ( from, to )

  case class Intervals( contents: SortedSet[Interval] ):
    def incl( interval: Interval ): Intervals =
      Intervals( Intervals.addLoop( contents, interval ) )

    private def maxBefore( x: Int ): Option[Interval] =
      contents.maxBefore( Interval( x, x ) )

    def contains( x: Int ): Boolean =
      maxBefore( x + 1 ).exists( _.contains( x ) )

    def includes( interval: Interval ): Boolean =
      maxBefore( interval.from + 1 ).exists( _.includes( interval ) )

    def size: Int = contents.foldMap( _.length )

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

  case class Row( y: Int, onRow: Intervals, below: Intervals )

  val debug: Boolean = false

  type Stop = Ior[Unit, Int]
  object Stop:
    val up: Stop             = Ior.left( () )
    def red( ix: Int ): Stop = Ior.right( ix )

  // positions on a row with either a red (except if the paths are Left-Right), or a green on a vertical path
  case class Node(
      x: Int,
      wasInside: Boolean,
      isInside: Boolean,
      hasPathDown: Boolean,
      insideIsUp: Option[Boolean]
  )

  case class IntervalsAcc( intervals: Vector[Interval], open: Option[Int] ):
    def add( x: Int ): IntervalsAcc =
      open match
        case Some( x0 ) => IntervalsAcc( intervals :+ Interval( x0, x ), none )
        case None       => IntervalsAcc( intervals, Some( x ) )

    def toIntervals: Either[String, Intervals] =
      open.toLeft( Intervals( intervals ): Intervals ).leftMap( x => s"open at $x not closed" )

  object IntervalsAcc:
    val empty: IntervalsAcc = IntervalsAcc( Vector.empty, none )

  case class RowAcc( onRow: IntervalsAcc, below: IntervalsAcc ):
    def add( node: Node ): Either[String, RowAcc] =
      val nextBelow = Option.when( node.hasPathDown )( node.x ).foldLeft( below )( _.add( _ ) )
      if ( node.wasInside && node.isInside )
        RowAcc( onRow, nextBelow ).asRight
      else
        RowAcc( onRow.add( node.x ), nextBelow ).asRight

    def toRow( y: Int ): Either[String, ( Row, SortedSet[Int] )] =
      ( onRow.toIntervals, below.toIntervals ).mapN( ( r, b ) =>
        ( Row( y, r, b ), below.intervals.foldMap( i => SortedSet( i.from, i.to ) ) )
      )

  object RowAcc:
    val empty: RowAcc = RowAcc( IntervalsAcc.empty, IntervalsAcc.empty )

  case class Tiles( reds: Vector[Pos] ):

    val segments: Vector[( Pos, Pos )] = reds.zip( reds.tail :+ reds.head )

    val redsToSegments: Map[Pos, ( Pos, Pos )] =
      reds.zip( ( reds.tail :+ reds.head ).zip( reds.last +: reds.init ) ).toMap

    val greenBorder: Set[Pos] =
      segments.foldMap {
        case ( p, q ) =>
          if ( p.x == q.x ) interior( p.y, q.y ).iterator.map( Pos( p.x, _ ) ).toSet
          else if ( p.y == q.y ) interior( p.x, q.x ).iterator.map( Pos( _, p.y ) ).toSet
          else Set.empty
      }

    def bordersByRow: Either[String, Vector[Row]] =
      reds.zipWithIndex
        .groupByNev( _._1.y )
        .toVector
        .foldLeftM( ( Vector.empty[Row], SortedSet.empty[Int] ) ):
          case ( ( acc, up ), ( y, rowReds ) ) =>
            // we are at row y
            // above, the vertical borders are at the indices in up
            // we want the red/green intervals on this row and the vertical borders going down
            rowBorders(
              y,
              rowReds.foldMap( p => SortedMap( p._1.x -> Stop.red( p._2 ) ) ) |+|
                up.foldMap( x => SortedMap( x -> Stop.up ) )
            ).leftMap(
              s"on row $y: " + _ +
                s"""
                   |acc = $acc
                   |up = $up
                   |rowReds = $rowReds
                   |""".stripMargin
            ).map:
              case ( row, down ) =>
                ( acc :+ row, down.to( SortedSet ) )
        ._1F

    private def rowBorders( y: Int, stops: SortedMap[Int, Stop] ): Either[String, ( Row, SortedSet[Int] )] =
      def red( ix: Int ): Pos =
        reds( ( ix % reds.length + reds.length ) % reds.length )

      def fail( x: Int, msg: String, log: Vector[String] = Vector.empty ): Either[String, Nothing] =
        Left(
          s"""Invariant failed at x=$x: $msg
             |${log.mkString_( "\n" )}
             |""".stripMargin
        )

      def rowOfNodes( nodes: Vector[Node] ): Either[String, ( Row, SortedSet[Int] )] =
        nodes.foldLeftM( RowAcc.empty )( ( acc, node ) => acc.add( node ) ).flatMap( _.toRow( y ) )

      // some simplification is possible (Both/Right distinction in stops is not too useful with Dirs)
      def addStop( acc: Vector[Node], x: Int, stop: Stop ): Either[String, Vector[Node]] =
        stop match
          case Ior.Left( _ ) =>
            val wasInside = acc.lastOption.exists( _.isInside )
            if ( debug && acc.lastOption.flatMap( _.insideIsUp ).isDefined )
              fail( x, s"on edge crossing vert path" )
            else
              ( acc :+ Node( x, wasInside, !wasInside, true, none ) ).asRight
          case Ior.Right( b ) =>
            val dirs: Set[Dir] =
              Vector( b - 1, b + 1 ).mapFilter( ix => Dir.path( Pos( x, y ), red( ix ) ) ).toSet
            val l = dirs.contains( Dir.Left )
            val r = dirs.contains( Dir.Right )
            if ( l && r ) // L, R
              acc.asRight
            else if ( l ) // L, D
              val wasInsideUpOpt = acc.lastOption.flatMap( _.insideIsUp )
              val wasInsideUp    = wasInsideUpOpt.getOrElse( false )
              val wasInside      = acc.lastOption.exists( _.isInside )
              if ( debug && wasInsideUpOpt.isEmpty )
                fail( x, "insideIsUp undefined at (L)" )
              else if ( debug && !wasInside )
                fail( x, "not inside at (L)" )
              else if ( debug && dirs.contains( Dir.Up ) )
                fail( x, "up path but no up stop" )
              else
                ( acc :+ Node( x, true, wasInsideUp, true, none ) ).asRight
            else if ( r ) // R, D
              if ( debug && acc.lastOption.flatMap( _.insideIsUp ).isDefined ) fail( x, "insideIdUp defined at (R)" )
              else if ( debug && dirs.contains( Dir.Up ) ) fail( x, "up path but no up stop" )
              else
                val wasInside = acc.lastOption.exists( _.isInside )
                ( acc :+ Node( x, wasInside, true, true, wasInside.some ) ).asRight
            else // U, D
            if ( debug ) fail( x, "up+down path but no up stop" )
            else
              val wasInside = acc.lastOption.exists( _.isInside )
              ( acc :+ Node( x, wasInside, !wasInside, true, none ) ).asRight
          case Ior.Both( _, b ) =>
            val dirs: Set[Dir] =
              Vector( b - 1, b + 1 ).mapFilter( ix => Dir.path( Pos( x, y ), red( ix ) ) ).toSet
            val l = dirs.contains( Dir.Left )
            val r = dirs.contains( Dir.Right )
            if ( l && r ) // L, R
              if ( debug )
                fail( x, "L-R at up stop" )
              else
                acc.asRight
            else if ( l ) // L, U
              val wasInsideUpOpt = acc.lastOption.flatMap( _.insideIsUp )
              val wasInsideUp    = wasInsideUpOpt.getOrElse( false )
              val wasInside      = acc.lastOption.exists( _.isInside )
              if ( debug && wasInsideUpOpt.isEmpty )
                fail( x, "insideIsUp undefined at (L)" )
              else if ( debug && !wasInside )
                fail( x, "not inside at (L)" )
              else if ( debug && dirs.contains( Dir.Down ) )
                fail( x, "up stop but no up path" )
              else
                ( acc :+ Node( x, true, !wasInsideUp, false, none ) ).asRight
            else if ( r ) // R, U
              if ( debug && acc.lastOption.flatMap( _.insideIsUp ).isDefined ) fail( x, "insideIdUp defined at (R)" )
              else if ( debug && dirs.contains( Dir.Down ) ) fail( x, "up stop but no up path" )
              else
                val wasInside = acc.lastOption.exists( _.isInside )
                ( acc :+ Node( x, wasInside, true, false, ( !wasInside ).some ) ).asRight
            else // U, D
              val wasInside = acc.lastOption.exists( _.isInside )
              ( acc :+ Node( x, wasInside, !wasInside, true, none ) ).asRight

      @tailrec
      def go(
          acc: Vector[Node],
          remStops: SortedMap[Int, Stop]
      ): Either[String, ( Row, SortedSet[Int] )] =
        if ( remStops.isEmpty )
          if ( acc.last._3 )
            fail( stops.last._1, "still inside at the end" )
          else
            rowOfNodes( acc )
        else
          val ( x, stop ) = remStops.head
          addStop( acc, x, stop ) match
            case Left( err )   => fail( x, err )
            case Right( next ) => go( next, remStops.tail )

      go( Vector.empty, stops )
  object Tiles:
    def loopStrict( reds: Vector[Pos] ): Either[String, Tiles] =
      reds.toNev
        .toRight( "empty" )
        .flatMap: redsNev =>
          redsNev.tail
            .foldLeftM( ( redsNev.head, Set.empty[Pos] ) ):
              case ( ( prev, greens ), p ) =>
                prev
                  .pathTo( p )
                  .toRight( show"no line $prev -> $p" )
                  .flatMap: path =>
                    path
                      .foldLeftM( greens ): ( acc, q ) =>
                        Option
                          .when( !acc( q ) )( acc + q )
                          .toRight( show"$prev -> $p intersects previous segment at $q" )
                      .tupleLeft( p )
            .as( Tiles( reds ) )

  override def run( in: Vector[String] ): Either[String, Long] =
    parseInput( in ).flatMap: ps =>
      ( ps, ps ).mapN( _.rect( _ ) ).maxOption.toRight( "empty input" )

  def isRowInside( rows: SortedMap[Int, Row], y: Int, x0: Int, x1: Int ): Boolean =
    rows
      .maxBefore( y + 1 )
      .exists:
        case ( _, row ) =>
          if ( row.y == y ) row.onRow.includes( Interval( x0, x1 ) )
          else row.below.includes( Interval( x0, x1 ) )

  def isColumnInside( rows: SortedMap[Int, Row], x: Int, y0: Int, y1: Int ): Boolean =
    rows
      .maxBefore( y0 + 1 )
      .exists:
        case ( yb, _ ) =>
          val range         = rows.range( yb, y1 + 1 )
          val excludedBelow = range.keySet.map( _ + 1 )
          range.forall:
            case ( y, row ) =>
              // basically, check that x is in the row's onRow and below, except at the extremities
              ( y < y0 || row.onRow.contains( x ) ) && ( y >= y1 || excludedBelow( y ) || row.below.contains( x ) )

  def isRectangleInside( rows: SortedMap[Int, Row], p: Pos, q: Pos ): Boolean =
    val xMin = p.x min q.x
    val xMax = p.x max q.x
    val yMin = p.y min q.y
    val yMax = p.y max q.y

    isRowInside( rows, yMin, xMin, xMax ) &&
    isRowInside( rows, yMax, xMin, xMax ) &&
    isColumnInside( rows, xMin, yMin, yMax ) &&
    isColumnInside( rows, xMax, yMin, yMax )

  def bestRectangleInside( tiles: Tiles, rows: Vector[Row] ): Option[Long] =
    val rowsMap: SortedMap[Int, Row] = rows.fproductLeft( _.y ).to( SortedMap )

    @tailrec
    def go( i: Int, j: Int, best: Option[Long] ): Option[Long] =
//      if ( j == 0 ) println( s"i=$i" )
      if ( i >= tiles.reds.length ) best
      else if ( j > i ) go( i + 1, 0, best )
      else
        val p = tiles.reds( i )
        val q = tiles.reds( j )
        val r = p.rect( q )
        if ( best.exists( _ >= r ) ) go( i, j + 1, best )
        else if ( isRectangleInside( rowsMap, p, q ) )
//          println( show"$p, $q -> $r" )
          go( i, j + 1, r.some )
        else go( i, j + 1, best )

    go( 0, 0, none[Long] )

  override def runBonus( in: Vector[String] ): Either[String, Long] =
    for
      tiles <- parseInput( in ).flatMap( Tiles.loopStrict )
      rows  <- tiles.bordersByRow
//      _ = println( rows )
      res <- bestRectangleInside( tiles, rows ).toRight( "not found" )
    yield res

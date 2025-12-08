package net.chwthewke.aoc

import cats.Show
import cats.data.Kleisli
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all.*
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

import Shell.IsSample

object Puzzle8 extends Puzzle( 8, Shell.either.returns[Long].sampleAware ):

  object parsers:
    val pos3: Parser[Pos3] =
      (
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption ) <* Parser.char( ',' ),
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption ) <* Parser.char( ',' ),
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption )
      ).mapN( Pos3( _, _, _ ) )

  case class Pos3( x: Int, y: Int, z: Int ):
    def distSq( other: Pos3 ): Long =
      val dx = ( x - other.x ).toLong
      val dy = ( y - other.y ).toLong
      val dz = ( z - other.z ).toLong
      dx * dx + dy * dy + dz * dz

    override def toString: String = s"($x, $y, $z)"

  object Pos3:
    given Show[Pos3] = Show.fromToString

  def connections( isSample: IsSample ): Int =
    isSample match
      case IsSample.Sample => 10
      case IsSample.Real   => 1000

  def parseInput( in: Vector[String] ): Either[String, Vector[Pos3]] =
    in.traverse( line => parsers.pos3.parseAll( line ).leftMap( err => s"Parse error in $line: ${err.show}" ) )

  def distancesIterator( positions: Vector[Pos3] ): Iterator[( Long, ( Int, Int ) )] =
    for
      i <- positions.indices.iterator
      j <- 0 until i
    yield ( positions( i ).distSq( positions( j ) ), ( i, j ) )

  def rankLinks( positions: Vector[Pos3], amount: Int ): Vector[( Int, Int )] =
    distancesIterator( positions )
      .foldLeft( SortedSet.empty[( Long, ( Int, Int ) )] ): ( acc, link ) =>
        ( acc + link ).take( amount )
      .toVector
      .map( _._2 )

  def addLink( components: Vector[Set[Int]], link: ( Int, Int ) ): Vector[Set[Int]] =
    val ( i, j ) = link
    ( components.indexWhere( _( i ) ), components.indexWhere( _( j ) ) ) match
      case ( -1, -1 )         => components :+ Set( i, j )
      case ( -1, k )          => components.updated( k, components( k ) + i )
      case ( k, -1 )          => components.updated( k, components( k ) + j )
      case ( k, l ) if k == l => components
      case ( k, l )           => components.updated( k, components( k ) ++ components( l ) ).patch( l, Vector.empty, 1 )

  def getComponents( links: Vector[( Int, Int )] ): Vector[Set[Int]] =
    links.foldLeft( Vector.empty[Set[Int]] )( addLink )

  def connectAll( boxes: Vector[Pos3], links: Vector[( Int, Int )] ): Either[String, ( Pos3, Pos3 )] =
    @tailrec
    def go( components: Vector[Set[Int]], remLinks: Vector[( Int, Int )] ): Either[String, ( Pos3, Pos3 )] =
      if ( remLinks.isEmpty ) Left( "not found" )
      else
        val ( i, j )       = remLinks.head
        val nextComponents = addLink( components, remLinks.head )
//        println( s"link $i $j ${boxes( i )} ${boxes( j )}" )
//        println(
//          nextComponents
//            .map: cc =>
//              s"[${cc.size}] ${cc.toVector.map( boxes( _ ) ).mkString_( " " )}"
//            .mkString( "", "\n", "\n" )
//        )
        if ( nextComponents.length == 1 && nextComponents.head.size == boxes.length )
          Right( boxes( i ), boxes( j ) )
        else go( nextComponents, remLinks.tail )
    go( Vector.empty, links )

  override def run( in: Vector[String] ): Kleisli[Either[String, *], IsSample, Long] =
    Kleisli: isSample =>
      parseInput( in )
        .map: boxes =>
          val links      = rankLinks( boxes, connections( isSample ) )
          val components = getComponents( links )
//          println(
//            components
//              .map: cc =>
//                s"[${cc.size}] ${cc.toVector.map( boxes( _ ) ).mkString_( " " )}"
//              .mkString( "\n" )
//          )
          components.map( _.size.toLong ).sorted.takeRight( 3 ).foldLeft( 1L )( _ * _ )

  override def runBonus( in: Vector[String] ): Kleisli[Either[String, *], IsSample, Long] =
    Kleisli.liftF:
      for
        boxes <- parseInput( in )
        links = distancesIterator( boxes ).to( SortedSet ).toVector.map( _._2 )
        ( p, q ) <- connectAll( boxes, links )
//        _ = println( show"$p -> $q" )
      yield p.x.toLong * q.x.toLong

package net.chwthewke.aoc

import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all.*

opaque type DialPos = Int

object DialPos:
  def apply( i: Int ): DialPos = i
  extension ( p: DialPos )
    def ++( n: Int ): DialPos = ( ( p + n ) % 100 + 100 ) % 100
    def +( n: Int ): DialPos  = p + n

    def reduce: DialPos              = ( p % 100 + 100 ) % 100
    def zero: Boolean                = p == 0
    def turnsFrom( o: DialPos ): Int =
      if ( p > 0 ) p / 100
      else ( if ( o > 0 ) 1 else 0 ) + p.abs / 100

object Puzzle1 extends Puzzle( 1, Shell.either ):
  enum Rotation:
    case Left( amount: Int )
    case Right( amount: Int )

  object parsers:
    val rotation: Parser[Rotation] =
      (
        Parser.charIn( "LR" ),
        Numbers.nonNegativeIntString.mapFilter( _.toIntOption ).filter( _ > 0 )
      ).mapN( ( c, n ) => if ( c == 'L' ) Rotation.Left( n ) else Rotation.Right( n ) )

  def rotations( in: Vector[String] ): Either[String, Vector[Rotation]] =
    in.traverse( l => parsers.rotation.parseAll( l ) ).leftMap( err => s"parse error at ${err.input}: ${err.show}" )

  def rotate( pos: DialPos, rotation: Rotation ): DialPos =
    rotation match
      case Rotation.Left( amount )  => pos ++ ( -amount )
      case Rotation.Right( amount ) => pos ++ amount

  def timesAt0( rotations: Vector[Rotation] ): Int =
    rotations
      .foldLeft( ( DialPos( 50 ), 0 ) ):
        case ( ( pos, acc ), rot ) =>
          val nextPos = rotate( pos, rot )
          ( nextPos, if ( nextPos.zero ) acc + 1 else acc )
      ._2

  def timesThrough0( from: DialPos, rotation: Rotation ): ( DialPos, Int ) =
    val nextRaw: DialPos = rotation match
      case Rotation.Left( amount )  => from + ( -amount )
      case Rotation.Right( amount ) => from + amount
    ( nextRaw.reduce, nextRaw.turnsFrom( from ) )

  def timesThrough0( rotations: Vector[Rotation] ): Int =
    rotations
      .foldLeft( ( DialPos( 50 ), 0 ) ):
        case ( ( pos, acc ), rot ) =>
          val ( nextPos, turns ) = timesThrough0( pos, rot )
          ( nextPos, acc + turns )
      ._2

  override def run( in: Vector[String] ): Either[String, Int] =
    rotations( in ).map( timesAt0 )

  override def runBonus( in: Vector[String] ): Either[String, Int] =
    rotations( in ).map( timesThrough0 )

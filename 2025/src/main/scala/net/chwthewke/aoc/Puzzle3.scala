package net.chwthewke.aoc

import cats.Eval
import cats.data.OptionT
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all.*
import scala.annotation.tailrec

object Puzzle3 extends Puzzle( 3, Shell.either.returns[Long] ):
  object parsers:
    val joltage: Parser[Int]          = Numbers.digit.string.mapFilter( _.toIntOption )
    val joltages: Parser[Vector[Int]] = joltage.rep.map( _.toNev.toVector )

  def maxJoltageWith2Orig( joltages: Vector[Int] ): Either[String, Long] =
    ( for
      ( first, ix ) <- joltages.dropRight( 1 ).zipWithIndex.maximumByList( _._1 ).headOption
      second        <- joltages.drop( 1 + ix ).maxOption
    yield ( 10 * first + second ).toLong ).toRight( "No joltages found" )

  def toLongLE( ints: List[Int] ): Long =
    @tailrec
    def go( acc: Long, rem: List[Int] ): Long =
      rem match
        case h :: t => go( acc * 10L + h, t )
        case Nil    => acc
    go( 0L, ints.reverse )

  case class State( acc: List[Int], joltages: Vector[Int], req: Int, leastUntried: Int ):
    private def value: Long = toLongLE( acc )

    def bestCombination: OptionT[Eval, Long] =
      bestCombination( leastUntried ).orElse( copy( leastUntried = leastUntried - 1 ).bestCombination )

    def bestCombination( m: Int ): OptionT[Eval, Long] =
      OptionT:
        if ( req == 0 ) Eval.now( value.some )
        else if ( m == 0 || joltages.length < req ) Eval.now( none )
        else
          Eval
            .later:
              joltages
                .dropRight( req - 1 )
                .zipWithIndex
                .filter( _._1 <= m )
                .maximumByList( _._1 )
                .map:
                  case ( d, i ) => State( d +: acc, joltages.drop( i + 1 ), req - 1, 9 )
            .flatMap( _.collectFirstSomeM( _.bestCombination.value ) )

  def maxJoltageWith2( joltages: Vector[Int] ): Either[String, Long] =
    State( Nil, joltages, 2, 9 ).bestCombination.value.value.toRight( "not found" )

  def maxJoltageWith12( joltages: Vector[Int] ): Either[String, Long] =
    State( Nil, joltages, 12, 9 ).bestCombination.value.value.toRight( "not found" )

  override def run( in: Vector[String] ): Either[String, Long] =
    in.traverse( parsers.joltages.parseAll )
      .leftMap( err => s"Parse error ${err.show}" )
      .flatMap( _.foldMapM( maxJoltageWith2 ) )

  override def runBonus( in: Vector[String] ): Either[String, Long] =
    in.traverse( parsers.joltages.parseAll )
      .leftMap( err => s"Parse error ${err.show}" )
      .flatMap( _.foldMapM( maxJoltageWith12 ) )

package net.chwthewke.aoc

import cats.data.NonEmptyVector
import cats.parse.Numbers
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234
import cats.syntax.all.*
import scala.annotation.tailrec

object Puzzle6 extends Puzzle( 6, Shell.either.returns[Long] ):
  object parsers:
    def spaced[A]( p: Parser[A] ): Parser[Vector[A]] =
      p
        .repSep( Rfc5234.wsp.rep )
        .between( Rfc5234.wsp.rep0, Rfc5234.wsp.rep0 )
        .map( _.toList.toVector )

    val numberLine: Parser[Vector[Long]] =
      spaced(
        Numbers.nonNegativeIntString
          .mapFilter( _.toLongOption )
      )

    val op: Parser[Op] =
      Parser.oneOf:
        Op.values.toList.map: op =>
          Parser.char( op.c ).as( op )

    val opLine: Parser[Vector[Op]] = spaced( op )

    object bonus:
      val numLine: Parser[Long] =
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption ).between( Rfc5234.wsp.rep0, Rfc5234.wsp.rep0 )
      val numOpLine: Parser[( Long, Op )] = numLine ~ op
      val emptyLine: Parser0[Unit]        = Rfc5234.wsp.rep0.void

  enum Op( val c: Char ):
    case Add extends Op( '+' )
    case Mul extends Op( '*' )

  case class Problem( numbers: NonEmptyVector[Long], op: Op ):
    def compute: Long =
      val f = op match
        case Op.Add => ( x: Long, y: Long ) => x + y
        case Op.Mul => ( x: Long, y: Long ) => x * y
      numbers.reduceLeft( f )

  case class Worksheet( problems: Vector[Problem] ):
    def compute: Long = problems.foldMap( _.compute )

  object Worksheet:
    def fromRows( rows: Vector[Vector[Long]], ops: Vector[Op] ): Either[String, Worksheet] =
      for
        columns    <- Either.catchNonFatal( rows.transpose ).leftMap( _.getMessage )
        columnsNev <- columns.traverse( _.toNev.toRight( "empty col" ) )
        _          <- Option.when( columns.length == ops.length )( () ).toRight( "dims" )
      yield Worksheet(
        columnsNev
          .zip( ops )
          .map:
            case ( col, op ) => Problem( col, op )
      )

    def fromColumns( columns: Vector[String] ): Either[String, Worksheet] =
      @tailrec
      def go(
          problems: Vector[Problem],
          opLine: Option[( Long, Op )],
          acc: Vector[Long],
          remCols: Vector[String]
      ): Either[String, Worksheet] =
        def packProblem: Vector[Problem] =
          problems ++ opLine.map { case ( n, op ) => Problem( NonEmptyVector( n, acc ), op ) }
        if ( remCols.isEmpty )
          Right( Worksheet( packProblem ) )
        else if ( opLine.isEmpty )
          parsers.bonus.numOpLine.parseAll( remCols.head ) match
            case Left( err )    => Left( s"Parse error numOpLine in ${remCols.head}: ${err.show}" )
            case Right( numOp ) => go( problems, numOp.some, Vector.empty, remCols.tail )
        else if ( remCols.head.trim.isEmpty )
          go( packProblem, none, Vector.empty, remCols.tail )
        else
          parsers.bonus.numLine.parseAll( remCols.head ) match
            case Left( err )    => Left( s"Parse error numLine in ${remCols.head}: ${err.show}" )
            case Right( value ) => go( problems, opLine, acc :+ value, remCols.tail )
      go( Vector.empty, none, Vector.empty, columns )

  def onParseError( err: Parser.Error ): String =
    s"Parse error: ${err.show}"

  def parseInput( in: Vector[String] ): Either[String, Worksheet] =
    (
      in.init.traverse( l => parsers.numberLine.parseAll( l ).leftMap( onParseError ) ),
      in.lastOption.toRight( "no lines" ).flatMap( l => parsers.opLine.parseAll( l ).leftMap( onParseError ) )
    ).flatMapN( ( rows, ops ) => Worksheet.fromRows( rows, ops ) )

  override def run( in: Vector[String] ): Either[String, Long] =
    parseInput( in ).map( _.compute )

  override def runBonus( in: Vector[String] ): Either[String, Long] =
    for
      t <- Either.catchNonFatal( in.transpose.map( _.mkString ) ).leftMap( _ => "no transpose" )
      w <- Worksheet.fromColumns( t )
    yield
      //      println( t.mkString( "> ", "<\n> ", "<" ) )
//      println( w.problems.map( p => s"${p.op.c} ${p.numbers.mkString_( " " )}" ) )
      w.compute

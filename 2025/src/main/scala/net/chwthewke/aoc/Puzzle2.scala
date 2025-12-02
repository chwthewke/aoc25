package net.chwthewke.aoc

import cats.data.NonEmptyVector
import cats.parse.Numbers
import cats.parse.Parser
import cats.syntax.all.*
import scala.annotation.tailrec

object Puzzle2 extends Puzzle( 2, Shell.either.returns[Long].reads( Shell.Read.Trimmed ) ):
  case class Interval( start: Long, end: Long ):
    def sumInvalidIds: Long =
      val startText = start.toString
      val endText   = end.toString

      if ( startText.length != endText.length )
        val b = startText.map( _ => '9' ).toLong
        Interval( start, b ).sumInvalidIds + Interval( b + 1, end ).sumInvalidIds
      else
        val l = startText.length
        if ( l % 2 == 0 ) sumInvalidIds( l / 2, startText, endText )
        else 0

    def sumAllInvalidIds: Long =
      val startText = start.toString
      val endText   = end.toString

      if ( startText.length != endText.length )
        val b = startText.map( _ => '9' ).toLong
        Interval( start, b ).sumAllInvalidIds + Interval( b + 1, end ).sumAllInvalidIds
      else
        val l   = startText.length
        val byP = ( 1 until l )
          .foldLeft( ( Vector.empty[( Int, Long, Long )], 0L ) ):
            case ( ( tested, acc ), p ) =>
              val raw = sumInvalidIds( p, startText, endText )
              val adj =
                if ( l % p == 0 )
                  raw - tested.mapFilter { case ( i, r, a ) => Option.when( p % i == 0 )( a ) }.sum
                else
                  raw
              ( tested :+ ( ( p, raw, adj ) ), acc + adj )
//        println( byP._1.map { case ( i, r, a ) => s"$i -> $r $a" }.mkString( "BY_P\n", "\n", "\n" ) )
        byP._2

    private def sumInvalidIds( p: Int, startText: String, endText: String ): Long =
      if ( p == 0 || startText.length % p != 0 ) 0
      else
        // p: length of the repeated digits
        // q: number of repetitions
        val q = startText.length / p
        val d = ( "1" + "0" * p ).toLong

        // repeat some digits q times
        def repeat( x: Long ): Long =
          @tailrec
          def loop( acc: Long, i: Int ): Long =
            if ( i == 1 ) acc
            else loop( acc * d + x, i - 1 )
          loop( x, q )

        val ep = endText.take( p ).toLong
        val sp = startText.take( p ).toLong
        // (first p digits of) the greatest invalid id less than start
        val si = if ( start > repeat( sp ) ) sp else sp - 1
        // (first p digits of) the greatest invalid id less than or equal to end
        val ei = if ( end < repeat( ep ) ) ep - 1 else ep
//        println( s"""s = $start
//                    |e = $end
//                    |p = $p
//                    |q = $q
//                    |d = $d
//                    |ep = $ep
//                    |sp = $sp
//                    |si = $si
//                    |ei = $ei""".stripMargin )
        if ( ei <= si ) 0
        else
          // the invalid ids are (i * d^q + ... + i * d + i) for si < i <= ei
          // we'll sum the i and do (s * d^q + ... + i * d + s) with the sum s
          val sh = ( ei * ( ei + 1 ) - si * ( si + 1 ) ) / 2
          // = (ei² - si² + ei - si) / 2
          // = ((ei + si) * (ei - si) + ei - si) / 2
          // = ((ei + si + 1) * (ei - si)) / 2
          // ... maybe
          val r = repeat( sh )
//          println( s"""sh = $sh
//                      |r = $r
//                      |""".stripMargin )
          r

  object parsers:
    val interval: Parser[Interval] =
      (
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption ),
        Parser.char( '-' ).void,
        Numbers.nonNegativeIntString.mapFilter( _.toLongOption )
      ).mapN( ( s, _, e ) => Interval( s, e ) )

    val intervals: Parser[NonEmptyVector[Interval]] = interval.repSep( Parser.char( ',' ) ).map( _.toNev )

  override def run( in: String ): Either[String, Long] =
    parsers.intervals
      .parseAll( in )
      .map: intervals =>
        intervals.foldMap( _.sumInvalidIds )
      .leftMap( err => s"Parser error at ${err.failedAtOffset} ${err.show}" )

  override def runBonus( in: String ): Either[String, Long] =
    parsers.intervals
      .parseAll( in )
      .map: intervals =>
        intervals.foldMap( _.sumAllInvalidIds )
      .leftMap( err => s"Parser error at ${err.failedAtOffset} ${err.show}" )

package net.chwthewke.aoc

import cats.Id
import cats.Monad
import cats.Show
import cats.effect.IO
import scala.concurrent.duration._

trait Shell:
  type In
  type Out
  type Effect[_]
  type Error

  given monadInstance: Monad[Effect] = scala.compiletime.deferred

  def read: Shell.Read[In]
  def eff: Shell.Eff[Effect, Error]
  def timeout: Duration
  def hasBonusInput: Boolean
  def show: Show[Out]

  def returns[O: Show]: Shell.Aux[Effect, In, O, Error]
  def reads[I]( read: Shell.Read[I] ): Shell.Aux[Effect, I, Out, Error]
  def withTimeout( timeout: Duration ): Shell.Aux[Effect, In, Out, Error]
  def withBonusInput: Shell.Aux[Effect, In, Out, Error]

object Shell:
  class Impl[F[_], I, O, E](
      override val read: Read[I],
      override val eff: Eff[F, E],
      override val timeout: Duration,
      override val hasBonusInput: Boolean
  )( using
      val show: Show[O],
      M: Monad[F]
  ) extends Shell:
    type In     = I
    type Out    = O
    type Effect = F
    type Error  = E

    override lazy val monadInstance: Monad[F] = M

    override def returns[O1: Show]: Aux[F, I, O1, E] = new Impl( read, eff, timeout, false )

    override def reads[I1]( read1: Read[I1] ): Aux[F, I1, O, E] = new Impl( read1, eff, timeout, false )

    override def withTimeout( timeout1: Duration ): Aux[F, I, O, E] = new Impl( read, eff, timeout1, false )

    override def withBonusInput: Aux[F, I, O, E] = new Impl( read, eff, timeout, true )

  type Aux[F[_], I, O, E] = Shell { type In = I; type Out = O; type Effect = F; type Error = E }

  private val defaultTimeout: Duration = 1.minute

  private def withEffect[F[_]: Monad, E]( eff: Eff[F, E] ): Shell.Aux[F, Vector[String], Int, E] =
    new Impl( Read.Lines, eff, defaultTimeout, hasBonusInput = false )

  val default: Shell.Aux[Id, Vector[String], Int, Nothing] = withEffect( Eff.IdEff )

  val either: Shell.Aux[Either[String, *], Vector[String], Int, String] = withEffect( Eff.EitherEff )

  val io: Shell.Aux[IO, Vector[String], Int, Throwable] = withEffect( Eff.IOEff )

  sealed abstract class Eff[F[_], -E]( using val F: Monad[F] ):
    def raise[A]( e: E ): F[A]

  object Eff:
    object IdEff extends Eff[Id, Nothing]:
      def raise[A]( e: Nothing ): Id[A] = e
    object EitherEff extends Eff[Either[String, *], String]:
      override def raise[A]( e: String ): Either[String, A] = Left( e )
    object IOEff extends Eff[IO, Throwable]:
      override def raise[A]( e: Throwable ): IO[A] = IO.raiseError( e )

  sealed trait Read[I]
  object Read:
    object All     extends Read[String]
    object Lines   extends Read[Vector[String]]
    object Trimmed extends Read[String]

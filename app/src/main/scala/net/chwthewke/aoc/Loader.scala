package net.chwthewke.aoc

import cats.effect.Sync
import cats.syntax.all.*
import fs2.io.readClassLoaderResource
import fs2.text
import fs2.text.utf8

object Loader:
  enum Source( val dir: String ):
    case Samples extends Source( "samples" )
    case Inputs  extends Source( "inputs" )

  def load[F[_]: Sync]( source: Source, name: String ): F[Input] =
    readClassLoaderResource[F]( s"${source.dir}/$name" )
      .through( utf8.decode )
      .through( text.lines )
      .compile
      .toVector
      .map( Input( _ ) )

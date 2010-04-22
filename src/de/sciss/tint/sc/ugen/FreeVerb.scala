package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

/**
 *    @version 0.11, 22-Apr-10
 */
object FreeVerb extends UGen4RArgs {
  def ar( in: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f ) : GE =
    make( in, mix, room, damp )
}
// note: deterministic
case class FreeVerb( in: UGenIn, mix: UGenIn, room: UGenIn, damp: UGenIn )
extends SingleOutUGen( in, mix, room, damp ) with AudioRated

object FreeVerb2 extends UGen5RArgs {
  def ar( left: GE, right: GE, mix: GE = 0.33f, room: GE = 0.5f, damp: GE = 0.5f ) : GE =
    make( left, right, mix, room, damp )
}
// note: deterministic
case class FreeVerb2( left: UGenIn, right: UGenIn, mix: UGenIn, room: UGenIn, damp: UGenIn )
extends MultiOutUGen( audio, 2, List( left, right, mix, room, damp ))
with AudioRated


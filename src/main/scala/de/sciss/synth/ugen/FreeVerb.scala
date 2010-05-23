/*
 *  FreeVerb.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth.ugen

import de.sciss.synth._
import SynthGraph._

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


/*
 *  GVerb.scala
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

object GVerb extends UGen10RArgs {
  def ar( in: GE, roomSize: GE = 10, revTime: GE = 3, damping: GE = 0.5f, inputBW: GE = 0.5f, spread: GE = 15,
			 dryLevel: GE = 1, earlyRefLevel: GE = 0.7f, tailLevel: GE = 0.5f, maxRoomSize: GE = 300 ) : GE =
    make( in, roomSize, revTime, damping, inputBW, spread, dryLevel, earlyRefLevel, tailLevel, maxRoomSize )
}
// deterministic?
case class GVerb( in: UGenIn, roomSize: UGenIn, revTime: UGenIn, damping: UGenIn, inputBW: UGenIn, spread: UGenIn,
                  dryLevel: UGenIn, earlyRefLevel: UGenIn, tailLevel: UGenIn, maxRoomSize: UGenIn )
extends MultiOutUGen( audio, 2, List( in, roomSize, revTime, damping, inputBW, spread, dryLevel, earlyRefLevel,
                                      tailLevel, maxRoomSize ))
with AudioRated
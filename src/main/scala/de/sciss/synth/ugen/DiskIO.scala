/*
 *  DiskIO.scala
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
import SC._
import GraphBuilder._

/**
 * 	@version	0.12, 16-Apr-10
 */
object DiskOut {
  def ar( bufID: GE, multi: GE ) : GE =
    simplify( for( List( b, m @ _* ) <-
                     expand( (bufID :: multi.outputs.toList): _* ))
                yield this( b, m ))
}
case class DiskOut( bufID: UGenIn, multi: Seq[ UGenIn ])
extends SingleOutUGen( (bufID :: multi.toList): _* ) with AudioRated // with SideEffectUGen

object DiskIn {
  def ar( numChannels: Int, bufID: GE, loop: GE = 0 ) =
    simplify( for( List( b, l ) <- expand( bufID, loop ))
      yield this( numChannels, b, l ))
}
case class DiskIn( numChannels: Int, bufID: UGenIn, loop: UGenIn )
extends MultiOutUGen( audio, numChannels, List( bufID, loop )) with AudioRated // with SideEffectUGen // side-effect: advancing sf offset

object VDiskIn {
  // note: argument 'rate' renamed to 'speed'
  def ar( numChannels: Int, bufID: GE, speed: GE = 1, loop: GE = 0, sendID: GE = 0 ) =
    simplify( for( List( b, s, l, i ) <- expand( bufID, speed, loop, sendID ))
      yield this( numChannels, b, s, l, i ))
}
case class VDiskIn( numChannels: Int, bufID: UGenIn, speed: UGenIn,
                    loop: UGenIn, sendID: UGenIn )
extends MultiOutUGen( audio, numChannels, List( bufID, speed, loop, sendID ))
with AudioRated // with SideEffectUGen
 
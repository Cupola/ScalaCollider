/*
 *  DiskIO.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

/**
 * 	@version	0.11, 03-Jan-10
 */
object DiskOut {
  def ar( bufNum: GE, multi: GE ) : GE =
    simplify( for( List( b, m @ _* ) <-
                     expand( (bufNum :: multi.toUGenIns.toList): _* ))
                yield this( b, m ))
}
case class DiskOut( bufNum: UGenIn, multi: Seq[ UGenIn ])
extends SingleOutUGen( (bufNum :: multi.toList): _* ) with AudioRated

object DiskIn {
  def ar( numChannels: Int, bufNum: GE, loop: GE = 0 ) =
    simplify( for( List( b, l ) <- expand( bufNum, loop ))
      yield this( numChannels, b, l ))
}
case class DiskIn( numChannels: Int, bufNum: UGenIn, loop: UGenIn )
extends MultiOutUGen( List.fill[ Rate ]( numChannels )( audio ),
                      List( bufNum, loop )) with AudioRated

object VDiskIn {
  // note: argument 'rate' renamed to 'speed'
  def ar( numChannels: Int, bufNum: GE, speed: GE = 1, loop: GE = 0, sendID: GE = 0 ) =
    simplify( for( List( b, s, l, i ) <- expand( bufNum, speed, loop, sendID ))
      yield this( numChannels, b, s, l, i ))
}
case class VDiskIn( numChannels: Int, bufNum: UGenIn, speed: UGenIn,
                    loop: UGenIn, sendID: UGenIn )
extends MultiOutUGen( List.fill[ Rate ]( numChannels )( audio ),
                      List( bufNum, speed, loop, sendID )) with AudioRated
 
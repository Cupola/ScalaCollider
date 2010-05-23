/*
 *  PSinGrain.scala
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
 *	@version	0.10, 01-Jan-10
 */
object PSinGrain extends UGen3Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, dur: GE = 0.2f, amp: GE = 1 ) : GE = arExp( freq, dur, amp )
   def kr : GE = kr()
   def kr( freq: GE = 440, dur: GE = 0.2f, amp: GE = 1 ) : GE = krExp( freq, dur, amp )
}
case class PSinGrain( rate: Rate, freq: UGenIn, dur: UGenIn, amp: UGenIn )
extends SingleOutUGen( freq, dur, amp )

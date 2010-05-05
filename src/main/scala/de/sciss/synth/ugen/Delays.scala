/*
 *  Delays.scala
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
 * 	@version	0.12, 01-Jan-10
 */
object Delay1 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class Delay1( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object Delay2 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class Delay2( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

// ---- the following delays use real time allocated memory ----

object DelayN extends UGen3Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      arExp( in, maxDelayTime, delayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      krExp( in, maxDelayTime, delayTime )
}
case class DelayN( rate: Rate, in: UGenIn, maxDelayTime: UGenIn, delayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime )

object DelayL extends UGen3Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      arExp( in, maxDelayTime, delayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      krExp( in, maxDelayTime, delayTime )
}
case class DelayL( rate: Rate, in: UGenIn, maxDelayTime: UGenIn, delayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime )

object DelayC extends UGen3Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      arExp( in, maxDelayTime, delayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f ) : GE =
      krExp( in, maxDelayTime, delayTime )
}
case class DelayC( rate: Rate, in: UGenIn, maxDelayTime: UGenIn, delayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime )

object CombN extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class CombN( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                  delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object CombL extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class CombL( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                  delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object CombC extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class CombC( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                  delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object AllpassN extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class AllpassN( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                     delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object AllpassL extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class AllpassL( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                     delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object AllpassC extends UGen4Args {
	def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      arExp( in, maxDelayTime, delayTime, decayTime )

    def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE =
      krExp( in, maxDelayTime, delayTime, decayTime )
}
case class AllpassC( rate: Rate, in: UGenIn, maxDelayTime: UGenIn,
                     delayTime: UGenIn, decayTime: UGenIn )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

// ---- the following delays use shared buffers ----

// BufDelayN XXX
// BufDelayL XXX
// BufDelayC XXX
// BufCombN XXX
// BufCombL XXX
// BufCombC XXX
// BufAllpassN XXX
// BufAllpassL XXX
// BufAllpassC XXX

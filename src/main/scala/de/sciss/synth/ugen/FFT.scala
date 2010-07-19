/*
 *  FFT.scala
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

object FFT extends UGen6RArgs {
   def apply( buf: GE, in: GE, hop: GE = 0.5f, winType: GE = 0, active: GE = 1, winSize: GE = 0 ) : GE =
      make( buf, in, hop, winType, active, winSize )
}
case class FFT( buf: UGenIn, in: UGenIn, hop: UGenIn, winType: UGenIn, active: UGenIn, winSize: UGenIn )
extends SingleOutUGen( buf, in, hop, winType, active, winSize ) with ControlRated

object IFFT extends UGen3Args {
   def apply( buf: GE, winType: GE = 0, winSize: GE = 0 ) : GE =
      arExp( buf, winType, winSize )

   def ar( buf: GE, winType: GE = 0, winSize: GE = 0 ) : GE =
      arExp( buf, winType, winSize )

   def kr( buf: GE, winType: GE = 0, winSize: GE = 0 ) : GE =
      arExp( buf, winType, winSize )
}
case class IFFT( rate: Rate, buf: UGenIn, winType: UGenIn, winSize: UGenIn )
extends SingleOutUGen( buf, winType, winSize )

object PV_MagAbove extends UGen2RArgs {
   def apply( buf: GE, thresh: GE = 0 ) : GE = make( buf, thresh )
}
case class PV_MagAbove( buf: UGenIn, thresh: UGenIn )
extends SingleOutUGen( buf, thresh ) with ControlRated

object PV_MagBelow extends UGen2RArgs {
   def apply( buf: GE, thresh: GE = 0 ) : GE = make( buf, thresh )
}
case class PV_MagBelow( buf: UGenIn, thresh: UGenIn )
extends SingleOutUGen( buf, thresh ) with ControlRated

object PV_MagClip extends UGen2RArgs {
   def apply( buf: GE, thresh: GE = 0 ) : GE = make( buf, thresh )
}
case class PV_MagClip( buf: UGenIn, thresh: UGenIn )
extends SingleOutUGen( buf, thresh ) with ControlRated

object PV_LocalMax extends UGen2RArgs {
   def apply( buf: GE, thresh: GE = 0 ) : GE = make( buf, thresh )
}
case class PV_LocalMax( buf: UGenIn, thresh: UGenIn )
extends SingleOutUGen( buf, thresh ) with ControlRated

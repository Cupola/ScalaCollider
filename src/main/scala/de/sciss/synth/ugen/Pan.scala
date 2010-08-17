/*
 *  Pan.scala
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

import de.sciss.synth.{ audio, control, GE, MultiOutUGen, Rate, SingleOutUGen, SynthGraph, UGenIn }
import SynthGraph._

/**
 *	@version	0.11, 22-Apr-10
 */
object Pan2 extends UGen3Args {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = arExp( in, pos, level )
	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = krExp( in, pos, level )
}
case class Pan2( rate: Rate, in: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( rate, 2, List( in, pos, level ))

object LinPan2 extends UGen3Args {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = arExp( in, pos, level )
	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = krExp( in, pos, level )
}
case class LinPan2( rate: Rate, in: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( rate, 2, List( in, pos, level ))

object Pan4 extends UGen4Args {
	def ar( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE =
      arExp( in, xpos, ypos, level )

	def kr( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE =
      krExp( in, xpos, ypos, level )
}
case class Pan4( rate: Rate, in: UGenIn, xpos: UGenIn, ypos: UGenIn, level: UGenIn )
extends MultiOutUGen( rate, 4, List( in, xpos, ypos, level ))

object Balance2 extends UGen4Args {
	def ar( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE =
      arExp( left, right, pos, level )

	def kr( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE =
      krExp( left, right, pos, level )
}
case class Balance2( rate: Rate, left: UGenIn, right: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( rate, 2, List( left, right, pos, level ))

object Rotate2 extends UGen3Args {
	def ar( x: GE, y: GE, pos: GE = 0 ) : GE = arExp( x, y, pos )
	def kr( x: GE, y: GE, pos: GE = 0 ) : GE = krExp( x, y, pos )
}
case class Rotate2( rate: Rate, x: UGenIn, y: UGenIn, pos: UGenIn )
extends MultiOutUGen( rate, 2, List( x, y, pos ))

// XXX PanB missing
// XXX PanB2 missing
// XXX BiPanB2 missing
// XXX DecodeB2 missing

object PanAz {
  private def make( rate: Rate, numChannels: Int, in: GE, pos: GE, level: GE,
                    width: GE, orient: GE ) : GE =
    simplify( for( List( i, p, l, w, o ) <-
                  expand( in, pos, level, width, orient ))
      yield this( rate, numChannels, i, p, l, w, o ))

	def ar( numChannels: Int, in: GE, pos: GE = 0, level: GE = 1, width: GE = 2,
            orient: GE = 0.5f ) : GE =
      make( audio, numChannels, in, pos, level, width, orient )

	def kr( numChannels: Int, in: GE, pos: GE = 0, level: GE = 1, width: GE = 2,
            orient: GE = 0.5f ) : GE =
      make( control, numChannels, in, pos, level, width, orient )
}
case class PanAz( rate: Rate, numChannels: Int, in: UGenIn, pos: UGenIn,
                  level: UGenIn, width: UGenIn, orient: UGenIn )
extends MultiOutUGen( rate, numChannels, List( in, pos, level, width, orient ))

object XFade2 extends UGen4Args {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      arExp( inA, inB, pan, level )

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      krExp( inA, inB, pan, level )
}
/**
 * An equal power two channel cross fading UGen.
 *
 * @param   pan   the xfade position from `-1` (only input A audible) to
 *    `+1` (only input B audible)
 *
 * @see  [[de.sciss.synth.ugen.LinXFade2]]
 */
case class XFade2( rate: Rate, inA: UGenIn, inB: UGenIn, pan: UGenIn, level: UGenIn )
extends SingleOutUGen( inA, inB, pan, level )

object LinXFade2 extends UGen4Args {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      arExp( inA, inB, pan, level )

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      krExp( inA, inB, pan, level )
}
/**
 * An linear two channel cross fading UGen.
 *
 * @param   pan   the xfade position from `-1` (only input A audible) to
 *    `+1` (only input B audible)
 *
 * @see  [[de.sciss.synth.ugen.XFade2]]
 */
case class LinXFade2( rate: Rate, inA: UGenIn, inB: UGenIn, pan: UGenIn, level: UGenIn )
extends SingleOutUGen( inA, inB, pan, level )

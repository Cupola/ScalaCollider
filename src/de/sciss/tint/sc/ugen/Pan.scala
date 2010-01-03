/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

/**
 *	@version	0.11, 01-Jan-10
 */
object Pan2 extends UGen3Args {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = arExp( in, pos, level )
	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = krExp( in, pos, level )
}
case class Pan2( rate: Rate, in: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( List( rate, rate ), List( in, pos, level ))

object LinPan2 extends UGen3Args {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = arExp( in, pos, level )
	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = krExp( in, pos, level )
}
case class LinPan2( rate: Rate, in: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( List( rate, rate ), List( in, pos, level ))

object Pan4 extends UGen4Args {
	def ar( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE =
      arExp( in, xpos, ypos, level )

	def kr( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE =
      krExp( in, xpos, ypos, level )
}
case class Pan4( rate: Rate, in: UGenIn, xpos: UGenIn, ypos: UGenIn, level: UGenIn )
extends MultiOutUGen( List( rate, rate, rate, rate ), List( in, xpos, ypos, level ))

object Balance2 extends UGen4Args {
	def ar( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE =
      arExp( left, right, pos, level )

	def kr( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE =
      krExp( left, right, pos, level )
}
case class Balance2( rate: Rate, left: UGenIn, right: UGenIn, pos: UGenIn, level: UGenIn )
extends MultiOutUGen( List( rate, rate ), List( left, right, pos, level ))

object Rotate2 extends UGen3Args {
	def ar( x: GE, y: GE, pos: GE = 0 ) : GE = arExp( x, y, pos )
	def kr( x: GE, y: GE, pos: GE = 0 ) : GE = krExp( x, y, pos )
}
case class Rotate2( rate: Rate, x: UGenIn, y: UGenIn, pos: UGenIn )
extends MultiOutUGen( List( rate, rate ), List( x, y, pos ))

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
extends MultiOutUGen( List.fill[ Rate ]( numChannels )( rate ),
                      List( in, pos, level, width, orient ))

object XFade2 extends UGen4Args {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      arExp( inA, inB, pan, level )

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      krExp( inA, inB, pan, level )
}
case class XFade2( rate: Rate, inA: UGenIn, inB: UGenIn, pan: UGenIn, level: UGenIn )
extends SingleOutUGen( inA, inB, pan, level )

object LinXFade2 extends UGen4Args {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      arExp( inA, inB, pan, level )

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE =
      krExp( inA, inB, pan, level )
}
case class LinXFade2( rate: Rate, inA: UGenIn, inB: UGenIn, pan: UGenIn, level: UGenIn )
extends SingleOutUGen( inA, inB, pan, level )

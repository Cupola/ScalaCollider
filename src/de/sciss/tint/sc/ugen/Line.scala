/*
 *  Line.scala
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
//import Rates._

/**
 * 	@version	0.12, 01-Jan-10
 */
object Line extends UGen4Args {
	def ar( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0 ) : GE =
      arExp( start, end, dur, doneAction )

	def kr( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0 ) : GE =
      krExp( start, end, dur, doneAction )
}
case class Line( rate: Rate, start: UGenIn, end: UGenIn, dur: UGenIn, doneAction: UGenIn )
extends SingleOutUGen( start, end, dur, doneAction )

object XLine extends UGen4Args {
	def ar( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0 ) : GE =
      arExp( start, end, dur, doneAction )

	def kr( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0 ) : GE =
      krExp( start, end, dur, doneAction )
}
case class XLine( rate: Rate, start: UGenIn, end: UGenIn, dur: UGenIn, doneAction: UGenIn )
extends SingleOutUGen( start, end, dur, doneAction )

object LinExp extends UGen5Args {
	def ar( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE =
      arExp( in, srcLo, srcHi, dstLo, dstHi )

	def kr( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE =
      krExp( in, srcLo, srcHi, dstLo, dstHi )
}
case class LinExp( rate: Rate, in: UGenIn, srcLo: UGenIn, srcHi: UGenIn, dstLo: UGenIn, dstHi: UGenIn )
extends SingleOutUGen( in, srcLo, srcHi, dstLo, dstHi )

object LinLin extends UGen5Args {
	def ar( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE =
      arExp( in, srcLo, srcHi, dstLo, dstHi )

	def kr( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE =
      krExp( in, srcLo, srcHi, dstLo, dstHi )
}
case class LinLin( rate: Rate, in: UGenIn, srcLo: UGenIn, srcHi: UGenIn, dstLo: UGenIn, dstHi: UGenIn )
extends SingleOutUGen( in, srcLo, srcHi, dstLo, dstHi )

object AmpComp extends UGen3Args {
	def ar( freq: GE = midicps( 60 ), root: GE = midicps( 60 ), expon: GE = 0.3333 ) : GE =
      arExp( freq, root, expon )

	def kr( freq: GE = midicps( 60 ), root: GE = midicps( 60 ), expon: GE = 0.3333 ) : GE =
      krExp( freq, root, expon )

	def ir( freq: GE = midicps( 60 ), root: GE = midicps( 60 ), expon: GE = 0.3333 ) : GE =
      irExp( freq, root, expon )
// XXX checkInputs
}
case class AmpComp( rate: Rate, freq: UGenIn, root: UGenIn, expon: UGenIn )
extends SingleOutUGen( freq, root, expon )

object AmpCompA extends UGen4Args {
	def ar( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE =
      arExp( freq, root, minAmp, rootAmp )

	def kr( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE =
      krExp( freq, root, minAmp, rootAmp )

	def ir( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE =
      irExp( freq, root, minAmp, rootAmp )
}
case class AmpCompA( rate: Rate, freq: UGenIn, root: UGenIn, minAmp: UGenIn, maxAmp: UGenIn )
extends SingleOutUGen( freq, root, minAmp, maxAmp )

object K2A extends UGen1Args {
  def ar( in: GE ) : GE = arExp( in )
}
case class K2A( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object A2K extends UGen1Args {
  def kr( in: GE ) : GE = krExp( in )
}
case class A2K( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object T2K extends UGen1Args {
  def kr( in: GE ) : GE = krExp( in )
}
case class T2K( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object T2A extends UGen1Args {
  def ar( in: GE ) : GE = arExp( in )
}
case class T2A( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object DC {
	def ar( in: GE ) : GE = this( audio, in.toUGenIns: _* )
	def kr( in: GE ) : GE = this( control, in.toUGenIns: _* )
}
case class DC( rate: Rate, in: UGenIn* )
extends MultiOutUGen( in.map( _ => audio ), in )

object Silent {
	def ar( numChannels: Int ) : GE = this( numChannels )
}
case class Silent( numChannels: Int )
extends MultiOutUGen( List.fill( numChannels )( audio ), Nil ) { val rate = audio }
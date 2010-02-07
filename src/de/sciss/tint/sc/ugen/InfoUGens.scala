/*
 *  InfoUGens.scala
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

/**
 *	@version	0.12, 07-Feb-10
 */
abstract class InfoUGen extends SingleOutUGen() with ScalarRated

object SampleRate {
	def ir: GE = this()
}
case class SampleRate() extends InfoUGen

object SampleDur {
	def ir: GE = this()
}
case class SampleDur() extends InfoUGen

object RadiansPerSample {
	def ir: GE = this()
}
case class RadiansPerSample() extends InfoUGen

object ControlRate {
	def ir: GE = this()
}
case class ControlRate() extends InfoUGen

object ControlDur {
	def ir: GE = this()
}
case class ControlDur() extends InfoUGen

object SubsampleOffset {
	def ir: GE = this()
}
case class SubsampleOffset() extends InfoUGen

object NumOutputBuses {
	def ir: GE = this()
}
case class NumOutputBuses() extends InfoUGen

object NumInputBuses {
	def ir: GE = this()
}
case class NumInputBuses() extends InfoUGen

object NumAudioBuses {
	def ir: GE = this()
}
case class NumAudioBuses() extends InfoUGen

object NumControlBuses {
	def ir: GE = this()
}
case class NumControlBuses() extends InfoUGen

object NumBuffers {
	def ir: GE = this()
}
case class NumBuffers() extends InfoUGen

object NumRunningSynths {
	def ir: GE = this()
}
case class NumRunningSynths() extends InfoUGen

trait BufInfoUGenBase extends UGen1Args {
   def ir( bufNum: GE ) : GE = irExp( bufNum )
   def kr( bufNum: GE ) : GE = krExp( bufNum )
}

object BufSampleRate extends BufInfoUGenBase
case class BufSampleRate( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

object BufRateScale extends BufInfoUGenBase
case class BufRateScale( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

object BufFrames extends BufInfoUGenBase
case class BufFrames( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

object BufSamples extends BufInfoUGenBase
case class BufSamples( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

object BufDur extends BufInfoUGenBase
case class BufDur( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

object BufChannels extends BufInfoUGenBase
case class BufChannels( rate: Rate, bufNum: UGenIn ) extends SingleOutUGen( bufNum )

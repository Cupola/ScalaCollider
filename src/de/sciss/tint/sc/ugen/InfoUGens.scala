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

//import Rates._

// InfoUGenBase : UGen {
//	*ir {
//		^this.multiNew(scalar')
//	}
// }

/**
 *	@version	0.11, 31-Dec-09
 */
abstract class InfoUGenBase extends SingleOutUGen() { val rate = scalar }

object SampleRate {
	def ir: GE = this()
}
case class SampleRate() extends InfoUGenBase

object SampleDur {
	def ir: GE = this()
}
case class SampleDur() extends InfoUGenBase

object RadiansPerSample {
	def ir: GE = this()
}
case class RadiansPerSample() extends InfoUGenBase

object ControlRate {
	def ir: GE = this()
}
case class ControlRate() extends InfoUGenBase

object ControlDur {
	def ir: GE = this()
}
case class ControlDur() extends InfoUGenBase

object SubsampleOffset {
	def ir: GE = this()
}
case class SubsampleOffset() extends InfoUGenBase

object NumOutputBuses {
	def ir: GE = this()
}
case class NumOutputBuses() extends InfoUGenBase

object NumInputBuses {
	def ir: GE = this()
}
case class NumInputBuses() extends InfoUGenBase

object NumAudioBuses {
	def ir: GE = this()
}
case class NumAudioBuses() extends InfoUGenBase

object NumControlBuses {
	def ir: GE = this()
}
case class NumControlBuses() extends InfoUGenBase

object NumBuffers {	/* extends InfoUGenBase */
	def ir: GE = this()
}
case class NumBuffers() extends InfoUGenBase

object NumRunningSynths {	/* extends InfoUGenBase */
	def ir: GE = this()
}
case class NumRunningSynths() extends InfoUGenBase


/*
BufInfoUGenBase : UGen {
	*kr { arg bufnum;
		^this.multiNew(control', bufnum)
	}
	
	// the .ir method is not the safest choice. Since a buffer can be reallocated at any time,
	// using .ir will not track the changes.
	*ir { arg bufnum;
		^this.multiNew(scalar',bufnum)
	}
}
*/

object BufSampleRate {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufSampleRate", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufSampleRate", control, List( control ), List( bufNum ))
	}
}

object BufRateScale {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufRateScale", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufRateScale", control, List( control ), List( bufNum ))
	}
}

object BufFrames {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufFrames", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufFrames", control, List( control ), List( bufNum ))
	}
}

object BufSamples {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufSamples", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufSamples", control, List( control ), List( bufNum ))
	}
}

object BufDur {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufDur", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufDur", control, List( control ), List( bufNum ))
	}
}

object BufChannels {	/* extends BufInfoUGenBase */
	def ir( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufChannels", scalar, List( scalar ), List( bufNum ))
	}

	def kr( bufNum: GE ) : GE = {
	  UGen.multiNew( "BufChannels", control, List( control ), List( bufNum ))
	}
}

/*
 *  InfoUGens.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.tint.sc

import Rates._

// InfoUGenBase : UGen {
//	*ir {
//		^this.multiNew(scalar')
//	}
// }

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.10, 15-Jun-08
 */
object SampleRate {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "SampleRate", scalar, scalar, Nil )
	}
}

object SampleDur {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "SampleDur", scalar, scalar, Nil )
	}
}

object RadiansPerSample {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "RadiansPerSample", scalar, scalar, Nil )
	}
}

object ControlRate {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "ControlRate", scalar, scalar, Nil )
	}
}

object SubsampleOffset {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "SubsampleOffset", scalar, scalar, Nil )
	}
}

object NumOutputBuses {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "NumOutputBuses", scalar, scalar, Nil )
	}
}

object NumInputBuses {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "NumInputBuses", scalar, scalar, Nil )
	}
}

object NumAudioBuses {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "NumAudioBuses", scalar, scalar, Nil )
	}
}

object NumControlBuses {	/* extends InfoUGenBase */
	def ir : UGenInput = {
      new SingleOutUGen( "NumControlBuses", scalar, scalar, Nil )
	}
}

object NumBuffers {	/* extends InfoUGenBase */
	def ir : UGenInput = {
	  new SingleOutUGen( "NumBuffers", scalar, scalar, Nil )
	}
}

object NumRunningSynths {	/* extends InfoUGenBase */
	def kr : UGenInput = {
	  new SingleOutUGen( "NumRunningSynths", control, control, Nil )
	}
}


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

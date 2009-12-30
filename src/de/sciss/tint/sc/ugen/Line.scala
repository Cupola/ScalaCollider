/*
 *  Line.scala
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
package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
//import Rates._

/**
 * 	@version	0.11, 09-Dec-09
 */
object Line {
//	def ar( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0, mul: GE = 1, add: GE = 0 ) : GE = {
//		ar( start, end, dur, doneAction ).madd( mul, add )
//	}

	def ar( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0 ) : GE = {
    	UGen.multiNew( "Line", audio, List( audio ), List( start, end, dur, doneAction ))
	}

//	def kr( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0, mul: GE = 1, add: GE = 0 ) : GE = {
//		kr( start, end, dur, doneAction ).madd( mul, add )
//	}

	def kr( start: GE = 0, end: GE = 1, dur: GE = 1, doneAction: GE = 0 ) : GE = {
    	UGen.multiNew( "Line", control, List( control ), List( start, end, dur, doneAction ))
	}
}

object XLine {
//	def ar( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0, mul: GE = 1, add: GE = 0 ) : GE = {
//		ar( start, end, dur, doneAction ).madd( mul, add )
//	}

	def ar( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0 ) : GE = {
    	UGen.multiNew( "XLine", audio, List( audio ), List( start, end, dur, doneAction ))
	}

//	def kr( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0, mul: GE = 1, add: GE = 0 ) : GE = {
//		kr( start, end, dur, doneAction ).madd( mul, add )
//	}

	def kr( start: GE = 1, end: GE = 2, dur: GE = 1, doneAction: GE = 0 ) : GE = {
    	UGen.multiNew( "XLine", control, List( control ), List( start, end, dur, doneAction ))
	}
}

object LinExp {
	def ar( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE = {
    	UGen.multiNew( "LinExp", audio, List( audio ), List( in, srcLo, srcHi, dstLo, dstHi ))
	}

	def kr( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE = {
    	UGen.multiNew( "LinExp", control, List( control ), List( in, srcLo, srcHi, dstLo, dstHi ))
	}
}

object LinLin {
	def ar( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE = {
    	UGen.multiNew( "LinLin", audio, List( audio ), List( in, srcLo, srcHi, dstLo, dstHi ))
	}

	def kr( in: GE, srcLo: GE = 0, srcHi: GE = 1, dstLo: GE = 1, dstHi: GE = 2 ) : GE = {
    	UGen.multiNew( "LinLin", control, List( control ), List( in, srcLo, srcHi, dstLo, dstHi ))
	}
}

object AmpComp {
	def ar( freq: GE = 261.6255653006, root: GE = 261.6255653006, exp: GE = 0.3333 ) : GE = {
    	UGen.multiNew( "AmpComp", audio, List( audio ), List( freq, root, exp ))
	}

	def kr( freq: GE = 261.6255653006, root: GE = 261.6255653006, exp: GE = 0.3333 ) : GE = {
    	UGen.multiNew( "AmpComp", control, List( control ), List( freq, root, exp ))
	}

	def ir( freq: GE = 261.6255653006, root: GE = 261.6255653006, exp: GE = 0.3333 ) : GE = {
    	UGen.multiNew( "AmpComp", scalar, List( scalar ), List( freq, root, exp ))
	}
}

object AmpCompA {
	def ar( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE = {
    	UGen.multiNew( "AmpCompA", audio, List( audio ), List( freq, root, minAmp, rootAmp ))
	}

	def kr( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE = {
    	UGen.multiNew( "AmpCompA", control, List( control ), List( freq, root, minAmp, rootAmp ))
	}

	def ir( freq: GE = 1000, root: GE = 0, minAmp: GE = 0.32, rootAmp: GE = 1 ) : GE = {
    	UGen.multiNew( "AmpCompA", scalar, List( scalar ), List( freq, root, minAmp, rootAmp ))
	}
}

object K2A {
  def ar( in: GE ) : GE = {
    UGen.multiNew( "K2A", audio, List( audio ), List( in ))
  }
}

object A2K {
  def kr( in: GE ) : GE = {
    UGen.multiNew( "A2K", control, List( control ), List( in ))
  }
}

object T2K {
  def kr( in: GE ) : GE = {
    UGen.multiNew( "T2K", control, List( control ), List( in ))
  }
}

object T2A {
  def ar( in: GE ) : GE = {
    UGen.multiNew( "T2A", audio, List( audio ), List( in ))
  }
}

// DC missing

object Silent {
	def ar( numChannels: Int ) : GE = {
	  new MultiOutUGen( "Silent", audio, dup( audio, numChannels ), Nil );
	}
}

/*
 *  FSinOsc.scala
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

import Predef._
import Rates._

/**
 *	@version	0.11, 09-Dec-09
 */
object FSinOsc {	
	def ar( freq: GE = 440, iphase: GE = 0, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "FSinOsc", audio, List( audio ), List( freq, iphase )).madd( mul, add )
	}

	def kr( freq: GE = 440, iphase: GE = 0, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "FSinOsc", control, List( control ), List( freq, iphase )).madd( mul, add )
	}
}

// Klang XXX missing
// Klank XXX missing
// DynKlank XXX missing
// DynKlang XXX missing

object Blip {
	def ar( freq: GE = 440, numHarm: GE = 200, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Blip", audio, List( audio ), List( freq, numHarm )).madd( mul, add )
	}

	def kr( freq: GE = 440, numHarm: GE = 200, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Blip", control, List( control ), List( freq, numHarm )).madd( mul, add )
	}
}

object Saw {
	def ar( freq: GE = 440, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Saw", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 440, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Saw", control, List( control ), List( freq )).madd( mul, add )
	}
}

object Pulse {
	def ar( freq: GE = 440, width: GE = 0.5f, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Pulse", audio, List( audio ), List( freq, width )).madd( mul, add )
	}

	def kr( freq: GE = 440, width: GE = 0.5f, mul: GE = 1, add: GE = 1 ) : GE = {
		UGen.multiNew( "Pulse", control, List( control ), List( freq, width )).madd( mul, add )
	}
}
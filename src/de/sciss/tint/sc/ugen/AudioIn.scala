/*
 *  AudioIn.scala
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

/**
 *	@version	0.11, 16-Jun-09
 */
object SoundIn {
//	( mul: GE = 1, add: GE = 0 ) : GE = ar( bus ).madd( mul, add )
//	
	def ar( bus: GE = 0 ) : GE = {
		val chanOffset = NumOutputBuses.ir
		if( bus.numOutputs == 1 ) {
			return( In.ar( chanOffset + bus, 1 )) // .madd( mul, add )
		}
null
/* XXX
		// check to see if channels array is consecutive [n,n+1,n+2...]
		if( bus.every({arg item, i; 
				(i==0) or: {item == (bus.at(i-1)+1)}
			}),{
			^In.ar(chanOffset + bus.first, bus.size).madd(mul,add)
		},{
			// allow In to multi channel expand
			^In.ar(chanOffset + bus).madd(mul,add)
		})
 */
	}
}
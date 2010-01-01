/*
 *  Mix.scala
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

/**
 *	@version	0.10, 09-Dec-09
 */
object Mix {
	def apply( array: GE ) : GE = {
		val inputs = array.toUGenIns
		if( inputs.size == 0 ) {
			GESeq()
		} else if( inputs.size == 1 ) {
			array
		} else {
			var i = 0
			var sum: GE = 0
			inputs.foreach( inp => {
				if( i == 0 ) sum = inp else sum += inp
				i = i + 1
			})
			sum
		}
	}

	// support this common idiom
	def fill( n: Int, func: (Int) => GE ) : GE = {
		var sum: GE = 0
		for( i <- Range( 0, n )) {
			val res = func.apply( i )
			if( i == 0 ) sum = res else sum = sum + res
		}
		sum
	}
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc

import Predef._

/**
 *	@version	0.10, 09-Dec-09
 */
object Mix {
	def apply( array: GE ) : GE = {
		val inputs = array.toUGenInputs
		if( inputs.size == 0 ) {
			GESeq( Nil )
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

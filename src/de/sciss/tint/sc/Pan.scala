/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc

import Predef._
import Rates._

/**
 *	@version	0.10, 09-Dec-09
 */
object Pan2 {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Pan2", audio, List( audio, audio ), List( in, pos, level ))
	}

	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Pan2", control, List( control, control ), List( in, pos, level ))
	}
}

object LinPan2 {
	def ar( in: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "LinPan2", audio, List( audio, audio ), List( in, pos, level ))
	}

	def kr( in: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "LinPan2", control, List( control, control ), List( in, pos, level ))
	}
}

object Pan4 {
	def ar( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Pan4", audio, List( audio, audio, audio, audio ), List( in, xpos, ypos, level ))
	}

	def kr( in: GE, xpos: GE = 0, ypos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Pan4", control, List( control, control, control, control ), List( in, xpos, ypos, level ))
	}
}

object Balance2 {
	def ar( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Balance2", audio, List( audio, audio ), List( left, right, pos, level ))
	}

	def kr( left: GE, right: GE, pos: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "Balance2", control, List( control, control ), List( left, right, pos, level ))
	}
}

object Rotate2 {
	def ar( x: GE, y: GE, pos: GE = 0 ) : GE = {
		  UGen.multiNew( "Rotate2", audio, List( audio, audio ), List( x, y, pos ))
	}

	def kr( x: GE, y: GE, pos: GE = 0 ) : GE = {
		  UGen.multiNew( "Rotate2", control, List( control, control ), List( x, y, pos ))
	}
}

// XXX PanB missing
// XXX PanB2 missing
// XXX BiPanB2 missing
// XXX DecodeB2 missing

object PanAz {
	def ar( numChans: Int, in: GE, pos: GE = 0, level: GE = 1, width: GE = 2, orientation: GE = 0.5f ) : GE = {
		  UGen.multiNew( "PanAz", audio, dup( audio, numChans ), List( in, pos, level, width, orientation ))
	}

	def kr( numChans: Int, in: GE, pos: GE = 0, level: GE = 1, width: GE = 2, orientation: GE = 0.5f ) : GE = {
		  UGen.multiNew( "PanAz", control, dup( control, numChans ), List( in, pos, level, width, orientation ))
	}
}

object XFade2 {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "XFade2", audio, List( audio ), List( inA, inB, pan, level ))
	}

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "XFade2", control, List( control ), List( inA, inB, pan, level ))
	}
}

object LinXFade2 {
	def ar( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "LinXFade2", audio, List( audio ), List( inA, inB, pan, level ))
	}

	def kr( inA: GE, inB: GE = 0, pan: GE = 0, level: GE = 1 ) : GE = {
		  UGen.multiNew( "LinXFade2", control, List( control ), List( inA, inB, pan, level ))
	}
}

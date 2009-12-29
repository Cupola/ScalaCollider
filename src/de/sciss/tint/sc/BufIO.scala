/*
 *  BufIO.scala
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
import _root_.scala.Predef.error
import Rates._

/**
 * 	@version	0.11, 16-Jun-09
 */
object PlayBuf {
	def ar( numChannels: Int, bufNum: GE = 0, rate: GE = 1, trigger: GE = 1, startPos: GE = 0, loop: GE = 1, doneAction: GE = 0 ) : GE = {
		UGen.multiNew( "PlayBuf", audio, dup( audio, numChannels ), List( bufNum, rate, trigger, startPos, loop, doneAction ))
	}

	def kr( numChannels: Int, bufNum: GE = 0, rate: GE = 1, trigger: GE = 1, startPos: GE = 0, loop: GE = 1, doneAction: GE = 0 ) : GE = {
		UGen.multiNew( "PlayBuf", control, dup( control, numChannels ), List( bufNum, rate, trigger, startPos, loop, doneAction ))
	}
}

object TGrains {
	def ar( numChannels: Int, trigger: GE = 0, bufNum: GE = 0, rate: GE = 1, centerPos: GE = 0, dur: GE = 0.1f, pan: GE = 0, amp: GE = 0.1f, interp: GE = 4 ) : GE = {
		if( numChannels < 2 ) {
			 error( "TGrains needs at least two channels." )
		}
		UGen.multiNew( "TGrains", audio, dup( audio, numChannels ),
			List( trigger, bufNum, rate, centerPos, dur, pan, amp, interp ))
	}
}

object BufRd {
	def ar( numChannels: Int, bufNum: GE = 0, phase: GE = 0, loop: GE = 1, interpolation: GE = 2 ) : GE = {
		UGen.multiNew( "BufRd", audio, dup( audio, numChannels ), List( bufNum, phase, loop, interpolation ))
	}

	def kr( numChannels: Int, bufNum: GE = 0, phase: GE = 0, loop: GE = 1, interpolation: GE = 2 ) : GE = {
		UGen.multiNew( "BufRd", control, dup( control, numChannels ), List( bufNum, phase, loop, interpolation ))
	}
}

object BufWr {
	def ar( inputArray: GE, bufNum: GE = 0, phase: GE = 0, loop: GE = 1 ) : GE = {
		UGen.multiNew( "BufWr", audio, List( audio ), List( bufNum, phase, loop ) ++ inputArray.toUGenInputs )
	}

	def kr( inputArray: GE, bufNum: GE = 0, phase: GE = 0, loop: GE = 1 ) : GE = {
		UGen.multiNew( "BufWr", control, List( control ), List( bufNum, phase, loop ) ++ inputArray.toUGenInputs )
	}

//	checkInputs {
// 		if (rate == audio' and: {inputs.at(1).rate != audio'}, {
// 			^("phase input is not audio rate: " + inputs.at(1) + inputs.at(1).rate);
// 		});
// 		^this.checkValidInputs
// 	}
}

object RecordBuf {
	def ar( inputArray: GE, bufNum: GE = 0, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trigger: GE = 1, doneAction: GE = 0 ) : GE = {
		UGen.multiNew( "RecordBuf", audio, List( audio ),
			List( bufNum, offset, recLevel, preLevel, run, loop, trigger, doneAction ) ++
				inputArray.toUGenInputs )
	}

	def kr( inputArray: GE, bufNum: GE = 0, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trigger: GE = 1, doneAction: GE = 0 ) : GE = {
		UGen.multiNew( "RecordBuf", control, List( control ),
			List( bufNum, offset, recLevel, preLevel, run, loop, trigger, doneAction ) ++
				inputArray.toUGenInputs )
	}
}

object Tap {
	// Warning: different arg order than sclang!
	def ar( numChannels: Int = 1, bufNum: GE = 0, delayTime: GE = 0.2f ) : GE = {
		val n = delayTime * SampleRate.ir.neg; // this depends on the session sample rate, not buffer.
		PlayBuf.ar( numChannels, bufNum, 1, 0, n, 1 )
	}
}

// ScopeOut
// LocalBuf
// MaxLocalBufs
// SetBuf
// ClearBuf

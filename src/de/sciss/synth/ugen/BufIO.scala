/*
 *  BufIO.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth.ugen

import de.sciss.synth._
import SC._
import GraphBuilder._

/**
 * 	@version	0.13, 16-Apr-10
 */
object PlayBuf {
   // note: argument 'rate' renamed to 'speed'
   def ar( numChannels: Int, bufNum: GE, speed: GE = 1, trig: GE = 1, startPos: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      make( audio, numChannels, bufNum, speed, trig, startPos, loop, doneAction )

   def kr( numChannels: Int, bufNum: GE, speed: GE = 1, trig: GE = 1, startPos: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      make( control, numChannels, bufNum, speed, trig, startPos, loop, doneAction )

   private def make( rate: Rate, numChannels: Int, bufNum: GE, speed: GE, trig: GE, startPos: GE, loop: GE,
                       doneAction: GE ) =
      simplify( for( List( b, s, t, p, l, d ) <- expand( bufNum, speed, trig, startPos, loop, doneAction ))
         yield this( rate, numChannels, b, s, t, p, l, d ))
}
case class PlayBuf( rate: Rate, numChannels: Int, bufNum: UGenIn, speed: UGenIn, trig: UGenIn, startPos: UGenIn,
                    loop: UGenIn, doneAction: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufNum, speed, trig, startPos, loop, doneAction ))
// with SideEffectUGen // side-effect: done flag

object TGrains {
   // note: argument 'rate' renamed to 'speed'
   def ar( numChannels: Int, trig: GE, bufNum: GE, speed: GE = 1, centerPos: GE = 0, dur: GE = 0.1f, pan: GE = 0,
           amp: GE = 0.1f, interp: GE = 4 ) : GE =
      simplify( for( List( b, s, c, d, p, a, i ) <- expand( bufNum, speed, centerPos, dur, pan, amp, interp ))
         yield this( audio, numChannels, b, s, c, d, p, a, i ))
}
case class TGrains( rate: Rate, numChannels: Int, bufNum: UGenIn, speed: UGenIn, centerPos: UGenIn, dur: UGenIn,
                    pan: UGenIn, amp: UGenIn, interp: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufNum, speed, centerPos, dur, pan, amp, interp ))

object BufRd {
   def ar( numChannels: Int, bufNum: GE, phase: GE = 0, loop: GE = 1, interp: GE = 2 ) : GE =
      make( audio, numChannels, bufNum, phase, loop, interp )

   def kr( numChannels: Int, bufNum: GE, phase: GE = 0, loop: GE = 1, interp: GE = 2 ) : GE =
      make( control, numChannels, bufNum, phase, loop, interp )

   private def make( rate: Rate, numChannels: Int, bufNum: GE, phase: GE, loop: GE, interp: GE ) =
      simplify( for( List( b, p, l, i ) <- expand( bufNum, phase, loop, interp ))
         yield this( rate, numChannels, b, p, l, i ))
}
case class BufRd( rate: Rate, numChannels: Int, bufNum: UGenIn, phase: UGenIn, loop: UGenIn, interp: UGenIn )
//class BufRd( val rate: Rate, val numChannels: Int, val bufNum: UGenIn, val phase: UGenIn, val loop: UGenIn,
//             val interp: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufNum, phase, loop, interp ))
// with SideEffectUGen // side-effect: done-flag

object BufWr {
   def ar( multi: GE, bufNum: GE, phase: GE = 0, loop: GE = 1 ) : GE =
      make( audio, multi, bufNum, phase, loop )

   def kr( multi: GE, bufNum: GE, phase: GE = 0, loop: GE = 1 ) : GE =
      make( control, multi, bufNum, phase, loop )

   private def make( rate: Rate, multi: GE, bufNum: GE, phase: GE, loop: GE ) =
      simplify( for( List( b, p, l, m @ _* ) <- expand( (bufNum :: phase :: loop :: multi.outputs.toList) :_* ))
         yield this( rate, b, p, l, m ))

//	checkInputs {
// 		if (rate == audio' and: {inputs.at(1).rate != audio'}, {
// 			^("phase input is not audio rate: " + inputs.at(1) + inputs.at(1).rate);
// 		});
// 		^this.checkValidInputs
// 	}
}
case class BufWr( rate: Rate, bufNum: UGenIn, phase: UGenIn, loop: UGenIn, multi: Seq[ UGenIn ])
extends SingleOutUGen( (bufNum :: phase :: loop :: multi.toList): _* ) // with SideEffectUGen

object RecordBuf {
   def ar( multi: GE, bufNum: GE, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing ) : GE =
      make( audio, multi, bufNum, offset, recLevel, preLevel, run, loop, trig, doneAction )

   def kr( multi: GE, bufNum: GE, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing ) : GE =
      make( control, multi, bufNum, offset, recLevel, preLevel, run, loop, trig, doneAction )

   private def make( rate: Rate, multi: GE, bufNum: GE, offset: GE, recLevel: GE, preLevel: GE, run: GE,
                       loop: GE, trig: GE, doneAction: GE ) =
      simplify( for( List( b, o, r, p, n, l, t, d, m @ _* ) <- expand( (bufNum :: offset :: recLevel :: preLevel ::
                     run :: loop :: trig :: doneAction :: multi.outputs.toList) :_* ))
         yield this( rate, b, o, r, p, n, l, t, d, m, SynthDef.individuate ))
}
case class RecordBuf( rate: Rate, bufNum: UGenIn, offset: UGenIn, recLevel: UGenIn, preLevel: UGenIn,
                      run: UGenIn, loop: UGenIn, trig: UGenIn, doneAction: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (bufNum :: offset :: recLevel :: preLevel :: run :: loop :: trig :: doneAction :: multi.toList): _* )
// with SideEffectUGen

//object Tap {
//	// Warning: different arg order than sclang!
//	def ar( numChannels: Int = 1, bufNum: GE = 0, delayTime: GE = 0.2f ) : GE = {
//		val n = delayTime * SampleRate.ir.neg; // this depends on the session sample rate, not buffer.
//		PlayBuf.ar( numChannels, bufNum, 1, 0, n, 1 )
//	}
//}

// ScopeOut
// LocalBuf
// MaxLocalBufs
// SetBuf
// ClearBuf

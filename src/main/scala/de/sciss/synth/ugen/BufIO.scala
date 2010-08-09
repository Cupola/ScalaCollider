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

import de.sciss.synth.{ audio, control, doNothing, GE, MultiOutUGen, Rate, SideEffectUGen, SingleOutUGen, SynthGraph,
                        UGenIn }
import SynthGraph._

/**
 * 	@version	0.13, 16-Apr-10
 */
object PlayBuf {
   // note: argument 'rate' renamed to 'speed'
   def ar( numChannels: Int, bufID: GE, speed: GE = 1, trig: GE = 1, startPos: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      make( audio, numChannels, bufID, speed, trig, startPos, loop, doneAction )

   def kr( numChannels: Int, bufID: GE, speed: GE = 1, trig: GE = 1, startPos: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      make( control, numChannels, bufID, speed, trig, startPos, loop, doneAction )

   private def make( rate: Rate, numChannels: Int, bufID: GE, speed: GE, trig: GE, startPos: GE, loop: GE,
                       doneAction: GE ) =
      simplify( for( List( b, s, t, p, l, d ) <- expand( bufID, speed, trig, startPos, loop, doneAction ))
         yield this( rate, numChannels, b, s, t, p, l, d ))
}

/**
 * A UGen to play back samples from a buffer in memory.
 *
 * `PlayBuf` provides a kind of high-level interface to sample-playback, whereas `BufRd`
 * represents a kind of lower-level access. While `BufRd` has a random-access-pointer
 * in the form of a phase input, `PlayBuf` advances the phase automatically based on
 * a given playback speed. `PlayBuf` uses cubic interpolation.
 *
 * @param   numChannels the number of channels that the buffer will be. Since
 *    this is a constant, a change in number of channels of the underlying bus must
 *    be reflected by creating different SynthDefs. If a buffer identifier is used of a buffer
 *    that has a different numChannels then specified in the PlayBuf, it will fail silently.
 * @param   bufID       the identifier of the buffer to use
 * @param   rate        1.0 advances the play head by the server's sample rate each second,
 *    so 2.0 means doubling speed (and pitch), and 0.5 means half speed (and half pitch).
 *    Negative numbers can be used for backwards playback. If the underlying buffer
 *    represents a sound at a different sample rate, the rate should be
 *    multiplied by `BufRateScale.kr( bufID )` to obtain the correct speed.
 * @param   trig        a trigger which causes a jump to the given startPos. A trigger occurs when a
 *    signal changes from non-positive to positive (e.g. <= 0 to > 0).
 * @param   startPos    sample frame to start playback. This is read when a trigger occurs.
 * @param   loop        1 to loop after the play head reaches the buffer end, 0 to not loop. this can be modulated.
 * @param   doneAction  what to do when the play head reaches the buffer end.
 *
 * @see  [[de.sciss.synth.ugen.BufRd]]
 * @see  [[de.sciss.synth.ugen.DiskIn]]
 * @see  [[de.sciss.synth.ugen.RecordBuf]]
 * @see  [[de.sciss.synth.DoneAction]]
 * @see  [[de.sciss.synth.ugen.Done]]
 * @see  [[de.sciss.synth.ugen.BufRateScale]]
 * @see  [[de.sciss.synth.ugen.BufFrames]]  
 */
case class PlayBuf( rate: Rate, numChannels: Int, bufID: UGenIn, speed: UGenIn, trig: UGenIn, startPos: UGenIn,
                    loop: UGenIn, doneAction: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufID, speed, trig, startPos, loop, doneAction ))
// with SideEffectUGen // side-effect: done flag

object TGrains {
   // note: argument 'rate' renamed to 'speed'
   def ar( numChannels: Int, trig: GE, bufID: GE, speed: GE = 1, centerPos: GE = 0, dur: GE = 0.1f, pan: GE = 0,
           amp: GE = 0.1f, interp: GE = 4 ) : GE =
      simplify( for( List( b, s, c, d, p, a, i ) <- expand( bufID, speed, centerPos, dur, pan, amp, interp ))
         yield this( audio, numChannels, b, s, c, d, p, a, i ))
}
case class TGrains( rate: Rate, numChannels: Int, bufID: UGenIn, speed: UGenIn, centerPos: UGenIn, dur: UGenIn,
                    pan: UGenIn, amp: UGenIn, interp: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufID, speed, centerPos, dur, pan, amp, interp ))

object BufRd {
   def ar( numChannels: Int, bufID: GE, phase: GE = 0, loop: GE = 1, interp: GE = 2 ) : GE =
      make( audio, numChannels, bufID, phase, loop, interp )

   def kr( numChannels: Int, bufID: GE, phase: GE = 0, loop: GE = 1, interp: GE = 2 ) : GE =
      make( control, numChannels, bufID, phase, loop, interp )

   private def make( rate: Rate, numChannels: Int, bufID: GE, phase: GE, loop: GE, interp: GE ) =
      simplify( for( List( b, p, l, i ) <- expand( bufID, phase, loop, interp ))
         yield this( rate, numChannels, b, p, l, i ))
}
/**
 * A UGen which reads the content of a buffer, using an index pointer.
 *
 * Warning: if the supplied `bufID` refers to a buffer whose number of channels
 * differs from `numChannels`, the UGen will fail silently.
 *
 * An alternative to `BufRd` is `PlayBuf`. While `PlayBuf` plays
 * through the buffer by itself, `BufRd` only moves its read point by the phase input
 * and therefore has no pitch input. `PlayBuf` uses cubic interplation, while
 * `BufRd` has variable interpolation. `PlayBuf` can determine the end of the buffer
 * and issue a done-action.
 *
 * @param   numChannels    number of channels that the buffer will be.
 *    Since this is an integer constant, a change in the number of channels must
 *    be reflected by creating different SynthDefs.
 *
 * @param   bufID          the identifier of the buffer to use
 * @param   phase          audio rate frame-index into the buffer.
 * @param   loop           1 to enable looping, 0 to disable looping. this can be modulated.
 * @param   interpolation  1 for no interpolation, 2 for linear, and 4 for cubic interpolation
 *
 * @see  [[de.sciss.synth.ugen.PlayBuf]]
 * @see  [[de.sciss.synth.ugen.BufWr]]
 * @see  [[de.sciss.synth.ugen.Phasor]]
 * @see  [[de.sciss.synth.ugen.BufFrames]]
 * @see  [[de.sciss.synth.ugen.BufRateScale]]
 */
case class BufRd( rate: Rate, numChannels: Int, bufID: UGenIn, phase: UGenIn, loop: UGenIn, interp: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bufID, phase, loop, interp ))
// with SideEffectUGen // side-effect: done-flag -- NO, NOT A SIDE-EFFECT!

object BufWr {
   def ar( multi: GE, bufID: GE, phase: GE = 0, loop: GE = 1 ) : GE =
      make( audio, multi, bufID, phase, loop )

   def kr( multi: GE, bufID: GE, phase: GE = 0, loop: GE = 1 ) : GE =
      make( control, multi, bufID, phase, loop )

   private def make( rate: Rate, multi: GE, bufID: GE, phase: GE, loop: GE ) =
      simplify( for( List( b, p, l, m @ _* ) <- expand( (bufID :: phase :: loop :: multi.outputs.toList) :_* ))
         yield this( rate, b, p, l, m ))

//	checkInputs {
// 		if (rate == audio' and: {inputs.at(1).rate != audio'}, {
// 			^("phase input is not audio rate: " + inputs.at(1) + inputs.at(1).rate);
// 		});
// 		^this.checkValidInputs
// 	}
}
case class BufWr( rate: Rate, bufID: UGenIn, phase: UGenIn, loop: UGenIn, multi: Seq[ UGenIn ])
extends SingleOutUGen( (bufID :: phase :: loop :: multi.toList): _* ) with SideEffectUGen

object RecordBuf {
   def ar( multi: GE, bufID: GE, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing ) : GE =
      make( audio, multi, bufID, offset, recLevel, preLevel, run, loop, trig, doneAction )

   def kr( multi: GE, bufID: GE, offset: GE = 0, recLevel: GE = 1, preLevel: GE = 0,
			run: GE = 1, loop: GE = 1, trig: GE = 1, doneAction: GE = doNothing ) : GE =
      make( control, multi, bufID, offset, recLevel, preLevel, run, loop, trig, doneAction )

   private def make( rate: Rate, multi: GE, bufID: GE, offset: GE, recLevel: GE, preLevel: GE, run: GE,
                       loop: GE, trig: GE, doneAction: GE ) =
      simplify( for( List( b, o, r, p, n, l, t, d, m @ _* ) <- expand( (bufID :: offset :: recLevel :: preLevel ::
                     run :: loop :: trig :: doneAction :: multi.outputs.toList) :_* ))
         yield this( rate, b, o, r, p, n, l, t, d, m, SynthGraph.individuate ))
}
case class RecordBuf( rate: Rate, bufID: UGenIn, offset: UGenIn, recLevel: UGenIn, preLevel: UGenIn,
                      run: UGenIn, loop: UGenIn, trig: UGenIn, doneAction: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (bufID :: offset :: recLevel :: preLevel :: run :: loop :: trig :: doneAction :: multi.toList): _* )
with SideEffectUGen

//object Tap {
//	// Warning: different arg order than sclang!
//	def ar( numChannels: Int = 1, bufID: GE = 0, delayTime: GE = 0.2f ) : GE = {
//		val n = delayTime * SampleRate.ir.neg; // this depends on the session sample rate, not buffer.
//		PlayBuf.ar( numChannels, bufID, 1, 0, n, 1 )
//	}
//}

// ScopeOut
// LocalBuf
// MaxLocalBufs
// SetBuf
// ClearBuf

/*
 *  Demand.scala
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

import de.sciss.synth.{ audio, control, demand, doNothing, GE, MultiOutUGen, Rate, SingleOutUGen, SynthGraph,
                        UGen, UGenIn }
import SynthGraph._
import Float.{ PositiveInfinity => inf }

/**
 *    @version	0.10, 05-Aug-10
 */
object Demand {
   def ar( trig: GE, reset: GE = 0, multi: GE ) : GE = make( audio, trig, reset, multi )
   def kr( trig: GE, reset: GE = 0, multi: GE ) : GE = make( control, trig, reset, multi )

   private def make( rate: Rate, trig: GE, reset: GE, multi: GE ) : GE = {
      val args = trig +: reset +: multi.outputs
      simplify( for( t :: r :: m <- expand( args: _* ))
         yield this( rate, t, r, m.toIndexedSeq ))
   }
}

/**
 * A UGen which polls results from demand-rate ugens.
 * When there is a trigger at the `trig` input, a value is demanded from each ugen in the `multi` input
 * and output. The unit generators in the list should be demand-rate.
 * When there is a trigger at the reset input, the demand rate ugens in the list are reset.
 *
 * Note: By design, a reset trigger only resets the demand ugens; it does not reset the value at Demand's output.
 * Demand continues to hold its value until the next value is demanded, at which point its output value will
 * be the first expected item in the `multi` argument.
 *
 * Note: One demand-rate ugen represents a single stream of values, so that embedding the same ugen twice
 * calls this stream twice per demand, possibly yielding different values. To embed the same sequence
 * twice, either make sure the ugen is demanded only once, or create two instances of the ugen.
 *
 * @param   trig  trigger. Can be any signal. A trigger happens when the signal changes from non-positive to positive.
 * @param   reset trigger. Resets the list of ugens (`multi`) when triggered.
 * @param   multi a demand-rate signal (possibly multi-channel) which is read at each trigger
 *
 * @see  [[de.sciss.synth.ugen.Duty]]
 * @see  [[de.sciss.synth.ugen.TDuty]]
 */
case class Demand( rate: Rate, trig: UGenIn, reset: UGenIn, multi: IndexedSeq[ UGenIn ])
extends MultiOutUGen( rate, multi.size, trig +: reset +: multi )

object Duty extends UGen4Args {
	def ar( dur: GE = 1, reset: GE = 0, level: GE, doneAction: GE = doNothing ) :  GE =
      // ! WARNING ! different order
      arExp( dur, reset, doneAction, level )

   def kr( dur: GE = 1, reset: GE = 0, level: GE, doneAction: GE = doNothing ) :  GE =
      // ! WARNING ! different order
      krExp( dur, reset, doneAction, level )

	// XXX checkInputs
}
/**
 * @see  [[de.sciss.synth.ugen.TDuty]]
 * @see  [[de.sciss.synth.ugen.Demand]]
 */
case class Duty( rate: Rate, dur: UGenIn, reset: UGenIn, doneAction: UGenIn, level: UGenIn )
extends SingleOutUGen( dur, reset, doneAction, level )

object TDuty extends UGen5Args {
	def ar( dur: GE = 1, reset: GE = 0, level: GE = 1, doneAction: GE = doNothing, gapFirst: GE = 0 ) :  GE =
      // ! WARNING ! different order
      arExp( dur, reset, doneAction, level, gapFirst )

   def kr( dur: GE = 1, reset: GE = 0, level: GE = 1, doneAction: GE = doNothing, gapFirst: GE = 0 ) :  GE =
   // ! WARNING ! different order
      krExp( dur, reset, doneAction, level, gapFirst )

	// XXX checkInputs
}

/**
 * @see  [[de.sciss.synth.ugen.Duty]]
 * @see  [[de.sciss.synth.ugen.Demand]]
 */
case class TDuty( rate: Rate, dur: UGenIn, reset: UGenIn, doneAction: UGenIn, level: UGenIn, gapFirst: UGenIn )
extends SingleOutUGen( dur, reset, doneAction, level, gapFirst )

object DemandEnvGen extends UGen10Args {
   def ar( levels: GE, durs: GE, shapes: GE = 1, curvatures: GE = 0, gate: GE = 1, reset: GE = 1,
           levelScale: GE = 1, levelBias: GE = 0, timeScale: GE = 1, doneAction: GE = doNothing ) : GE = {
      val rGate  = Rate.highest( gate.outputs.map( _.rate ): _* )
      val rReset = Rate.highest( reset.outputs.map( _.rate ): _* )
      val (gate2, reset2) = (rGate, rReset) match {
         case (`audio`, `audio`) => (gate, reset)
         case (`audio`, _)       => (gate, K2A.ar( reset ))
         case (_, `audio`)       => (K2A.ar( gate ), reset)
         case _                  => (gate, reset)
      }
      arExp( levels, durs, shapes, curvatures, gate2, reset2, levelScale, levelBias, timeScale, doneAction )
   }

   def kr( levels: GE, durs: GE, shapes: GE = 1, curvatures: GE = 0, gate: GE = 1, reset: GE = 1,
           levelScale: GE = 1, levelBias: GE = 0, timeScale: GE = 1, doneAction: GE = doNothing ) : GE =
      krExp( levels, durs, shapes, curvatures, gate, reset, levelScale, levelBias, timeScale, doneAction )
}
case class DemandEnvGen( rate: Rate, levels: UGenIn, durs: UGenIn, shapes: UGenIn, curvatures: UGenIn,
                         gate: UGenIn, reset: UGenIn, levelScale: UGenIn, levelBias: UGenIn, timeScale: UGenIn,
                         doneAction: UGenIn )
extends SingleOutUGen( levels, durs, shapes, curvatures, gate, reset, levelScale, levelBias, timeScale,
                       doneAction )

trait DemandRateUGen {
   me: UGen =>

   // "touch" audio-rate inputs
   // see http://permalink.gmane.org/gmane.comp.audio.supercollider.user/62088
//   inputs.flatMap( _.outputs ).filter( _.rate == audio ).foreach( _.firstarg( 0 ))

   def rate = demand
}

object Dseries extends UGen3RArgsIndiv {
	def apply( start: GE = 1, step: GE = 1, length: GE = inf ) : GE = make( start, step, length )
}
case class Dseries( start: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( start, step, length ) with DemandRateUGen

object Dgeom extends UGen3RArgsIndiv {
	def apply( start: GE = 1, grow: GE = 2, length: GE = inf ) : GE = make( start, grow, length )
}
case class Dgeom( start: UGenIn, grow: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( start, grow, length ) with DemandRateUGen

object Dbufrd extends UGen3RArgsIndiv {
	def apply( bufID: GE, phase: GE = 0, loop: GE = 1 ) : GE = make( bufID, phase, loop )
}

/**
 * A demand-rate UGen that reads out a buffer. All inputs can be either demand ugen or any other ugen.
 *
 * @param   bufID the identifier of the buffer to read out
 * @param   phase the frame index into the buffer
 * @param   loop  whether to wrap an exceeding phase around the buffer length (1) or not (0)
 *
 * @see  [[de.sciss.synth.ugen.BufRd]]
 * @see  [[de.sciss.synth.ugen.Dbufwr]] 
 */
case class Dbufrd( bufID: UGenIn, phase: UGenIn, loop: UGenIn, _indiv: Int )
extends SingleOutUGen( bufID, phase, loop ) with DemandRateUGen

/**
 * @see  [[de.sciss.synth.ugen.BufWd]]
 * @see  [[de.sciss.synth.ugen.Dbufrd]] 
 */
object Dbufwr extends UGen4RArgsIndiv {
	def apply( input: GE, bufID: GE, phase: GE = 0, loop: GE = 1 ) : GE = make( input, bufID, phase, loop )
}
case class Dbufwr( input: UGenIn, bufID: UGenIn, phase: UGenIn, loop: UGenIn, _indiv: Int )
extends SingleOutUGen( input, bufID, phase, loop ) with DemandRateUGen

trait AbstractSeqDemand {
   def apply( seq: GE, repeats: GE = 1 ) : GE = make( repeats, seq )
   def apply( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int ) : UGen

   private def make( repeats: GE, seq: GE ) : GE = {
      val args = repeats +: seq.outputs
      simplify( for( r :: s <- expand( args: _* )) yield this( r, s, individuate ))
   }
}

object Dseq extends AbstractSeqDemand
case class Dseq( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dser extends AbstractSeqDemand
case class Dser( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dshuf extends AbstractSeqDemand
case class Dshuf( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Drand extends AbstractSeqDemand
case class Drand( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dxrand extends AbstractSeqDemand
case class Dxrand( repeats: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dswitch1 {
   def apply( seq: GE, index: GE ) : GE = make( index, seq )

   private def make( index: GE, seq: GE ) : GE = {
      val args = index +: seq.outputs
      simplify( for( i :: s <- expand( args: _* )) yield this( i, s, individuate ))
   }
}
case class Dswitch1( index: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (index +: seq): _* ) with DemandRateUGen

object Dswitch {
   def apply( seq: GE, index: GE ) : GE = make( index, seq )

   private def make( index: GE, seq: GE ) : GE = {
      val args = index +: seq.outputs
      simplify( for( i :: s <- expand( args: _* )) yield this( i, s, individuate ))
   }
}
case class Dswitch( index: UGenIn, seq: Seq[ UGenIn ], _indiv: Int )
extends SingleOutUGen( (index +: seq): _* ) with DemandRateUGen

object Dwhite extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, length: GE = inf ) : GE = make( lo, hi, length )
}
case class Dwhite( lo: UGenIn, hi: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, length ) with DemandRateUGen

object Diwhite extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, length: GE = inf ) : GE = make( lo, hi, length )
}
case class Diwhite( lo: UGenIn, hi: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, length ) with DemandRateUGen

object Dbrown extends UGen4RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, step: GE = 0.01f, length: GE = inf ) : GE = make( lo, hi, step, length )
}
case class Dbrown( lo: UGenIn, hi: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, step, length ) with DemandRateUGen

object Dibrown extends UGen4RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, step: GE = 0.01f, length: GE = inf ) : GE = make( lo, hi, step, length )
}
case class Dibrown( lo: UGenIn, hi: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, step, length ) with DemandRateUGen

object Dstutter extends UGen2RArgsIndiv {
	def apply( n: GE, in: GE ) : GE = make( n, in )
}
case class Dstutter( n: UGenIn, in: UGenIn, _indiv: Int )
extends SingleOutUGen( n, in ) with DemandRateUGen

object Donce extends UGen1RArgsIndiv {
	def apply( in: GE ) : GE = make( in )
}
case class Donce( in: UGenIn, _indiv: Int )
extends SingleOutUGen( in ) with DemandRateUGen

object Dreset extends UGen2RArgsIndiv {
	def apply( in: GE, reset: GE = 0 ) : GE = make( in, reset )
}
case class Dreset( in: UGenIn, reset: UGenIn, _indiv: Int )
extends SingleOutUGen( in, reset ) with DemandRateUGen

// Dpoll

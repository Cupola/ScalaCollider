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
 *    @version	0.11, 16-Aug-10
 */
object Demand {
   def ar( trig: GE, multi: GE, reset: GE = 0 ) : GE = make( audio, trig, multi, reset )
   def kr( trig: GE, multi: GE, reset: GE = 0 ) : GE = make( control, trig, multi, reset )

   private def make( rate: Rate, trig: GE, multi: GE, reset: GE ) : GE = {
      val args = trig +: reset +: multi.outputs
      simplify( for( t :: r :: m <- expand( args: _* ))
         yield this( rate, t, m.toIndexedSeq, r ))  // careful!
   }
}

/**
 * A UGen which polls results from demand-rate ugens when receiving a trigger.
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
 * '''Warning''': the argument order is different from sclang, where `reset` precedes `multi`!
 *
 * '''Warning''': Demand currently seems to have problems with infinite sequences. As a workaround
 *    use a very large length instead. E.g. instead of `Dbrown( 0, 1, inf )` use `Dbrown( 0, 1, 0xFFFFFFFF )`!
 *
 * @param   trig  trigger. Can be any signal. A trigger happens when the signal changes from non-positive to positive.
 * @param   reset trigger. Resets the list of ugens (`multi`) when triggered.
 * @param   multi a demand-rate signal (possibly multi-channel) which is read at each trigger
 *
 * @see  [[de.sciss.synth.ugen.Duty]]
 * @see  [[de.sciss.synth.ugen.TDuty]]
 */
case class Demand( rate: Rate, trig: UGenIn, multi: IndexedSeq[ UGenIn ], reset: UGenIn )
extends MultiOutUGen( rate, multi.size, trig +: reset +: multi )   // ! WARNING ! different order

object Duty extends UGen4Args {
	def ar( dur: GE = 1, reset: GE = 0, level: GE, doneAction: GE = doNothing ) :  GE =
      arExp( dur, reset, level, doneAction )

   def kr( dur: GE = 1, reset: GE = 0, level: GE, doneAction: GE = doNothing ) :  GE =
      krExp( dur, reset, level, doneAction )

	// XXX checkInputs
}
/**
 * A UGen which polls results from demand-rate ugens in intervals specified by a durational input.
 * A value from the `level` ugen is demanded and output according to a stream
 * of duration values. When there is a trigger at the reset input, the `level`
 * and the `dur` input are reset.
 *
 * @param   dur         the provider of time values. Can be a demand-rate ugen or any signal.
 *			The next poll is acquired after the previous duration.
 * @param   reset		   a trigger which resets the dur input (if demand-rated) and the
 *    the level input ugen. The reset input may also be a demand-rate ugen, in this case
 *    providing a stream of reset times.
 * @param   level       a demand-rate ugen providing the output values.
 * @param   doneAction  a doneAction that is evaluated when the duration stream ends.
 *
 * @see  [[de.sciss.synth.ugen.TDuty]]
 * @see  [[de.sciss.synth.ugen.Demand]]
 * @see  [[de.sciss.synth.DoneAction]]
 */
case class Duty( rate: Rate, dur: UGenIn, reset: UGenIn, level: UGenIn, doneAction: UGenIn )
extends SingleOutUGen( dur, reset, doneAction, level ) // ! WARNING ! different order

object TDuty extends UGen5Args {
	def ar( dur: GE = 1, reset: GE = 0, level: GE = 1, doneAction: GE = doNothing, gapFirst: GE = 0 ) :  GE =
      arExp( dur, reset, level, doneAction, gapFirst )

   def kr( dur: GE = 1, reset: GE = 0, level: GE = 1, doneAction: GE = doNothing, gapFirst: GE = 0 ) :  GE =
      krExp( dur, reset, level, doneAction, gapFirst )

	// XXX checkInputs
}

/**
 * A UGen which polls results from demand-rate ugens in intervals specified by a durational input,
 * and outputs them as trigger values.
 * A value from the `level` ugen is demanded and output for one sample (when
 * running at audio-rate) or one block (when running at control-rate) according to a stream
 * of duration values. When there is a trigger at the reset input, the `level` and
 * the `dur` input are reset.
 *
 * @param   dur         the provider of time values. Can be a demand-rate ugen or any signal.
 *			The next poll is acquired after the previous duration.
 * @param   reset		   a trigger which resets the dur input (if demand-rated) and the
 *    the level input ugen. The reset input may also be a demand-rate ugen, in this case
 *    providing a stream of reset times.
 * @param   level       a demand-rate ugen providing the output values.
 * @param   doneAction  a doneAction that is evaluated when the duration stream ends.
 * @param   gapFirst    when 0 (default), the UGen does the first level poll immediately and then
 *    waits for the first durational value. When this is 1, the UGen initially polls the first
 *    durational value, waits for that duration, and then polls the first level
 *    (along with polling the next durational value).
 *
 * @see  [[de.sciss.synth.ugen.Duty]]
 * @see  [[de.sciss.synth.ugen.Demand]]
 * @see  [[de.sciss.synth.DoneAction]]
 */
case class TDuty( rate: Rate, dur: UGenIn, reset: UGenIn, level: UGenIn, doneAction: UGenIn, gapFirst: UGenIn )
extends SingleOutUGen( dur, reset, doneAction, level, gapFirst )  // ! WARNING ! different order

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
/**
 * An envelope generator UGen using demand-rate inputs for the envelope segments.
 * For each parameter of the envelope (levels, durations and shapes), values are polled
 * every time a new segment starts.
 *
 * @param   levels      demand-rate ugen (or other ugen) returning level values
 * @param   durs        demand-rate ugen (or other ugen) returning durational values
 * @param   shapes      demand-rate ugen (or other ugen) returning shape number for the
 *    envelope segment.
 * @param   curvatures  demand-rate ugen (or other ugen) returning curvature values. these are
 *    used for curveShape segments (shape number 5) and should be zero for other shapes.
 * @param   gate	      a control rate gate: if gate is x >= 1, the ugen runs.
 *    if gate is 0 > x > 1, the ugen is released at the next level (according to doneAction).
 *    if gate is x <= 0, the ugen is sampled end held.
 * @param   reset       a trigger signal. a trigger occurs when passing from non-positive to positive.
 *    when the trigger amplitude is < 1, the input ugens (those that are demand-rated)
 *    are reset when the current segment ends. if the trigger amplitude is > 1,
 *    the reset is performed immediately.
 * @param   levelScale  demand-rate ugen returning level scaling values
 * @param   levelBias   demand-rate ugen returning level offset values
 * @param   timeScale   demand-rate ugen returning time scaling values
 * @param   doneAction  a done action performed when one of the demand-rated series ends
 *
 * @see  [[de.sciss.synth.ugen.EnvGen]]
 * @see  [[de.sciss.synth.EnvShape]]
 * @see  [[de.sciss.synth.DoneAction]] 
 */
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
	def apply( start: GE = 0, step: GE = 1, length: GE = inf ) : GE = make( start, step, length )
}

/**
 * A demand-rate UGen which produces an arithmetic (linear) series.
 *
 * The arguments can be constant or any other ugens.
 *
 * @param   start the start value of the series
 * @param   step  the incremental step by which the series changes. the step is
 *          added to the previous value on each demand.
 * @param   length   the number of elements to produces (maybe be infinite)
 *
 * @see  [[de.sciss.synth.ugen.Dgeom]]
 * @see  [[de.sciss.synth.ugen.Dseq]]
 */
case class Dseries( start: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, start, step ) with DemandRateUGen   // ! WARNING ! different order

object Dgeom extends UGen3RArgsIndiv {
	def apply( start: GE = 1, grow: GE = 2, length: GE = inf ) : GE = make( start, grow, length )
}
case class Dgeom( start: UGenIn, grow: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, start, grow ) with DemandRateUGen  // ! WARNING ! different order

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
extends SingleOutUGen( bufID, phase, input, loop ) with DemandRateUGen // ! WARNING ! different order

trait AbstractSeqDemand {
   def apply( seq: GE, repeats: GE = 1 ) : GE = make( seq, repeats )
   def apply( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int ) : UGen

   private def make( seq: GE, repeats: GE ) : GE = {
      // be ware careful here with the fucked up argument order of
      // constructor versus underlying ugen!
      val args = repeats +: seq.outputs
      simplify( for( r :: s <- expand( args: _* )) yield this( s, r, individuate ))
   }
}

object Dseq extends AbstractSeqDemand
/**
 * @see  [[de.sciss.synth.ugen.Dseries]]
 */
case class Dseq( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen // ! WARNING ! different order

object Dser extends AbstractSeqDemand
case class Dser( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dshuf extends AbstractSeqDemand
case class Dshuf( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Drand extends AbstractSeqDemand
case class Drand( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dxrand extends AbstractSeqDemand
case class Dxrand( seq: Seq[ UGenIn ], repeats: UGenIn, _indiv: Int )
extends SingleOutUGen( (repeats +: seq): _* ) with DemandRateUGen

object Dswitch1 {
   def apply( seq: GE, index: GE ) : GE = make( seq, index )

   private def make( seq: GE, index: GE ) : GE = {
      // be ware careful here with the fucked up argument order of
      // constructor versus underlying ugen!
      val args = index +: seq.outputs
      simplify( for( i :: s <- expand( args: _* )) yield this( s, i, individuate ))
   }
}
case class Dswitch1( seq: Seq[ UGenIn ], index: UGenIn, _indiv: Int )
extends SingleOutUGen( (index +: seq): _* ) with DemandRateUGen // ! WARNING ! different order

object Dswitch {
   def apply( seq: GE, index: GE ) : GE = make( seq, index )

   private def make( seq: GE, index: GE ) : GE = {
      // be ware careful here with the fucked up argument order of
      // constructor versus underlying ugen!
      val args = index +: seq.outputs
      simplify( for( i :: s <- expand( args: _* )) yield this( s, i, individuate ))
   }
}
case class Dswitch( seq: Seq[ UGenIn ], index: UGenIn, _indiv: Int )
extends SingleOutUGen( (index +: seq): _* ) with DemandRateUGen // ! WARNING ! different order

object Dwhite extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, length: GE = inf ) : GE = make( lo, hi, length )
}
case class Dwhite( lo: UGenIn, hi: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, lo, hi ) with DemandRateUGen // ! WARNING ! different order

object Diwhite extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, length: GE = inf ) : GE = make( lo, hi, length )
}
case class Diwhite( lo: UGenIn, hi: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, lo, hi ) with DemandRateUGen  // ! WARNING ! different order

object Dbrown extends UGen4RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, step: GE = 0.01f, length: GE = inf ) : GE = make( lo, hi, step, length )
}
case class Dbrown( lo: UGenIn, hi: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, lo, hi, step ) with DemandRateUGen  // ! WARNING ! different order

object Dibrown extends UGen4RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1, step: GE = 0.01f, length: GE = inf ) : GE = make( lo, hi, step, length )
}
case class Dibrown( lo: UGenIn, hi: UGenIn, step: UGenIn, length: UGenIn, _indiv: Int )
extends SingleOutUGen( length, lo, hi, step ) with DemandRateUGen    // ! WARNING ! different order

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

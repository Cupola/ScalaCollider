/*
 *  Trig.scala
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

import de.sciss.synth.{ control, ControlRated, GE, MultiOutUGen, Rate, SingleOutUGen, SynthGraph, UGenIn,
                        ZeroOutUGen }
import SynthGraph._

/**
 *	@version	0.12, 22-Apr-10
 */
object Trig1 extends UGen2Args {
  def ar( in: GE, dur: GE = 0.1f ) : GE = arExp( in, dur )
  def kr( in: GE, dur: GE = 0.1f ) : GE = krExp( in, dur )
}

/**
 * A UGen which outputs a value of 1 for a given duration when triggered.
 *
 * When a trigger occurs at the input, a value of 1 is output for the specified duration,
 * otherwise zero is output. When a new trigger occurs while this ugens outputs 1, the
 * hold-time is reset to the duration.
 *
 * @param   trig  the trigger. This can be any signal. A trigger happens when the signal changes
 *    from non-positive to positive.
 * @param   dur   the duration for which the ugens holds the value of 1 when triggered
 *
 * @see  [[de.sciss.synth.ugen.Trig]]
 */
case class Trig1( rate: Rate, in: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, dur )

object Trig extends UGen2Args {
  def ar( in: GE, dur: GE = 0.1f ) : GE = arExp( in, dur )
  def kr( in: GE, dur: GE = 0.1f ) : GE = krExp( in, dur )
}
case class Trig( rate: Rate, in: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, dur )

// XXX writeOutputSpecs?
object SendTrig extends UGen3Args {
  def ar( in: GE, id: GE = 0, value: GE = 0 ) : GE = arExp( in, id, value )
  def kr( in: GE, id: GE = 0, value: GE = 0 ) : GE = krExp( in, id, value )
}
case class SendTrig( rate: Rate, in: UGenIn, id: UGenIn, value: UGenIn )
extends ZeroOutUGen( in, id, value )

// XXX SendReply

object TDelay extends UGen2Args {
  def ar( in: GE, dur: GE = 0.1f ) : GE = arExp( in, dur )
  def kr( in: GE, dur: GE = 0.1f ) : GE = krExp( in, dur )
}
case class TDelay( rate: Rate, in: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, dur )

object Latch extends UGen2Args {
  def ar( in: GE, trig: GE ) : GE = arExp( in, trig )
  def kr( in: GE, trig: GE ) : GE = krExp( in, trig )
}
/**
 * A sample-and-hold UGen. When triggered, a new value is taken from the input and
 * hold until the next trigger occurs.
 *
 * @param   in    the input signal
 * @param   trig  the trigger. The can be any signal. A trigger happens when the signal changes from
 *    non-positive to positive.
 *
 * @see  [[de.sciss.synth.ugen.Gate]]
 * @see  [[de.sciss.synth.ugen.Demand]]
 */
case class Latch( rate: Rate, in: UGenIn, trig: UGenIn )
extends SingleOutUGen( in, trig )

object Gate extends UGen2Args {
  def ar( in: GE, trig: GE ) : GE = arExp( in, trig )
  def kr( in: GE, trig: GE ) : GE = krExp( in, trig )
}
/**
 * @see  [[de.sciss.synth.ugen.Latch]]
 */
case class Gate( rate: Rate, in: UGenIn, trig: UGenIn )
extends SingleOutUGen( in, trig )

object PulseCount extends UGen2Args {
  def ar( trig: GE, reset: GE = 0 ) : GE = arExp( trig, reset )
  def kr( trig: GE, reset: GE = 0 ) : GE = krExp( trig, reset )
}
case class PulseCount( rate: Rate, trig: UGenIn, reset: UGenIn )
extends SingleOutUGen( trig, reset )

object Peak extends UGen2Args {
  def ar( in: GE, trig: GE ) : GE = arExp( in, trig )
  def kr( in: GE, trig: GE ) : GE = krExp( in, trig )
}
case class Peak( rate: Rate, in: UGenIn, trig: UGenIn )
extends SingleOutUGen( in, trig )

object RunningMin extends UGen2Args {
  def ar( in: GE, trig: GE ) : GE = arExp( in, trig )
  def kr( in: GE, trig: GE ) : GE = krExp( in, trig )
}
case class RunningMin( rate: Rate, in: UGenIn, trig: UGenIn )
extends SingleOutUGen( in, trig )

object RunningMax extends UGen2Args {
  def ar( in: GE, trig: GE ) : GE = arExp( in, trig )
  def kr( in: GE, trig: GE ) : GE = krExp( in, trig )
}
case class RunningMax( rate: Rate, in: UGenIn, trig: UGenIn )
extends SingleOutUGen( in, trig )

object Stepper extends UGen6Args {
  def ar( trig: GE, reset: GE = 0, min: GE = 0, max: GE = 7, step: GE = 1, resetVal: GE = 0 ) : GE =
    arExp( trig, reset, min, max, step, resetVal )

  def kr( trig: GE, reset: GE = 0, min: GE = 0, max: GE = 7, step: GE = 1, resetVal: GE = 0 ) : GE =
    krExp( trig, reset, min, max, step, resetVal )
}
case class Stepper( rate: Rate, trig: UGenIn, reset: UGenIn, min: UGenIn,
                    max: UGenIn, step: UGenIn, resetVal: UGenIn )
extends SingleOutUGen( trig, reset, min, max, step, resetVal )

object PulseDivider extends UGen3Args {
  def ar( trig: GE, div: GE = 2, start: GE = 0 ) : GE = arExp( trig, div, start )
  def kr( trig: GE, div: GE = 2, start: GE = 0 ) : GE = krExp( trig, div, start )
}
case class PulseDivider( rate: Rate, trig: UGenIn, div: UGenIn, start: UGenIn )
extends SingleOutUGen( trig, div, start )

object SetResetFF extends UGen2Args {
  def ar( trig: GE, reset: GE ) : GE = arExp( trig, reset )
  def kr( trig: GE, reset: GE ) : GE = krExp( trig, reset )
}
case class SetResetFF( rate: Rate, trig: UGenIn, reset: UGenIn )
extends SingleOutUGen( trig, reset )

object ToggleFF extends UGen1Args {
  def ar( trig: GE ) : GE = arExp( trig )
  def kr( trig: GE ) : GE = krExp( trig )
}
/**
 * A UGen that toggles like a flip-flop between zero and one upon receiving a trigger.
 * The flip-flop is initially outputing zero, so changes to one when the first trigger
 * arrives.
 *
 * @param   trig  a signal to trigger the flip-flop. a trigger occurs when the signal
 *    changes from non-positive to positive.
 */
case class ToggleFF( rate: Rate, trig: UGenIn ) extends SingleOutUGen( trig )

object ZeroCrossing extends UGen1Args {
  def ar( in: GE ) : GE = arExp( in )
  def kr( in: GE ) : GE = krExp( in )
}
case class ZeroCrossing( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object Timer extends UGen1Args {
  def ar( trig: GE ) : GE = arExp( trig )
  def kr( trig: GE ) : GE = krExp( trig )
}
case class Timer( rate: Rate, trig: UGenIn ) extends SingleOutUGen( trig )

object Sweep extends UGen2Args {
  // note: argument name 'rate' already taken
  def ar( trig: GE, freq: GE ) : GE = arExp( trig, freq )
  def kr( trig: GE, freq: GE ) : GE = krExp( trig, freq )
}
/**
 * A UGen which starts a linear raise from zero each time it is
 * triggered.
 *
 * @param   trig  the trigger that restarts the ramp, when passing from
 *    non-positive to positive
 * @param   freq  the amount of increment of the output signal per second.
 *    In SCLang this argument is named `rate`, while ScalaCollider uses
 *    `freq` to avoid conflict with the UGen's calculation rate.
 *
 * @see  [[de.sciss.synth.ugen.Ramp]]
 * @see  [[de.sciss.synth.ugen.Phasor]]
 * @see  [[de.sciss.synth.ugen.Line]]
 */
case class Sweep( rate: Rate, trig: UGenIn, freq: UGenIn )
extends SingleOutUGen( trig, freq )

object Phasor extends UGen5Args {
  // note: argument name 'rate' already taken
  def ar( trig: GE, freq: GE = 1, min: GE = 0, max: GE = 1, resetVal: GE = 0 ) : GE =
    arExp( trig, freq, min, max, resetVal )

  def kr( trig: GE, freq: GE = 1, min: GE = 0, max: GE = 1, resetVal: GE = 0 ) : GE =
    krExp( trig, freq, min, max, resetVal )
}
case class Phasor( rate: Rate, trig: UGenIn, freq: UGenIn, min: UGenIn,
                   max: UGenIn, resetVal: UGenIn )
extends SingleOutUGen( trig, freq, min, max, resetVal )

object PeakFollower extends UGen2Args {
  def ar( in: GE, decay: GE = 0.999f ) : GE = arExp( in, decay )
  def kr( in: GE, decay: GE = 0.999f ) : GE = krExp( in, decay )
}
case class PeakFollower( rate: Rate, in: UGenIn, decay: UGenIn )
extends SingleOutUGen( in, decay )

object Pitch {
  def kr( in: GE, initFreq: GE = 440, minFreq: GE = 60, maxFreq: GE = 4000,
			execFreq: GE = 100, binsPerOct: GE = 16, median: GE = 1,
			ampThresh: GE = 0.01f, peakThresh: GE = 0.5f, downSample: GE = 1 ) : GE = {

    simplify( for( List( i, inf, mif, maf, exf, b, m, a, p, d ) <-
                   expand( in, initFreq, minFreq, maxFreq, execFreq, binsPerOct,
                           median, ampThresh, peakThresh, downSample ))
      yield this( i, inf, mif, maf, exf, b, m, a, p, d ))
  }
}
case class Pitch( in: UGenIn, initFreq: UGenIn, minFreq: UGenIn, maxFreq: UGenIn,
			execFreq: UGenIn, binsPerOct: UGenIn, median: UGenIn,
			ampThresh: UGenIn, peakThresh: UGenIn, downSample: UGenIn )
extends MultiOutUGen( control, 2, List( in, initFreq, minFreq,
                      maxFreq, execFreq, binsPerOct, median, ampThresh,
                      peakThresh, downSample ))
with ControlRated

object InRange extends UGen3Args {
   def ar( in: GE, min: GE = 0, max: GE = 0 ) : GE = arExp( in, min, max )
   def kr( in: GE, min: GE = 0, max: GE = 0 ) : GE = krExp( in, min, max )
   def ir( in: GE, min: GE = 0, max: GE = 0 ) : GE = irExp( in, min, max )
}
case class InRange( rate: Rate, in: UGenIn, min: UGenIn, max: UGenIn )
extends SingleOutUGen( in, min, max )

object InRect extends UGen6Args {
   def ar( x: GE, y: GE, left: GE = 0, top: GE = 0, right: GE = 1, bottom: GE = 1 ) : GE =
      arExp( x, y, left, top, right, bottom )

   def kr( x: GE, y: GE, left: GE = 0, top: GE = 0, right: GE = 1, bottom: GE = 1 ) : GE =
      krExp( x, y, left, top, right, bottom )

   def ir( x: GE, y: GE, left: GE = 0, top: GE = 0, right: GE = 1, bottom: GE = 1 ) : GE =
      irExp( x, y, left, top, right, bottom )
}
case class InRect( rate: Rate, x: UGenIn, y: UGenIn, left: UGenIn, top: UGenIn,
                   right: UGenIn, bottom: UGenIn )
extends SingleOutUGen( x, y, left, top, right, bottom )

object Fold extends UGen3Args {
   def ar( in: GE, min: GE = 0, max: GE = 0 ) : GE = arExp( in, min, max )
   def kr( in: GE, min: GE = 0, max: GE = 0 ) : GE = krExp( in, min, max )
   def ir( in: GE, min: GE = 0, max: GE = 0 ) : GE = irExp( in, min, max )
}
case class Fold( rate: Rate, in: UGenIn, min: UGenIn, max: UGenIn )
extends SingleOutUGen( in, min, max )

object Clip extends UGen3Args {
   def ar( in: GE, min: GE = 0, max: GE = 0 ) : GE = arExp( in, min, max )
   def kr( in: GE, min: GE = 0, max: GE = 0 ) : GE = krExp( in, min, max )
   def ir( in: GE, min: GE = 0, max: GE = 0 ) : GE = irExp( in, min, max )
}
case class Clip( rate: Rate, in: UGenIn, min: UGenIn, max: UGenIn )
extends SingleOutUGen( in, min, max )

object Wrap extends UGen3Args {
   def ar( in: GE, min: GE = 0, max: GE = 0 ) : GE = arExp( in, min, max )
   def kr( in: GE, min: GE = 0, max: GE = 0 ) : GE = krExp( in, min, max )
}
case class Wrap( rate: Rate, in: UGenIn, min: UGenIn, max: UGenIn )
extends SingleOutUGen( in, min, max )

object Schmidt extends UGen3Args {
   def ar( in: GE, min: GE = 0, max: GE = 0 ) : GE = arExp( in, min, max )
   def kr( in: GE, min: GE = 0, max: GE = 0 ) : GE = krExp( in, min, max )
}
case class Schmidt( rate: Rate, in: UGenIn, min: UGenIn, max: UGenIn )
extends SingleOutUGen( in, min, max )

object MostChange extends UGen2Args {
   def ar( a: GE, b: GE ) : GE = arExp( a, b )
   def kr( a: GE, b: GE ) : GE = krExp( a, b )
}
case class MostChange( rate: Rate, a: UGenIn, b: UGenIn )
extends SingleOutUGen( a, b )

object LeastChange extends UGen2Args {
   def ar( a: GE, b: GE ) : GE = arExp( a, b )
   def kr( a: GE, b: GE ) : GE = krExp( a, b )
}
case class LeastChange( rate: Rate, a: UGenIn, b: UGenIn )
extends SingleOutUGen( a, b )

object LastValue extends UGen2Args {
   def ar( in: GE, thresh: GE = 0.01f ) : GE = arExp( in, thresh )
   def kr( in: GE, thresh: GE = 0.01f ) : GE = krExp( in, thresh )
}
case class LastValue( rate: Rate, in: UGenIn, thresh: UGenIn )
extends SingleOutUGen( in, thresh )

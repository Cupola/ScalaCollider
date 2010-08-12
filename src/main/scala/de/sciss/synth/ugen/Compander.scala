/*
 *  Compander.scala
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

import de.sciss.synth.{ GE, Rate, SingleOutUGen, UGenIn }

/**
 *	@version	0.10, 01-Jan-10
 */
object Amplitude extends UGen3Args {
  def ar( in: GE, attack: GE = 0.01f, release: GE = 0.01f ) =
    arExp( in, attack, release )

  def kr( in: GE, attack: GE = 0.01f, release: GE = 0.01f ) =
    krExp( in, attack, release )
}
case class Amplitude( rate: Rate, in: UGenIn, attack: UGenIn, release: UGenIn )
extends SingleOutUGen( in, attack, release )

object Compander extends UGen7Args {
  def ar( in: GE, control: GE = 0, thresh: GE = 0.5f, ratioBelow: GE = 1,
          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
    arExp( in, control, thresh, ratioBelow, ratioAbove, attack, release )

  def kr( in: GE, control: GE = 0, thresh: GE = 0.5f, ratioBelow: GE = 1,
          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
    krExp( in, control, thresh, ratioBelow, ratioAbove, attack, release )
}

/**
 * A compressor, expander, limiter, gate and ducking UGen. This dynamic processor uses a
 * hard-knee characteristic. All of the thresholds and ratios are given as direct
 * values, not in decibels!
 *
 * @param   in          The signal to be compressed / expanded / gated.
 * @param   control     The signal whose amplitude controls the processor. Often the same as in, but one may wish
 *    to apply equalization or delay to it to change the compressor character (side-chaining), or even feed
 *    a completely different signal, for instance in a ducking application.
 * @param   thresh      Control signal amplitude threshold, which determines the break point between slopeBelow
 *    and slopeAbove. Usually 0..1. The control signal amplitude is calculated using RMS.
 * @param   ratioBelow  Slope of the amplitude curve below the threshold. If this slope > 1.0, the amplitude
 *    will drop off more quickly the softer the control signal gets; when the control signal is close to 0
 *    amplitude, the output should be exactly zero -- hence, noise gating. Values < 1.0 are possible,
 *    but it means that a very low-level control signal will cause the input signal to be amplified,
 *    which would raise the noise floor.
 * @param   ratioAbove  Slope of the amplitude curve above the threshold.. alues < 1.0 achieve compression
 *    (louder signals are attenuated); > 1.0, you get expansion (louder signals are made even louder).
 *    For 3:1 compression, you would use a value of 1/3 here.
 * @param   attack      The amount of time it takes for the amplitude adjustment to kick in fully. This is
 *    usually pretty small, not much more than 10 milliseconds (the default value). I often set it as low as
 *    2 milliseconds (0.002).
 * @param   release     The amount of time for the amplitude adjustment to be released. Usually a bit longer
 *    than attack; if both times are too short, you can get some (possibly unwanted) artifacts.
 */
case class Compander( rate: Rate, in: UGenIn, control: UGenIn, thresh: UGenIn,
                      ratioBelow: UGenIn, ratioAbove: UGenIn, attack: UGenIn,
                      release: UGenIn )
extends SingleOutUGen( in, control, thresh, ratioBelow, ratioAbove, attack, release )

//// _not_ a ugen; XXX remove
//object CompanderD {
//  def ar( in: GE, thresh: GE = 0.5f, ratioBelow: GE = 1,
//          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
//    Compander.ar( DelayN.ar( in, attack, attack ), in, thresh,
//                  ratioBelow, ratioAbove, attack, release )
//}

object Normalizer extends UGen3Args {
  def ar( in: GE, level: GE = 1, dur: GE = 0.01f ) = arExp( in, level, dur )
  def kr( in: GE, level: GE = 1, dur: GE = 0.01f ) = krExp( in, level, dur )
}
case class Normalizer( rate: Rate, in: UGenIn, level: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, level, dur )

object Limiter extends UGen3Args {
  def ar( in: GE, level: GE = 1, dur: GE = 0.01f ) = arExp( in, level, dur )
  def kr( in: GE, level: GE = 1, dur: GE = 0.01f ) = krExp( in, level, dur )
}
case class Limiter( rate: Rate, in: UGenIn, level: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, level, dur )

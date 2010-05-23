/*
 *  Filter.scala
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
import SynthGraph._

/**
 * 	@version	0.12, 01-Jan-10
 */
object Resonz extends UGen3Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = arExp( in, freq, rq )
  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = krExp( in, freq, rq )
}
case class Resonz( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq )

object OnePole extends UGen2Args {
	def ar( in: GE, coeff: GE = 0.5f ) : GE = arExp( in, coeff )
	def kr( in: GE, coeff: GE = 0.5f ) : GE = krExp( in, coeff )
}
case class OnePole( rate: Rate, in: UGenIn, coeff: UGenIn )
extends SingleOutUGen( in, coeff )

object OneZero extends UGen2Args {
	def ar( in: GE, coeff: GE = 0.5f ) : GE = arExp( in, coeff )
	def kr( in: GE, coeff: GE = 0.5f ) : GE = krExp( in, coeff )
}
case class OneZero( rate: Rate, in: UGenIn, coeff: UGenIn )
extends SingleOutUGen( in, coeff )

object TwoPole extends UGen3Args {
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = arExp( in, freq, radius )
	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = krExp( in, freq, radius )
}
case class TwoPole( rate: Rate, in: UGenIn, freq: UGenIn, radius: UGenIn )
extends SingleOutUGen( in, freq, radius )

object TwoZero extends UGen3Args {
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = arExp( in, freq, radius )
	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = krExp( in, freq, radius )
}
case class TwoZero( rate: Rate, in: UGenIn, freq: UGenIn, radius: UGenIn )
extends SingleOutUGen( in, freq, radius )

object APF extends UGen3Args {
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = arExp( in, freq, radius )
	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = krExp( in, freq, radius )
}
case class APF( rate: Rate, in: UGenIn, freq: UGenIn, radius: UGenIn )
extends SingleOutUGen( in, freq, radius )

object Integrator extends UGen2Args {
	def ar( in: GE, coeff: GE = 1 ) : GE = arExp( in, coeff )
	def kr( in: GE, coeff: GE = 1 ) : GE = krExp( in, coeff )
}
case class Integrator( rate: Rate, in: UGenIn, coeff: UGenIn )
extends SingleOutUGen( in, coeff )

object Decay extends UGen2Args {
	def ar( in: GE, time: GE = 1 ) : GE = arExp( in, time )
	def kr( in: GE, time: GE = 1 ) : GE = krExp( in, time )
}
case class Decay( rate: Rate, in: UGenIn, time: UGenIn )
extends SingleOutUGen( in, time )

object Decay2 extends UGen3Args {
	def ar( in: GE, attack: GE = 0.01f, release: GE = 1 ) : GE =
      arExp( in, attack, release )
	def kr( in: GE, attack: GE = 0.01f, release: GE = 1 ) : GE =
      krExp( in, attack, release )
}
case class Decay2( rate: Rate, in: UGenIn, attack: UGenIn, release: UGenIn )
extends SingleOutUGen( in, attack, release )

object Lag extends UGen2Args {
	def ar( in: GE, time: GE = 0.1f ) : GE = arExp( in, time )
	def kr( in: GE, time: GE = 0.1f ) : GE = krExp( in, time )
}
case class Lag( rate: Rate, in: UGenIn, time: UGenIn )
extends SingleOutUGen( in, time )

object Lag2 extends UGen2Args {
	def ar( in: GE, time: GE = 0.1f ) : GE = arExp( in, time )
	def kr( in: GE, time: GE = 0.1f ) : GE = krExp( in, time )
}
case class Lag2( rate: Rate, in: UGenIn, time: UGenIn )
extends SingleOutUGen( in, time )

object Lag3 extends UGen2Args {
	def ar( in: GE, time: GE = 0.1f ) : GE = arExp( in, time )
	def kr( in: GE, time: GE = 0.1f ) : GE = krExp( in, time )
}
case class Lag3( rate: Rate, in: UGenIn, time: UGenIn )
extends SingleOutUGen( in, time )

object Ramp extends UGen2Args {
	def ar( in: GE, time: GE = 0.1f ) : GE = arExp( in, time )
	def kr( in: GE, time: GE = 0.1f ) : GE = krExp( in, time )
}
case class Ramp( rate: Rate, in: UGenIn, time: UGenIn )
extends SingleOutUGen( in, time )

object LagUD extends UGen3Args {
	def ar( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      arExp( in, timeUp, timeDown )
	def kr( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      krExp( in, timeUp, timeDown )
}
case class LagUD( rate: Rate, in: UGenIn, timeUp: UGenIn, timeDown: UGenIn )
extends SingleOutUGen( in, timeUp, timeDown )

object Lag2UD extends UGen3Args {
	def ar( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      arExp( in, timeUp, timeDown )
	def kr( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      krExp( in, timeUp, timeDown )
}
case class Lag2UD( rate: Rate, in: UGenIn, timeUp: UGenIn, timeDown: UGenIn )
extends SingleOutUGen( in, timeUp, timeDown )

object Lag3UD extends UGen3Args {
	def ar( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      arExp( in, timeUp, timeDown )
	def kr( in: GE, timeUp: GE = 0.1f, timeDown: GE = 0.1f ) : GE =
      krExp( in, timeUp, timeDown )
}
case class Lag3UD( rate: Rate, in: UGenIn, timeUp: UGenIn, timeDown: UGenIn )
extends SingleOutUGen( in, timeUp, timeDown )

object LeakDC extends UGen2Args {
	def ar( in: GE, coeff: GE = 0.995f ) : GE = arExp( in, coeff )
	def kr( in: GE, coeff: GE = 0.9f ) : GE = krExp( in, coeff )
}
case class LeakDC( rate: Rate, in: UGenIn, coeff: UGenIn )
extends SingleOutUGen( in, coeff )

object RLPF extends UGen3Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = arExp( in, freq, rq )
  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = krExp( in, freq, rq )
}
case class RLPF( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq )

object RHPF extends UGen3Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = arExp( in, freq, rq )
  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = krExp( in, freq, rq )
}
case class RHPF( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq )

object LPF extends UGen2Args {
  def ar( in: GE, freq: GE = 440 ) : GE = arExp( in, freq )
  def kr( in: GE, freq: GE = 440 ) : GE = krExp( in, freq )
}
case class LPF( rate: Rate, in: UGenIn, freq: UGenIn )
extends SingleOutUGen( in, freq )

object HPF extends UGen2Args {
  def ar( in: GE, freq: GE = 440 ) : GE = arExp( in, freq )
  def kr( in: GE, freq: GE = 440 ) : GE = krExp( in, freq )
}
case class HPF( rate: Rate, in: UGenIn, freq: UGenIn )
extends SingleOutUGen( in, freq )

object BPF extends UGen3Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = arExp( in, freq, rq )
  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = krExp( in, freq, rq )
}
case class BPF( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq )

object BRF extends UGen3Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = arExp( in, freq, rq )
  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = krExp( in, freq, rq )
}
case class BRF( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq )

object MidEQ extends UGen4Args {
  def ar( in: GE, freq: GE = 440, rq: GE = 1, db: GE = 0 ) : GE =
    arExp( in, freq, rq, db )
  def kr( in: GE, freq: GE = 440, rq: GE = 1, db: GE = 0 ) : GE =
    krExp( in, freq, rq, db )
}
case class MidEQ( rate: Rate, in: UGenIn, freq: UGenIn, rq: UGenIn, db: UGenIn )
extends SingleOutUGen( in, freq, rq, db )

object LPZ1 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class LPZ1( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object HPZ1 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class HPZ1( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object Slope extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class Slope( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object LPZ2 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class LPZ2( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object HPZ2 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class HPZ2( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object BPZ2 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class BPZ2( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object BRZ2 extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
case class BRZ2( rate: Rate, in: UGenIn )
extends SingleOutUGen( in )

object Median extends UGen2Args {
  def ar( length: GE = 3, in: GE ) : GE = arExp( length, in )
  def kr( length: GE = 3, in: GE ) : GE = krExp( length, in )
}
case class Median( rate: Rate, length: UGenIn, in: UGenIn )
extends SingleOutUGen( length, in )

object Slew extends UGen3Args {
  def ar( in: GE, up: GE = 1, down: GE = 1 ) : GE = arExp( in, up, down )
  def kr( in: GE, up: GE = 1, down: GE = 1 ) : GE = krExp( in, up, down )
}
case class Slew( rate: Rate, in: UGenIn, up: UGenIn, down: UGenIn )
extends SingleOutUGen( in, up, down )

object FOS extends UGen4Args {
  def ar( in: GE, a0: GE = 0, a1: GE = 0, b1: GE = 0 ) : GE =
    arExp( in, a0, a1, b1 )

  def kr( in: GE, a0: GE = 0, a1: GE = 0, b1: GE = 0 ) : GE =
    krExp( in, a0, a1, b1 )
}
case class FOS( rate: Rate, in: UGenIn, a0: UGenIn, a1: UGenIn, b1: UGenIn )
extends SingleOutUGen( in, a0, a1, b1)

object SOS extends UGen6Args {
  def ar( in: GE, a0: GE = 0, a1: GE = 0, a2: GE = 0, b1: GE = 0, b2: GE = 0 ) : GE =
    arExp( in, a0, a1, a2, b1, b2 )

  def kr( in: GE, a0: GE = 0, a1: GE = 0, a2: GE = 0, b1: GE = 0, b2: GE = 0 ) : GE =
    krExp( in, a0, a1, a2, b1, b2 )
}
case class SOS( rate: Rate, in: UGenIn, a0: UGenIn, a1: UGenIn, a2: UGenIn,
                b1: UGenIn, b2: UGenIn )
extends SingleOutUGen( in, a0, a1, a2, b1, b2 )

object Ringz extends UGen4Args {
  def ar( in: GE, freq: GE = 440, attack: GE = 1, decay: GE = 1 ) : GE =
    arExp( in, freq, attack, decay )

  def kr( in: GE, freq: GE = 440, attack: GE = 1, decay: GE = 1 ) : GE =
    krExp( in, freq, attack, decay )
}
case class Ringz( rate: Rate, in: UGenIn, freq: UGenIn, attack: UGenIn, decay: UGenIn )
extends SingleOutUGen( in, freq, attack, decay )

object Formlet extends UGen4Args {
  def ar( in: GE, freq: GE = 440, attack: GE = 1, decay: GE = 1 ) : GE =
    arExp( in, freq, attack, decay )

  def kr( in: GE, freq: GE = 440, attack: GE = 1, decay: GE = 1 ) : GE =
    krExp( in, freq, attack, decay )
}
case class Formlet( rate: Rate, in: UGenIn, freq: UGenIn, attack: UGenIn, decay: UGenIn )
extends SingleOutUGen( in, freq, attack, decay )

object DetectSilence extends UGen4Args {
  def ar( in: GE, amp: GE = 0.0001f, time: GE = 0.1f, doneAction: GE = 0 ) : GE =
    arExp( in, amp, time, doneAction )

  def kr( in: GE, amp: GE = 0.0001f, time: GE = 0.1f, doneAction: GE = 0 ) : GE =
    krExp( in, amp, time, doneAction )
}
case class DetectSilence( rate: Rate, in: UGenIn, amp: UGenIn, time: UGenIn,
                          doneAction: UGenIn )
extends SingleOutUGen( in, amp, time, doneAction ) with SideEffectUGen // side-effect: done-action


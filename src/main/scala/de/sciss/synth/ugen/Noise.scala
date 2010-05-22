/*
 *  Noise.scala
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

import collection.breakOut
import de.sciss.synth._
import GraphBuilder._

/**
 *    @version	0.13, 22-Apr-10
 */
object RandSeed extends UGen2Args {
	def kr( trig: GE, seed: GE = 56789 ) : GE = krExp( trig, seed )
	def ir( trig: GE, seed: GE = 56789 ) : GE = irExp( trig, seed )
}
case class RandSeed( rate: Rate, trig: UGenIn, seed: UGenIn )
extends SingleOutUGen( trig, seed ) with SideEffectUGen

object RandID extends UGen1Args {
	def kr( id: GE ) : GE = krExp( id )
	def ir( id: GE ) : GE = irExp( id )
}
case class RandID( rate: Rate, id: UGenIn )
extends SingleOutUGen( id ) // with ExclusiveUGen

object Rand extends UGen2RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 1 ) : GE = make( lo, hi )
}
case class Rand( lo: UGenIn, hi: UGenIn, _indiv: Int ) extends SingleOutUGen( lo, hi )
with ScalarRated

object IRand extends UGen2RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 127 ) : GE = make( lo, hi )
}
case class IRand( lo: UGenIn, hi: UGenIn, _indiv: Int ) extends SingleOutUGen( lo, hi )
with ScalarRated

object TRand extends UGen3ArgsIndiv {
	def ar( lo: GE = 0, hi: GE = 1, trig: GE ) : GE = arExp( lo, hi, trig )
	def kr( lo: GE = 0, hi: GE = 1, trig: GE ) : GE = krExp( lo, hi, trig )
}
case class TRand( rate: Rate, lo: UGenIn, hi: UGenIn, trig: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, trig )

object TIRand extends UGen3ArgsIndiv {
	def ar( lo: GE = 0, hi: GE = 127, trig: GE ) : GE = arExp( lo, hi, trig )
	def kr( lo: GE = 0, hi: GE = 127, trig: GE ) : GE = krExp( lo, hi, trig )
}
case class TIRand( rate: Rate, lo: UGenIn, hi: UGenIn, trig: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, trig )

object LinRand extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 127, minMax: GE = 0 ) : GE =
      make( lo, hi, minMax )
}
case class LinRand( lo: UGenIn, hi: UGenIn, minMax: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, minMax ) with ScalarRated

object NRand extends UGen3RArgsIndiv {
	def apply( lo: GE = 0, hi: GE = 127, n: GE = 0 ) : GE =
      make( lo, hi, n )
}
case class NRand( lo: UGenIn, hi: UGenIn, n: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, n ) with ScalarRated

object ExpRand extends UGen2RArgsIndiv {
	def apply( lo: GE = 0.01f, hi: GE = 1 ) : GE = make( lo, hi )
}
case class ExpRand( lo: UGenIn, hi: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi ) with ScalarRated

object TExpRand extends UGen3ArgsIndiv {
	def ar( lo: GE = 0.01f, hi: GE = 1, trig: GE ) : GE = arExp( lo, hi, trig )
	def kr( lo: GE = 0.01f, hi: GE = 1, trig: GE ) : GE = krExp( lo, hi, trig )
}
case class TExpRand( rate: Rate, lo: UGenIn, hi: UGenIn, trig: UGenIn, _indiv: Int )
extends SingleOutUGen( lo, hi, trig )

object CoinGate extends UGen2ArgsIndiv {
	def ar( prob: GE = 0.5f, in: GE ) : GE = arExp( prob, in )
	def kr( prob: GE = 0.5f, in: GE ) : GE = krExp( prob, in )
}
case class CoinGate( rate: Rate, prob: UGenIn, in: UGenIn, _indiv: Int )
extends SingleOutUGen( prob, in )

object TWindex {
   def ar( trig: GE, list: GE, normalize: GE = 0 ) : GE =
      make( audio, trig, list, normalize )

   def kr( trig: GE, list: GE, normalize: GE = 0 ) : GE =
      make( control, trig, list, normalize )

   private def make( rate: Rate, trig: GE, list: GE, normalize: GE ) : GE =
      simplify( for( List( t, n, l @ _* ) <-
                  expand( (trig :: normalize :: list.outputs.toList): _* ))
                     yield this( rate, t, l, n, SynthGraph.individuate ))

//   def apply( rate: Rate, trig: UGenIn, list: Seq[ UGenIn ], normalize: UGenIn ) =
//      new TWindex( rate, trig, list, normalize )
}
case class TWindex( rate: Rate, trig: UGenIn, list: Seq[ UGenIn ], normalize: UGenIn, _indiv: Int )
extends SingleOutUGen( (trig :: normalize :: list.toList): _* )

trait NoiseUGen {
   def apply( rate: Rate, _indiv: Int ) : SingleOutUGen

   def ar: SingleOutUGen = this( audio,   SynthGraph.individuate )
   def kr: SingleOutUGen = this( control, SynthGraph.individuate )

   private def make( make1: => SingleOutUGen, mul: GE ) : GE = {
//      val numOutputs = mul.numOutputs
//      val outputs    = mul.outputs
//      if( numOutputs == 1 ) make1 * outputs.head
//      else UGenInSeq( outputs.map( m => BinaryOpUGen.make1( BinaryOpUGen.Times, make1, m )))
//      val zipped = Vector.fill[ UGenIn ]( mul.numOutputs )( make1 ).zip( mul.outputs )
//      seq( zipped.flatMap( p => (p._1 * p._2).outputs ))
      seq( mul.outputs.flatMap( m => (make1 * m).outputs )( breakOut ))
   }

   def ar( mul: GE ): GE = make( ar, mul )
   def kr( mul: GE ): GE = make( kr, mul )
}

object WhiteNoise extends NoiseUGen // { type noiseType = WhiteNoise }
case class WhiteNoise( rate: Rate, _indiv: Int ) extends SingleOutUGen()

object BrownNoise extends NoiseUGen // { type noiseType = BrownNoise }
case class BrownNoise( rate: Rate, _indiv: Int ) extends SingleOutUGen()

object PinkNoise extends NoiseUGen // { type noiseType = PinkNoise }
case class PinkNoise( rate: Rate, _indiv: Int ) extends SingleOutUGen()

object ClipNoise extends NoiseUGen // { type noiseType = ClipNoise }
case class ClipNoise( rate: Rate, _indiv: Int ) extends SingleOutUGen()

object GrayNoise extends NoiseUGen // { type noiseType = GrayNoise }
case class GrayNoise( rate: Rate, _indiv: Int ) extends SingleOutUGen()

object Crackle extends UGen1Args {
   def ar : GE = ar()
	def ar( chaosParam: GE = 1.5f ) : GE = arExp( chaosParam )
   def kr : GE = kr()
	def kr( chaosParam: GE = 1.5f ) : GE = krExp( chaosParam )
}
// note: Crackle is deterministic in the sense that
// two UGens with the same chaosParam will produce
// the exact same output.
case class Crackle( rate: Rate, chaosParam: UGenIn )
extends SingleOutUGen( chaosParam )

object Logistic extends UGen3Args {
   def ar : GE = ar()
	def ar( chaosParam: GE = 3, freq: GE = 1000, init: GE = 0.5f ) : GE =
      arExp( chaosParam, freq, init )

   def kr : GE = kr()
	def kr( chaosParam: GE = 3, freq: GE = 1000, init: GE = 0.5f ) : GE =
      krExp( chaosParam, freq, init )
}
// note: deterministic
case class Logistic( rate: Rate, chaosParam: UGenIn, freq: UGenIn, init: UGenIn )
extends SingleOutUGen( chaosParam, freq, init )

object LFNoise0 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFNoise0( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFNoise1 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFNoise1( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFNoise2 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFNoise2( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFClipNoise extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFClipNoise( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFDNoise0 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFDNoise0( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFDNoise1 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFDNoise1( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFDNoise3 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFDNoise3( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object LFDClipNoise extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( freq: GE = 500 ) : GE = arExp( freq )
   def kr : GE = kr()
	def kr( freq: GE = 500 ) : GE = krExp( freq )
}
case class LFDClipNoise( rate: Rate, freq: UGenIn, _indiv: Int ) extends SingleOutUGen( freq )

object Hasher extends UGen1Args {
	def ar( in: GE ) : GE = arExp( in )
	def kr( in: GE ) : GE = krExp( in )
}
// note: deterministic
case class Hasher( rate: Rate, in: UGenIn ) extends SingleOutUGen( in )

object MantissaMask extends UGen2Args {
	def ar( in: GE, bits: GE = 3 ) : GE = arExp( in, bits )
	def kr( in: GE, bits: GE = 3 ) : GE = krExp( in, bits )
}
// note: deterministic
case class MantissaMask( rate: Rate, in: UGenIn, bits: UGenIn )
extends SingleOutUGen( in, bits )

object Dust extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( density: GE = 1 ) : GE = arExp( density )
   def kr : GE = kr()
	def kr( density: GE = 1 ) : GE = krExp( density )
}
case class Dust( rate: Rate, density: UGenIn, _indiv: Int ) extends SingleOutUGen( density )

object Dust2 extends UGen1ArgsIndiv {
   def ar : GE = ar()
	def ar( density: GE = 1 ) : GE = arExp( density )
   def kr : GE = kr()
	def kr( density: GE = 1 ) : GE = krExp( density )
}
case class Dust2( rate: Rate, density: UGenIn, _indiv: Int ) extends SingleOutUGen( density )

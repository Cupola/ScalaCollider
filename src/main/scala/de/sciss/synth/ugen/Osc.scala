/*
 *  Osc.scala
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
import GraphBuilder._

/**
 *  @version  0.11, 01-Jan-10
 */
object Osc extends UGen3Args {
  def ar( bufID: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    arExp( bufID, freq, phase )

  def kr( bufID: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    krExp( bufID, freq, phase )
}
case class Osc( rate: Rate, bufID: UGenIn, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( bufID, freq, phase )

// class Osc private ( r: Rate, val bufID: UGenIn, val freq: UGenIn, val phase: UGenIn )
// extends UGen( "Osc", r, List( r ), List( bufID, freq, phase )) {
// }

object SinOsc extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, phase: GE = 0 ) : GE = arExp( freq, phase )
   def kr : GE = kr()
   def kr( freq: GE = 440, phase: GE = 0 ) : GE = krExp( freq, phase )
}
case class SinOsc( rate: Rate, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( freq, phase )

object SinOscFB extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, feedback: GE = 0 ) : GE = arExp( freq, feedback )
   def kr : GE = kr()
   def kr( freq: GE = 440, feedback: GE = 0 ) : GE = krExp( freq, feedback )
}
case class SinOscFB( rate: Rate, freq: UGenIn, feedback: UGenIn )
extends SingleOutUGen( freq, feedback )

object OscN extends UGen3Args {
  def ar( bufID: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    arExp( bufID, freq, phase )

  def kr( bufID: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    krExp( bufID, freq, phase )
}
case class OscN( rate: Rate, bufID: UGenIn, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( bufID, freq, phase ) // with SideEffectUGen

object VOsc extends UGen3Args {
  def ar( bufPos: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    arExp( bufPos, freq, phase )

  def kr( bufPos: GE, freq: GE = 440, phase: GE = 0 ) : GE =
    krExp( bufPos, freq, phase )
}
case class VOsc( rate: Rate, bufPos: UGenIn, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( bufPos, freq, phase ) // with SideEffectUGen

object VOsc3 extends UGen4Args {
  def ar( bufPos: GE, freq1: GE = 110, freq2: GE = 220, freq3: GE = 440 ) : GE =
    arExp( bufPos, freq1, freq2, freq3 )

  def kr( bufPos: GE, freq1: GE = 110, freq2: GE = 220, freq3: GE = 440 ) : GE =
    krExp( bufPos, freq1, freq2, freq3 )
}
case class VOsc3( rate: Rate, bufPos: UGenIn, freq1: UGenIn, freq2: UGenIn, freq3: UGenIn )
extends SingleOutUGen( bufPos, freq1, freq2, freq3 ) // with SideEffectUGen

object COsc extends UGen3Args {
  def ar( bufID: GE, freq: GE = 440, beats: GE = 0.5f ) : GE =
    arExp( bufID, freq, beats )
}
case class COsc( rate: Rate, bufID: UGenIn, freq: UGenIn, beats: UGenIn )
extends SingleOutUGen( bufID, freq, beats ) // with SideEffectUGen

object Formant extends UGen3Args {
   def ar : GE = ar()
   def ar( fundFreq: GE = 440, formFreq: GE = 1760, bw: GE = 880 ) : GE =
      arExp( fundFreq, formFreq, bw )

   def kr : GE = kr()
   def kr( fundFreq: GE = 440, formFreq: GE = 1760, bw: GE = 880 ) : GE =
      krExp( fundFreq, formFreq, bw )
}
case class Formant( rate: Rate, fundFreq: UGenIn, formFreq: UGenIn, bw: UGenIn )
extends SingleOutUGen( fundFreq, formFreq, bw )

object LFSaw extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0 ) : GE = arExp( freq, iphase )
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0 ) : GE = krExp( freq, iphase )
}
case class LFSaw( rate: Rate, freq: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, iphase )

object LFPar extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0 ) : GE = arExp( freq, iphase )
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0 ) : GE = krExp( freq, iphase )
}
case class LFPar( rate: Rate, freq: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, iphase )

object LFCub extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0 ) : GE = arExp( freq, iphase )
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0 ) : GE = krExp( freq, iphase )
}
case class LFCub( rate: Rate, freq: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, iphase )

object LFTri extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0 ) : GE = arExp( freq, iphase )
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0 ) : GE = krExp( freq, iphase )
}
case class LFTri( rate: Rate, freq: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, iphase )

object LFGauss extends UGen5Args {
   def ar : GE = ar()
   def ar( dur: GE = 1, width: GE = 0.1f, iphase: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      arExp( dur, width, iphase, loop, doneAction )

   def kr : GE = kr()
   def kr( dur: GE = 1, width: GE = 0.1f, iphase: GE = 0, loop: GE = 1,
           doneAction: GE = doNothing ) : GE =
      krExp( dur, width, iphase, loop, doneAction )
}
case class LFGauss( rate: Rate, dur: UGenIn, width: UGenIn, iphase: UGenIn,
                    loop: UGenIn, doneAction: UGenIn )
extends SingleOutUGen( dur, width, iphase, loop, doneAction )

object LFPulse extends UGen3Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0, width: GE = 0.5f ) : GE =
      arExp( freq, iphase, width )
  
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0, width: GE = 0.5f ) : GE =
      krExp( freq, iphase, width )
}
case class LFPulse( rate: Rate, freq: UGenIn, iphase: UGenIn, width: UGenIn )
extends SingleOutUGen( freq, iphase, width )

object VarSaw extends UGen3Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0, width: GE = 0.5f ) : GE =
      arExp( freq, iphase, width )

   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0, width: GE = 0.5f ) : GE =
      krExp( freq, iphase, width )
}
case class VarSaw( rate: Rate, freq: UGenIn, iphase: UGenIn, width: UGenIn )
extends SingleOutUGen( freq, iphase, width )

object Impulse extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, phase: GE = 0 ) : GE = arExp( freq, phase )
   def kr : GE = kr()
   def kr( freq: GE = 440, phase: GE = 0 ) : GE = krExp( freq, phase )
}
case class Impulse( rate: Rate, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( freq, phase )

object SyncSaw extends UGen2Args {
   def ar : GE = ar()
   def ar( syncFreq: GE = 440, sawFreq: GE = 440 ) : GE = arExp( syncFreq, sawFreq )
   def kr : GE = kr()
   def kr( syncFreq: GE = 440, sawFreq: GE = 440 ) : GE = krExp( syncFreq, sawFreq )
}
case class SyncSaw( rate: Rate, syncFreq: UGenIn, sawFreq: UGenIn )
extends SingleOutUGen( syncFreq, sawFreq )

object Index extends UGen2Args {
  def ar( bufID: GE, in: GE = 0 ) : GE = arExp( bufID, in )
  def kr( bufID: GE, in: GE = 0 ) : GE = krExp( bufID, in )
}
case class Index( rate: Rate, bufID: UGenIn, in: UGenIn )
extends SingleOutUGen( bufID, in ) // with SideEffectUGen

object WrapIndex extends UGen2Args {
  def ar( bufID: GE, in: GE = 0 ) : GE = arExp( bufID, in )
  def kr( bufID: GE, in: GE = 0 ) : GE = krExp( bufID, in )
}
case class WrapIndex( rate: Rate, bufID: UGenIn, in: UGenIn )
extends SingleOutUGen( bufID, in ) // with SideEffectUGen

object IndexInBetween extends UGen2Args {
  def ar( bufID: GE, in: GE = 0 ) : GE = arExp( bufID, in )
  def kr( bufID: GE, in: GE = 0 ) : GE = krExp( bufID, in )
}
case class IndexInBetween( rate: Rate, bufID: UGenIn, in: UGenIn )
extends SingleOutUGen( bufID, in ) // with SideEffectUGen

object DetectIndex extends UGen2Args {
  def ar( bufID: GE, in: GE = 0 ) : GE = arExp( bufID, in )
  def kr( bufID: GE, in: GE = 0 ) : GE = krExp( bufID, in )
}
case class DetectIndex( rate: Rate, bufID: UGenIn, in: UGenIn )
extends SingleOutUGen( bufID, in ) // with SideEffectUGen

object Shaper extends UGen2Args {
  def ar( bufID: GE, in: GE = 0 ) : GE = arExp( bufID, in )
  def kr( bufID: GE, in: GE = 0 ) : GE = krExp( bufID, in )
}
case class Shaper( rate: Rate, bufID: UGenIn, in: UGenIn )
extends SingleOutUGen( bufID, in )

// IndexL XXX

object DegreeToKey extends UGen3Args {
  def ar( bufID: GE, in: GE, octave: GE = 12 ) : GE = arExp( bufID, in, octave )
  def kr( bufID: GE, in: GE, octave: GE = 12 ) : GE = krExp( bufID, in, octave )
}
case class DegreeToKey( rate: Rate, bufID: UGenIn, in: UGenIn, octave: UGenIn )
extends SingleOutUGen( bufID, in, octave )

object Select {
  def ar( index: GE, multi: GE ) : GE = make( audio, index, multi )
  def kr( index: GE, multi: GE ) : GE = make( control, index, multi )

  private def make( rate: Rate, index: GE, multi: GE ) : GE =
    simplify( for( List( i, m @ _* ) <-
                     expand( (index :: multi.outputs.toList): _* ))
                yield this( rate, i, m ))
}
case class Select( rate: Rate, index: UGenIn, multi: Seq[ UGenIn ])
extends SingleOutUGen( (index :: multi.toList): _* )

object Vibrato {
   def ar : GE = ar()
   def ar( freq: GE = 440, beat: GE = 6, depth: GE = 0.02f, delay: GE = 0, onset: GE = 0,
           rateVar: GE = 0.04f, depthVar: GE = 0.1f, iphase: GE = 0 ) : GE =
      make( audio, freq, beat, depth, delay, onset, rateVar, depthVar, iphase )

   def kr : GE = kr()
   def kr( freq: GE = 440, beat: GE = 6, depth: GE = 0.02f, delay: GE = 0, onset: GE = 0,
           rateVar: GE = 0.04f, depthVar: GE = 0.1f, iphase: GE = 0 ) : GE =
      make( control, freq, beat, depth, delay, onset, rateVar, depthVar, iphase )

   // note: 'rate' argument already taken, using 'beat' instead
   private def make( rate: Rate, freq: GE = 440, beat: GE = 6, depth: GE = 0.02f,
                     delay: GE = 0, onset: GE = 0, rateVar: GE = 0.04f,
                     depthVar: GE = 0.1f, iphase: GE = 0 ) : GE = {

      simplify( for( List( f, b, d, dly, o, rv, dv, p ) <-
            expand( freq, beat, depth, delay, onset, rateVar, depthVar, iphase ))
         yield this( rate, f, b, d, dly, o, rv, dv, p ))
   }
}
case class Vibrato( rate: Rate, freq: UGenIn, beat: UGenIn, depth: UGenIn,
                    delay: UGenIn, onset: UGenIn, rateVar: UGenIn,
                    depthVar: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, beat, depth, delay, onset, rateVar, depthVar, iphase )

// TChoose XXX
// TWChoose XXX

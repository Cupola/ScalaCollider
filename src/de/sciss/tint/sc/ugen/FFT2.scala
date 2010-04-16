/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

// continuous convolution in freq domain
object Convolution extends UGen3Args {
   def ar( in: GE, kernel: GE, frameSize: GE ) : GE = arExp( in, kernel, frameSize )
}
case class Convolution( rate: Rate, in: UGenIn, kernel: UGenIn, frameSize: UGenIn )
extends SingleOutUGen( in, kernel, frameSize ) // with SideEffectUGen

// triggered convolution in freq domain
object Convolution2 extends UGen4Args {
   def ar( in: GE, kernel: GE, trig: GE, frameSize: GE ) : GE = arExp( in, kernel, trig, frameSize )
}
case class Convolution2( rate: Rate, in: UGenIn, kernel: UGenIn, trig: UGenIn, frameSize: UGenIn )
extends SingleOutUGen( in, kernel, trig, frameSize ) // with SideEffectUGen

// triggered cross-faded convolution in freq domain
object Convolution2L extends UGen5Args {
   def ar( in: GE, kernel: GE, trig: GE, frameSize: GE, fadePeriods: GE = 1 ) : GE =
      arExp( in, kernel, trig, frameSize, fadePeriods )
}
case class Convolution2L( rate: Rate, in: UGenIn, kernel: UGenIn, trig: UGenIn, frameSize: UGenIn, fadePeriods: UGenIn )
extends SingleOutUGen( in, kernel, trig, frameSize, fadePeriods ) // with SideEffectUGen

// triggered cross-faded stereo convolution in freq domain
// StereoConvolution2L XXX

// triggered convolution in time domain
object Convolution3 extends UGen4Args {
   def ar( in: GE, kernel: GE, trig: GE, frameSize: GE ) : GE = arExp( in, kernel, trig, frameSize )
   def kr( in: GE, kernel: GE, trig: GE, frameSize: GE ) : GE = krExp( in, kernel, trig, frameSize )
}
case class Convolution3( rate: Rate, in: UGenIn, kernel: UGenIn, trig: UGenIn, frameSize: UGenIn )
extends SingleOutUGen( in, kernel, trig, frameSize )

object RunningSum extends UGen2Args {
  def ar( in: GE, numSamples: GE = 400 ) : GE = arExp( in, numSamples )
  def kr( in: GE, numSamples: GE = 400 ) : GE = krExp( in, numSamples )

  def rms( in: GE, numSamples: GE = 400 ) : GE =
    (ar( in.squared, numSamples ) * numSamples.reciprocal).sqrt
}
case class RunningSum( rate: Rate, in: UGenIn, numSamples: UGenIn )
extends SingleOutUGen( in, numSamples )
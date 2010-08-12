/*
 *  FFT2.scala
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

import de.sciss.synth.{ GE, MultiOutUGen, Rate, SingleOutUGen, UGenIn }

// continuous convolution in freq domain
object Convolution extends UGen3Args {
   def ar( in: GE, kernel: GE, frameSize: GE ) : GE = arExp( in, kernel, frameSize )
}
/**
 * @see  [[de.sciss.synth.ugen.Convolution2L]]
 * @see  [[de.sciss.synth.ugen.Convolution3]]
 * @see  [[de.sciss.synth.ugen.Convolution2]]
 * @see  [[de.sciss.synth.ugen.StereoConvolution2L]]
 */
case class Convolution( rate: Rate, in: UGenIn, kernel: UGenIn, frameSize: UGenIn )
extends SingleOutUGen( in, kernel, frameSize ) // with SideEffectUGen

object Convolution2 extends UGen4Args {
   def ar( in: GE, kernel: GE, trig: GE = 1, frameSize: GE ) : GE = arExp( in, kernel, trig, frameSize )
}
/**
 * A frequency-domain convolution UGen using a fixed kernel which can be updated
 * by a trigger signal. The delay caused by the convolution when the kernel is a dirac impulse
 * is equal to `frameSize - (controlBlockSize + 1)` (measured august 2010), so for a frameSize
 * of 2048 and a controlBlockSize of 64, this is 1983 sample frames. 
 *
 * @param   in          the realtime input to be convolved
 * @param   kernel      buffer identifier for the fixed kernel, which may be modulated in combination
 *                      with the trigger
 * @param   trigger     updates the kernel on a change from non-positive to positive (<= 0 to >0)
 * @param   frameSize   size of the kernel. this must be a power of two. the FFT calculated internally
 *                      by the UGen has a size of twice this value. The maximum allowed frameSize
 *                      is 65536(?).
 *
 * @see  [[de.sciss.synth.ugen.Convolution2L]]
 * @see  [[de.sciss.synth.ugen.Convolution3]]
 * @see  [[de.sciss.synth.ugen.Convolution]]
 * @see  [[de.sciss.synth.ugen.StereoConvolution2L]]
 */
case class Convolution2( rate: Rate, in: UGenIn, kernel: UGenIn, trig: UGenIn, frameSize: UGenIn )
extends SingleOutUGen( in, kernel, trig, frameSize ) // with SideEffectUGen

// triggered cross-faded convolution in freq domain
object Convolution2L extends UGen5Args {
   def ar( in: GE, kernel: GE, trig: GE, frameSize: GE, fadePeriods: GE = 1 ) : GE =
      arExp( in, kernel, trig, frameSize, fadePeriods )
}
/**
 * @see  [[de.sciss.synth.ugen.Convolution2]]
 * @see  [[de.sciss.synth.ugen.Convolution3]]
 * @see  [[de.sciss.synth.ugen.Convolution]]
 * @see  [[de.sciss.synth.ugen.StereoConvolution2L]]
 */
case class Convolution2L( rate: Rate, in: UGenIn, kernel: UGenIn, trig: UGenIn, frameSize: UGenIn, fadePeriods: UGenIn )
extends SingleOutUGen( in, kernel, trig, frameSize, fadePeriods )

object StereoConvolution2L extends UGen6Args {
   def ar( in: GE, kernelL: GE, kernelR: GE, trig: GE = 1, frameSize: GE, fadePeriods: GE = 1 ) : GE =
      arExp( in, kernelL, kernelR, trig, frameSize, fadePeriods )   
}
/**
 * A frequency domain stereo convolution UGen, capable of performing linear crossfades between kernel updates.
 * When receiving a trigger, there is a linear crossfade between the old kernel the new buffer contents.
 * It operates similar to Convolution2L, however uses two buffers and outputs a stereo signal, resulting
 * in better CPU usage than two discrete instances of Convolution2L as this way one FFT transformation per period
 * is saved.
 *
 * @param   in          the realtime input to be convolved
 * @param   kernelL     buffer identifier for the left channel's fixed kernel, which may be modulated in combination
 *                      with the trigger
 * @param   kernelR     buffer identifier for the right channel's fixed kernel, which may be modulated in combination
 *                      with the trigger
 * @param   trigger     updates the kernel on a change from non-positive to positive (<= 0 to >0), and starts a new
 *                      crossfade from the previous kernel to the new one over the given amount of periods.
 * @param   frameSize   size of each kernel. this must be a power of two. the FFT calculated internally
 *                      by the UGen has a size of twice this value. The maximum allowed frameSize
 *                      is 65536(?).
 * @param   fadePeriods The number of periods over which a crossfade is performed. This must be an integer
 *
 * @see  [[de.sciss.synth.ugen.Convolution2]]
 * @see  [[de.sciss.synth.ugen.Convolution3]]
 * @see  [[de.sciss.synth.ugen.Convolution]]
 * @see  [[de.sciss.synth.ugen.Convolution2L]]
 */
case class StereoConvolution2L( rate: Rate, in: UGenIn, kernelL: UGenIn, kernelR: UGenIn, trig: UGenIn,
                                frameSize: UGenIn, fadePeriods: UGenIn )
extends MultiOutUGen( rate, 2, List( in, kernelL, kernelR, trig, frameSize, fadePeriods ))

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
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

import de.sciss.synth._
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
/*
 *  InOut.scala
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
 *    @version	0.12, 22-Apr-10
 */
object In {
   def ar( bus: GE, numChannels: Int = 1 ) : GE = make( audio,   bus, numChannels )
   def kr( bus: GE, numChannels: Int = 1 ) : GE = make( control, bus, numChannels )
   def ir( bus: GE, numChannels: Int = 1 ) : GE = make( scalar,  bus, numChannels )

   protected def make( rate: Rate, bus: GE, numChannels: Int ) : GE = {
      simplify( for( List( b ) <- expand( bus )) yield this( rate, b, numChannels ))
   }
}
case class In( rate: Rate, bus: UGenIn, numChannels: Int )
extends MultiOutUGen( rate, numChannels, List( bus ))
// with SideEffectUGen

object LocalIn {
   def ar( numChannels: Int = 1 ) : GE = this( audio, numChannels )
   def kr( numChannels: Int = 1 ) : GE = this( control, numChannels )
}
case class LocalIn( rate: Rate, numChannels: Int )
extends MultiOutUGen( rate, numChannels, Nil )
with SideEffectUGen // XXX not sure we need this really

object LagIn {
   def kr( bus: GE, numChannels: Int = 1, lag: GE = 0.1f ) : GE = {
      simplify( for( List( b, l ) <- expand( bus, lag ))
         yield this( control, b, numChannels, l ))
   }
}
case class LagIn( rate: Rate, bus: UGenIn, numChannels: Int, lag: UGenIn )
extends MultiOutUGen( rate, numChannels, List( bus, lag ))
// with SideEffectUGen

object InFeedback {
  def ar( bus: GE, numChannels: Int = 1 ) : GE = {
    simplify( for( List( b ) <- expand( bus )) yield this( b, numChannels ))
  }
}
/**
 * A UGen which reads a signal from an audio bus with a current or one cycle old timestamp
 *
 * Audio buses adhere to the concept of a cycle timestamp, which increases for each audio block
 * calculated. When the various output ugens (`Out`, `OffsetOut`, `XOut`) write data to a bus,
 * they mix it with any data from the current cycle, but overwrite any data from the previous cycle.
 * (`ReplaceOut` overwrites all data regardless.) Thus depending on node order and what synths are
 * writing to the bus, the data on a given bus may be from the current cycle or be one cycle old at
 * the time of reading.
 *
 * `In.ar` checks the timestamp of any data it reads in and zeros any data from the previous
 * cycle (for use within that node; the data remains on the bus). This is fine for audio data,
 * as it avoids feedback, but for control data it is useful to be able to read data from any place
 * in the node order. For this reason `In.kr` also reads data that is older than the current cycle.
 *
 * In some cases one might also want to read audio from a node later in the current node order.
 * This can be achieved with `InFeedback`. It reads from the previous cycle, and hence introduces
 * a '''delay''' of one block size, which by default is 64 sample frames (equal to about 1.45 ms
 * at 44.1 kHz sample rate).
 *
 * @param   bus         the index of the audio bus to read in from.
 * @param   numChannels the number of channels (i.e. adjacent buses) to read in. Since
 *    this is a constant, a change in number of channels of the underlying bus must
 *    be reflected by creating different SynthDefs.
 *
 * @see  [[de.sciss.synth.ugen.In]]
 * @see  [[de.sciss.synth.ugen.LocalIn]]
 * @see  [[de.sciss.synth.ugen.ControlDur]]
 */
case class InFeedback( bus: UGenIn, numChannels: Int )
extends MultiOutUGen( audio, numChannels, List( bus ))
with AudioRated // with SideEffectUGen

object InTrig {
   def kr( bus: GE, numChannels: Int = 1 ) : GE = {
      simplify( for( List( b ) <- expand( bus )) yield this( b, numChannels ))
   }
}
case class InTrig( bus: UGenIn, numChannels: Int )
extends MultiOutUGen( control, numChannels, List( bus ))
with ControlRated // with SideEffectUGen

abstract class AbstractOut {
   def ar( bus: GE, multi: GE ) : GE = make( audio, bus, multi )
   def kr( bus: GE, multi: GE ) : GE = make( control, bus, multi )

   protected def make( rate: Rate, bus: GE, multi: GE ) : GE = {
      val args = bus :: replaceZeroesWithSilence( multi ).outputs.toList
      simplify( for( b :: m <- expand( args: _* ))
         yield this( rate, b, m, SynthGraph.individuate ))
   }

   def apply( rate: Rate, bus: UGenIn, multi: Seq[ UGenIn ], _indiv: Int ) : GE
}

object Out extends AbstractOut {
   def ir( bus: GE, multi: GE ) : GE = make( scalar, bus, multi )
}
case class Out( rate: Rate, bus: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends ZeroOutUGen( (bus :: multi.toList): _* )

object ReplaceOut extends AbstractOut
case class ReplaceOut( rate: Rate, bus: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends ZeroOutUGen( (bus :: multi.toList): _* )

object OffsetOut {
   def ar( bus: GE, multi: GE ) : GE = {
      val args = bus :: replaceZeroesWithSilence( multi ).outputs.toList
      simplify( for( b :: m <- expand( args: _* ))
         yield this( b, m, SynthGraph.individuate ))
   }
}
case class OffsetOut( bus: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends ZeroOutUGen( (bus :: multi.toList): _* ) with AudioRated

object LocalOut {
   def ar( multi: GE ) : GE = make( audio, multi )
   def kr( multi: GE ) : GE = make( control, multi )

   private def make( rate: Rate, multi: GE ) : GE = {
      val ins = replaceZeroesWithSilence( multi ).outputs
      this( rate, ins )
   }
}
case class LocalOut( rate: Rate, multi: Seq[ UGenIn ])
extends ZeroOutUGen( multi: _* )

object XOut {
   def ar( bus: GE, xfade: GE, multi: GE ) : GE = make( audio, bus, xfade, multi )
   def kr( bus: GE, xfade: GE, multi: GE ) : GE = make( control, bus, xfade, multi )

   private def make( rate: Rate, bus: GE, xfade: GE, multi: GE ) : GE = {
      val args = bus :: xfade :: replaceZeroesWithSilence( multi ).outputs.toList
      simplify( for( b :: x :: m <- expand( args: _* ))
         yield this( rate, b, x, m, SynthGraph.individuate ))
   }
}
case class XOut( rate: Rate, bus: UGenIn, xfade: UGenIn, multi: Seq[ UGenIn ], _indiv: Int )
extends ZeroOutUGen( (bus :: xfade :: multi.toList): _* )

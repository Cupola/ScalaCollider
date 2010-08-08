/*
 *  FSinOsc.scala
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

import de.sciss.synth.{ Constant => c, audio, GE, Rate, SingleOutUGen, SynthGraph, UGenIn }
import SynthGraph._

/**
 *	@version	0.11, 09-Dec-09
 */
object FSinOsc extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, iphase: GE = 0 ) : GE = arExp( freq, iphase )
   def kr : GE = kr()
   def kr( freq: GE = 440, iphase: GE = 0 ) : GE = krExp( freq, iphase )
}
case class FSinOsc( rate: Rate, freq: UGenIn, iphase: UGenIn )
extends SingleOutUGen( freq, iphase )

object KlangSpec {
  def fill( n: Int )( thunk: => (GE, GE, GE) ) : List[ KlangSpec ] = {
    List.fill[ (GE, GE, GE) ]( n )( thunk ).map(
      (tup) => KlangSpec( tup._1, tup._2, tup._3 ))
  }

  def tabulate( n: Int )( func: (Int) => (GE, GE, GE) ) : List[ KlangSpec ] = {
    List.tabulate[ (GE, GE, GE) ]( n )( func ).map(
      (tup) => KlangSpec( tup._1, tup._2, tup._3 ))
  }
}
case class KlangSpec( freq: GE, amp: GE = 1, decay: GE = 0 ) {
  def toList = List( freq, amp, decay )
}

object Klang {
  def ar( specs: Seq[ KlangSpec ], freqScale: GE = 1, freqOffset: GE = 0 ) : GE = {
    val exp = expand( (List( freqScale, freqOffset ) ++ specs.flatMap( _.toList )): _* )
    simplify( for( List( fs, fo, sp @ _* ) <- exp) yield this( audio, fs, fo, sp ))
  }
}
case class Klang( rate: Rate, freqScale: UGenIn, freqOffset: UGenIn, specs: Seq[ UGenIn ])
extends SingleOutUGen( (List( freqScale, freqOffset ) ++ specs): _* )

object Klank {
  def ar( specs: Seq[ KlangSpec ], in: GE, freqScale: GE = 1, freqOffset: GE = 0,
          decayScale: GE = 1 ) : GE = {
    val exp = expand( (List( in, freqScale, freqOffset, decayScale ) ++
                      specs.flatMap( _.toList )): _* )
    simplify( for( List( i, fs, fo, ds, sp @ _* ) <- exp) yield this( audio, i, fs, fo, ds, sp ))
  }
}
case class Klank( rate: Rate, in: UGenIn, freqScale: UGenIn, freqOffset: UGenIn,
                  decayScale: UGenIn, specs: Seq[ UGenIn ])
extends SingleOutUGen( (List( in, freqScale, freqOffset, decayScale ) ++ specs): _* )

// DynKlank XXX missing
// DynKlang XXX missing

object Blip extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, numHarm: GE = 200 ) : GE = arExp( freq, numHarm )
   def kr : GE = kr()
   def kr( freq: GE = 440, numHarm: GE = 200 ) : GE = krExp( freq, numHarm )
}
case class Blip( rate: Rate, freq: UGenIn, numHarm: UGenIn )
extends SingleOutUGen( freq, numHarm )

object Saw extends UGen1Args {
   def ar : GE = ar()
   def ar( freq: GE = 440 ) : GE = arExp( freq )
   def kr : GE = kr()
   def kr( freq: GE = 440 ) : GE = krExp( freq )
}
case class Saw( rate: Rate, freq: UGenIn )
extends SingleOutUGen( freq )

object Pulse extends UGen2Args {
   def ar : GE = ar()
   def ar( freq: GE = 440, width: GE = 0.5f ) : GE = arExp( freq, width )
   def kr : GE = kr()
   def kr( freq: GE = 440, width: GE = 0.5f ) : GE = krExp( freq, width )
}
case class Pulse( rate: Rate, freq: UGenIn, width: UGenIn )
extends SingleOutUGen( freq, width )

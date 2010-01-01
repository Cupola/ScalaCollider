/*
 *  Noise.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

/**
 *	@version	0.11, 09-Dec-09
 */
object RandSeed {
	def kr( trig: GE, seed: GE = 56789 ) : GE = {
    	UGen.multiNew( "RandSeed", control, Nil, List( trig, seed ))
	}

	def ir( trig: GE, seed: GE = 56789 ) : GE = {
    	UGen.multiNew( "RandSeed", scalar, Nil, List( trig, seed ))
	}
}

object RandID {
	def kr( id: GE ) : GE = {
    	UGen.multiNew( "RandID", control, Nil, List( id ))
	}

	def ir( id: GE ) : GE = {
    	UGen.multiNew( "RandID", scalar, Nil, List( id ))
	}
}

object Rand {
	def apply( lo: GE = 0, hi: GE = 1 ) : GE = {
    	UGen.multiNew( "Rand", scalar, List( scalar ), List( lo, hi ))
	}
}

object IRand {
	def apply( lo: GE = 0, hi: GE = 127 ) : GE = {
    	UGen.multiNew( "IRand", scalar, List( scalar ), List( lo, hi ))
	}
}

object TRand {
	def ar( lo: GE = 0, hi: GE = 1, trig: GE  ) : GE = {
    	UGen.multiNew( "TRand", audio, List( audio ), List( lo, hi, trig ))
	}
	
	def kr( lo: GE = 0, hi: GE = 1, trig: GE  ) : GE = {
    	UGen.multiNew( "TRand", control, List( control ), List( lo, hi, trig ))
	}
}

object TIRand {
	def ar( lo: GE = 0, hi: GE = 127, trig: GE  ) : GE = {
    	UGen.multiNew( "TIRand", audio, List( audio ), List( lo, hi, trig ))
	}

	def kr( lo: GE = 0, hi: GE = 127, trig: GE  ) : GE = {
    	UGen.multiNew( "TIRand", control, List( control ), List( lo, hi, trig ))
	}
}

object LinRand {
	def apply( lo: GE = 0, hi: GE = 127, minmax: GE = 0 ) : GE = {
    	UGen.multiNew( "LinRand", scalar, List( scalar ), List( lo, hi, minmax ))
	}
}

object NRand {
	def apply( lo: GE = 0, hi: GE = 127, n: GE = 0 ) : GE = {
    	UGen.multiNew( "NRand", scalar, List( scalar ), List( lo, hi, n ))
	}
}

object ExpRand {
	def apply( lo: GE = 0.01f, hi: GE = 1 ) : GE = {
    	UGen.multiNew( "ExpRand", scalar, List( scalar ), List( lo, hi ))
	}
}

object TExpRand {
	def ar( lo: GE = 0.01f, hi: GE = 1, trig: GE ) : GE = {
    	UGen.multiNew( "TExpRand", audio, List( audio ), List( lo, hi, trig ))
	}

	def kr( lo: GE = 0.01f, hi: GE = 1, trig: GE ) : GE = {
    	UGen.multiNew( "TExpRand", control, List( control ), List( lo, hi, trig ))
	}
}

object CoinGate {
	def ar( prob: GE, in: GE ) : GE = {
    	UGen.multiNew( "CoinGate", audio, List( audio ), List( prob, in ))
	}

	def kr( prob: GE, in: GE ) : GE = {
    	UGen.multiNew( "CoinGate", control, List( control ), List( prob, in ))
	}
}

// TWindex XXX missing

trait NoiseUGen {
//  type noiseType <: UGen
  def apply( rate: Rate ) : SingleOutUGen

  def ar: SingleOutUGen = this( audio )
  def kr: SingleOutUGen = this( control )

  private def make( make1: => SingleOutUGen, mul: GE ) : GE = {
    val zipped = List.fill[ UGenIn ]( mul.numOutputs )( make1 ).zip( mul.toUGenIns )
    seq( zipped.flatMap( p => (p._1 * p._2).toUGenIns ): _* )
  }

  def ar( mul: GE ): GE = make( ar, mul )
  def kr( mul: GE ): GE = make( kr, mul )
}

object WhiteNoise extends NoiseUGen { type noiseType = WhiteNoise }
case class WhiteNoise( rate: Rate ) extends SingleOutUGen()

object BrownNoise extends NoiseUGen { type noiseType = BrownNoise }
case class BrownNoise( rate: Rate ) extends SingleOutUGen()

object PinkNoise extends NoiseUGen { type noiseType = PinkNoise }
case class PinkNoise( rate: Rate ) extends SingleOutUGen()

object ClipNoise extends NoiseUGen { type noiseType = ClipNoise }
case class ClipNoise( rate: Rate ) extends SingleOutUGen()

object GrayNoise extends NoiseUGen { type noiseType = GrayNoise }
case class GrayNoise( rate: Rate ) extends SingleOutUGen()

object Crackle {
	def ar( chaosParam: GE = 1.5f, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Crackle", audio, List( audio ), List( chaosParam )).madd( mul, add )
	}

	def kr( chaosParam: GE = 1.5f, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Crackle", control, List( control ), List( chaosParam )).madd( mul, add )
	}
}

object Logistic {
	def ar( chaosParam: GE = 3, freq: GE = 1000, init: GE = 0.5f, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Logistic", audio, List( audio ), List( chaosParam, freq, init )).madd( mul, add )
	}

	def kr( chaosParam: GE = 3, freq: GE = 1000, init: GE = 0.5f, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Logistic", control, List( control ), List( chaosParam, freq, init )).madd( mul, add )
	}
}

object LFNoise0 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise0", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise0", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFNoise1 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise1", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise1", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFNoise2 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise2", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFNoise2", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFClipNoise {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFClipNoise", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFClipNoise", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFDNoise0 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise0", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise0", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFDNoise1 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise1", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise1", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFDNoise3 {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise3", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDNoise3", control, List( control ), List( freq )).madd( mul, add )
	}
}

object LFDClipNoise {
	def ar( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDClipNoise", audio, List( audio ), List( freq )).madd( mul, add )
	}

	def kr( freq: GE = 500, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "LFDClipNoise", control, List( control ), List( freq )).madd( mul, add )
	}
}

object Hasher {
	def ar( in: GE, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Hasher", audio, List( audio ), List( in )).madd( mul, add )
	}

	def kr( in: GE, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Hasher", control, List( control ), List( in )).madd( mul, add )
	}
}

object MantissaMask {
	def ar( in: GE, bits: GE = 3, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "MantissaMask", audio, List( audio ), List( in, bits )).madd( mul, add )
	}

	def kr( in: GE, bits: GE = 3, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "MantissaMask", control, List( control ), List( in, bits )).madd( mul, add )
	}
}

object Dust {
	def ar( density: GE = 1, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Dust", audio, List( audio ), List( density )).madd( mul, add )
	}

	def kr( density: GE = 1, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Dust", control, List( control ), List( density )).madd( mul, add )
	}
}

object Dust2 {
	def ar( density: GE = 1, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Dust2", audio, List( audio ), List( density )).madd( mul, add )
	}

	def kr( density: GE = 1, mul: GE = 1, add: GE = 0 ) : GE = {
    	UGen.multiNew( "Dust2", control, List( control ), List( density )).madd( mul, add )
	}
}

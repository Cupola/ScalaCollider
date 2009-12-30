/*
 *  Filter.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
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
 * 	@version	0.11, 16-Jun-09
 */
object Resonz {	
	def ar( in: GE, freq: GE = 440, bwr: GE = 1 ) : GE = {
    	UGen.multiNew( "Resonz", audio, List( audio ), List( in, freq, bwr ))
	}

	def kr( in: GE, freq: GE = 440, bwr: GE = 1 ) : GE = {
		UGen.multiNew( "Resonz", control, List( control ), List( in, freq, bwr ))
	}
}

object OnePole {	
	def ar( in: GE, coeff: GE = 0.5f ) : GE = {
    	UGen.multiNew( "OnePole", audio, List( audio ), List( in, coeff ))
	}

	def kr( in: GE, coeff: GE = 0.5f ) : GE = {
		UGen.multiNew( "OnePole", control, List( control ), List( in, coeff ))
	}
}

object OneZero {	
	def ar( in: GE, coeff: GE = 0.5f ) : GE = {
    	UGen.multiNew( "OneZero", audio, List( audio ), List( in, coeff ))
	}

	def kr( in: GE, coeff: GE = 0.5f ) : GE = {
		UGen.multiNew( "OneZero", control, List( control ), List( in, coeff ))
	}
}

object TwoPole {	
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
    	UGen.multiNew( "TwoPole", audio, List( audio ), List( in, freq, radius ))
	}

	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
		UGen.multiNew( "TwoPole", control, List( control ), List( in, freq, radius ))
	}
}

object TwoZero {	
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
    	UGen.multiNew( "TwoZero", audio, List( audio ), List( in, freq, radius ))
	}

	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
		UGen.multiNew( "TwoZero", control, List( control ), List( in, freq, radius ))
	}
}

object APF {	
	def ar( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
    	UGen.multiNew( "APF", audio, List( audio ), List( in, freq, radius ))
	}

	def kr( in: GE, freq: GE = 440, radius: GE = 0.8f ) : GE = {
		UGen.multiNew( "APF", control, List( control ), List( in, freq, radius ))
	}
}

object Integrator {	
	def ar( in: GE, coeff: GE = 1 ) : GE = {
    	UGen.multiNew( "Integrator", audio, List( audio ), List( in, coeff ))
	}

	def kr( in: GE, coeff: GE = 1 ) : GE = {
		UGen.multiNew( "Integrator", control, List( control ), List( in, coeff ))
	}
}

object Decay {	
	def ar( in: GE, decayTime: GE = 1 ) : GE = {
    	UGen.multiNew( "Decay", audio, List( audio ), List( in, decayTime ))
	}

	def kr( in: GE, decayTime: GE = 1 ) : GE = {
		UGen.multiNew( "Decay", control, List( control ), List( in, decayTime ))
	}
}

object Decay2 {	
	def ar( in: GE, attackTime: GE = 0.01f, decayTime: GE = 1 ) : GE = {
    	UGen.multiNew( "Decay2", audio, List( audio ), List( in, attackTime, decayTime ))
	}

	def kr( in: GE, attackTime: GE = 0.01f, decayTime: GE = 1 ) : GE = {
		UGen.multiNew( "Decay2", control, List( control ), List( in, attackTime, decayTime ))
	}
}

object Lag {	
	def ar( in: GE, lagTime: GE = 0.1f ) : GE = {
    	UGen.multiNew( "Lag", audio, List( audio ), List( in, lagTime ))
	}
  
	def kr( in: GE, lagTime: GE = 0.1f ) : GE = {
		UGen.multiNew( "Lag", control, List( control ), List( in, lagTime ))
	}
}

object Lag2 {
	def ar( in: GE, lagTime: GE = 0.1f ) : GE = {
    	UGen.multiNew( "Lag2", audio, List( audio ), List( in, lagTime ))
	}
  
	def kr( in: GE, lagTime: GE = 0.1f ) : GE = {
		UGen.multiNew( "Lag2", control, List( control ), List( in, lagTime ))
	}
}

object Lag3 {
	def ar( in: GE, lagTime: GE = 0.1f ) : GE = {
		UGen.multiNew( "Lag3", audio, List( audio ), List( in, lagTime ))
	}

	def kr( in: GE, lagTime: GE = 0.1f ) : GE = {
		UGen.multiNew( "Lag3", control, List( control ), List( in, lagTime ))
	}
}

object Ramp {
	def ar( in: GE, lagTime: GE = 0.1f ) : GE = {
    	UGen.multiNew( "Ramp", audio, List( audio ), List( in, lagTime ))
	}

	def kr( in: GE, lagTime: GE = 0.1f ) : GE = {
		UGen.multiNew( "Ramp", control, List( control ), List( in, lagTime ))
	}
}

object LagUD {
	def ar( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
    	UGen.multiNew( "LagUD", audio, List( audio ), List( in, lagTimeU, lagTimeD ))
	}
  
	def kr( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
		UGen.multiNew( "LagUD", control, List( control ), List( in, lagTimeU, lagTimeD ))
	}
}

object LagUD2 {
	def ar( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
    	UGen.multiNew( "LagUD2", audio, List( audio ), List( in, lagTimeU, lagTimeD ))
	}
  
	def kr( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
		UGen.multiNew( "LagUD2", control, List( control ), List( in, lagTimeU, lagTimeD ))
	}
}

object LagUD3 {
	def ar( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
    	UGen.multiNew( "LagUD3", audio, List( audio ), List( in, lagTimeU, lagTimeD ))
	}
  
	def kr( in: GE, lagTimeU: GE = 0.1f, lagTimeD: GE = 0.1f ) : GE = {
		UGen.multiNew( "LagUD3", control, List( control ), List( in, lagTimeU, lagTimeD ))
	}
}

object LeakDC {	
	def ar( in: GE, coeff: GE = 0.995f ) : GE = {
    	UGen.multiNew( "LeakDC", audio, List( audio ), List( in, coeff ))
	}

	def kr( in: GE, coeff: GE = 0.9f ) : GE = {
		UGen.multiNew( "LeakDC", control, List( control ), List( in, coeff ))
	}
}

object RLPF {
  def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
    simplify( for( List( i, f, q ) <- expand( in, freq, rq )) yield this( audio, i, f, q ))
  }

  def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
    simplify( for( List( i, f, q ) <- expand( in, freq, rq )) yield this( control, i, f, q ))
  }
}

case class RLPF( override rate: Rate, in: UGenInput, freq: UGenInput, rq: UGenInput )
extends SingleOutUGen( "RLPF", rate, rate, List( in, freq, rq ))

object RHPF {	
	def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
    	UGen.multiNew( "RHPF", audio, List( audio ), List( in, freq, rq ))
	}
  
	def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
		UGen.multiNew( "RHPF", control, List( control ), List( in, freq, rq ))
	}
}

object LPF {
	def ar( in: GE, freq: GE = 440 ) : GE = {
    	UGen.multiNew( "LPF", audio, List( audio ), List( in, freq ))
	}
  
	def kr( in: GE, freq: GE = 440 ) : GE = {
    	UGen.multiNew( "LPF", control, List( control ), List( in, freq ))
	}
}

object HPF {
	def ar( in: GE, freq: GE = 440 ) : GE = {
    	UGen.multiNew( "HPF", audio, List( audio ), List( in, freq ))
	}
  
	def kr( in: GE, freq: GE = 440 ) : GE = {
    	UGen.multiNew( "HPF", control, List( control ), List( in, freq ))
	}
}

object BPF {	
	def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
    	UGen.multiNew( "BPF", audio, List( audio ), List( in, freq, rq ))
	}
  
	def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
		UGen.multiNew( "BPF", control, List( control ), List( in, freq, rq ))
	}
}

object BRF {	
	def ar( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
    	UGen.multiNew( "BRF", audio, List( audio ), List( in, freq, rq ))
	}
  
	def kr( in: GE, freq: GE = 440, rq: GE = 1 ) : GE = {
		UGen.multiNew( "BRF", control, List( control ), List( in, freq, rq ))
	}
}

object MidEQ {	
	def ar( in: GE, freq: GE = 440, rq: GE = 1, db: GE = 0 ) : GE = {
    	UGen.multiNew( "MidEQ", audio, List( audio ), List( in, freq, rq, db ))
	}
  
	def kr( in: GE, freq: GE = 440, rq: GE = 1, db: GE = 0 ) : GE = {
		UGen.multiNew( "MidEQ", control, List( control ), List( in, freq, rq, db ))
	}
}

object LPZ1 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "LPZ1", audio, List( audio ), List( in ))
	}

	def kr( in: GE ) : GE = {
    	UGen.multiNew( "LPZ1", control, List( control ), List( in ))
	}
}

object HPZ1 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "HPZ1", audio, List( audio ), List( in ))
	}
  
	def kr( in: GE ) : GE = {
    	UGen.multiNew( "HPZ1", control, List( control ), List( in ))
	}
}

object Slope {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "Slope", audio, List( audio ), List( in ))
	}

	def kr( in: GE ) : GE = {
    	UGen.multiNew( "Slope", control, List( control ), List( in ))
	}
}

object LPZ2 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "LPZ2", audio, List( audio ), List( in ))
	}
  
	def kr( in: GE ) : GE = {
    	UGen.multiNew( "LPZ2", control, List( control ), List( in ))
	}
}

object HPZ2 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "HPZ2", audio, List( audio ), List( in ))
	}

	def kr( in: GE ) : GE = {
    	UGen.multiNew( "HPZ2", control, List( control ), List( in ))
	}
}

object BPZ2 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "BPZ2", audio, List( audio ), List( in ))
	}
  
	def kr( in: GE ) : GE = {
    	UGen.multiNew( "BPZ2", control, List( control ), List( in ))
	}
}

object BRZ2 {
	def ar( in: GE ) : GE = {
    	UGen.multiNew( "BRZ2", audio, List( audio ), List( in ))
	}
  
	def kr( in: GE ) : GE = {
    	UGen.multiNew( "BRZ2", control, List( control ), List( in ))
	}
}

// Median XXX
// Slew XXX
// FOS XXX
// SOS XXX
// Ringz XXX
// Formlet XXX
// DetectSilence XXX

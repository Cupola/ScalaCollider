/*
 *  Osc.scala
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
 *  @author   Hanns Holger Rutz
 *  @version  0.11, 29-Dec-09
 */
object Osc {	
  def ar( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
    UGen.multiNew( "Osc", audio, List( audio ), List( bufNum, freq, phase ))
  }

  def kr( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
    UGen.multiNew( "Osc", control, List( control ), List( bufNum, freq, phase ))
  }
}

// class Osc private ( r: Rate, val bufNum: UGenInput, val freq: UGenInput, val phase: UGenInput )
// extends UGen( "Osc", r, List( r ), List( bufNum, freq, phase )) {
// }

object SinOsc {
  def ar( freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, phase )) yield this( audio, f, p ))
  }

  def kr( freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, phase )) yield this( control, f, p ))
  }
}

case class SinOsc( rate: Rate, freq: UGenInput, phase: UGenInput )
extends SingleOutUGen( freq, phase )

object SinOscFB {	
  def ar : GE = ar( Constant( 440f ), Constants.zero )
  def ar( freq: GE ) : GE = ar( freq, Constants.zero )
  
  def ar( freq: GE, feedback: GE ) : GE = {
    UGen.multiNew( "SinOscFB", audio, List( audio ), List( freq, feedback ))
  }
  
  def ar( freq: GE, feedback: GE, mul: GE ) : GE = {
    ar( freq, feedback ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, feedback: GE, mul: GE, add: GE ) : GE = {
    ar( freq, feedback ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero )
  def kr( freq: GE ) : GE = kr( freq, Constants.zero )

  def kr( freq: GE, feedback: GE ) : GE = {
    UGen.multiNew( "SinOscFB", control, List( control ), List( freq, feedback ))
  }

  def kr( freq: GE, feedback: GE, mul: GE ) : GE = {
    kr( freq, feedback ).madd( mul, Constants.zero )
  }
  def kr( freq: GE, feedback: GE, mul: GE, add: GE ) : GE = {
    kr( freq, feedback ).madd( mul, add )
  }
}

object OscN {	
  def ar( bufNum: GE ) : GE = ar( bufNum, Constant( 440f ), Constants.zero )
  def ar( bufNum: GE, freq: GE ) : GE = ar( bufNum, freq, Constants.zero )
  
  def ar( bufNum: GE, freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "OscN", audio, List( audio ), List( bufNum, freq, phase ))
  }
  
  def ar( bufNum: GE, freq: GE, phase: GE, mul: GE ) : GE = {
    ar( bufNum, freq, phase ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, freq, phase ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constant( 440f ), Constants.zero )
  def kr( bufNum: GE, freq: GE ) : GE = kr( bufNum, freq, Constants.zero )

  def kr( bufNum: GE, freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "OscN", control, List( control ), List( bufNum, freq, phase ))
  }

  def kr( bufNum: GE, freq: GE, phase: GE, mul: GE ) : GE = {
    kr( bufNum, freq, phase ).madd( mul, Constants.zero )
  }
  def kr( bufNum: GE, freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, freq, phase ).madd( mul, add )
  }
}

object VOsc {
  def ar( bufPos: GE ) : GE = ar( bufPos, Constant( 440f ), Constants.zero )
  def ar( bufPos: GE, freq: GE ) : GE = ar( bufPos, freq, Constants.zero )
  
  def ar( bufPos: GE, freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "VOsc", audio, List( audio ), List( bufPos, freq, phase ))
  }
  
  def ar( bufPos: GE, freq: GE, phase: GE, mul: GE ) : GE = {
    ar( bufPos, freq, phase ).madd( mul, Constants.zero ) 
  }
  def ar( bufPos: GE, freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    ar( bufPos, freq, phase ).madd( mul, add ) 
  }

  def kr( bufPos: GE ) : GE = kr( bufPos, Constant( 440f ), Constants.zero )
  def kr( bufPos: GE, freq: GE ) : GE = kr( bufPos, freq, Constants.zero )

  def kr( bufPos: GE, freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "VOsc", control, List( control ), List( bufPos, freq, phase ))
  }

  def kr( bufPos: GE, freq: GE, phase: GE, mul: GE ) : GE = {
    kr( bufPos, freq, phase ).madd( mul, Constants.zero )
  }
  def kr( bufPos: GE, freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    kr( bufPos, freq, phase ).madd( mul, add )
  }
}

object VOsc3 {
  def ar( bufPos: GE ) : GE = ar( bufPos, Constant( 110f ), Constant( 220f ), Constant( 440f ))
  def ar( bufPos: GE, freq1: GE, freq2: GE, freq3: GE ) : GE = {
    UGen.multiNew( "VOsc3", audio, List( audio ), List( bufPos, freq1, freq2, freq3 ))
  }
  
  def ar( bufPos: GE, freq1: GE, freq2: GE, freq3: GE, mul: GE ) : GE = {
    ar( bufPos, freq1, freq2, freq3 ).madd( mul, Constants.zero ) 
  }
  def ar( bufPos: GE, freq1: GE, freq2: GE, freq3: GE, mul: GE, add: GE ) : GE = {
    ar( bufPos, freq1, freq2, freq3 ).madd( mul, add ) 
  }

  def kr( bufPos: GE ) : GE = kr( bufPos, Constant( 110f ), Constant( 220f ), Constant( 440f ))
  def kr( bufPos: GE, freq1: GE, freq2: GE, freq3: GE ) : GE = {
    UGen.multiNew( "VOsc3", control, List( control ), List( bufPos, freq1, freq2, freq3 ))
  }
  
  def kr( bufPos: GE, freq1: GE, freq2: GE, freq3: GE, mul: GE ) : GE = {
    kr( bufPos, freq1, freq2, freq3 ).madd( mul, Constants.zero ) 
  }
  def kr( bufPos: GE, freq1: GE, freq2: GE, freq3: GE, mul: GE, add: GE ) : GE = {
    kr( bufPos, freq1, freq2, freq3 ).madd( mul, add ) 
  }
}

object COsc {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constant( 440f ), Constant( 0.5f ))
  def ar( bufNum: GE, freq: GE ) : GE = ar( bufNum, freq, Constant( 0.5f ))
  
  def ar( bufNum: GE, freq: GE, beats: GE ) : GE = {
    UGen.multiNew( "COsc", audio, List( audio ), List( bufNum, freq, beats ))
  }
  
  def ar( bufNum: GE, freq: GE, beats: GE, mul: GE ) : GE = {
    ar( bufNum, freq, beats ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, freq: GE, beats: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, freq, beats ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constant( 440f ), Constant( 0.5f ))
  def kr( bufNum: GE, freq: GE ) : GE = kr( bufNum, freq, Constant( 0.5f ))

  def kr( bufNum: GE, freq: GE, beats: GE ) : GE = {
    UGen.multiNew( "COsc", control, List( control ), List( bufNum, freq, beats ))
  }

  def kr( bufNum: GE, freq: GE, beats: GE, mul: GE ) : GE = {
    kr( bufNum, freq, beats ).madd( mul, Constants.zero )
  }
  def kr( bufNum: GE, freq: GE, beats: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, freq, beats ).madd( mul, add )
  }
}

object Formant {	
  def ar : GE = ar( Constant( 440f ), Constant( 1760f ), Constant( 880f ))
  def ar( fundFreq: GE ) : GE = ar( fundFreq, Constant( 1760f ), Constant( 880f ))
  def ar( fundFreq: GE, formFreq: GE ) : GE = ar( fundFreq, formFreq, Constant( 880f ))
  
  def ar( fundFreq: GE, formFreq: GE, bwFreq: GE ) : GE = {
    UGen.multiNew( "Formant", audio, List( audio ), List( fundFreq, formFreq, bwFreq ))
  }
  
  def ar( fundFreq: GE, formFreq: GE, bwFreq: GE, mul: GE ) : GE = {
    ar( fundFreq, formFreq, bwFreq ).madd( mul, Constants.zero ) 
  }
  def ar( fundFreq: GE, formFreq: GE, bwFreq: GE, mul: GE, add: GE ) : GE = {
    ar( fundFreq, formFreq, bwFreq ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constant( 1760f ), Constant( 880f ))
  def kr( fundFreq: GE ) : GE = kr( fundFreq, Constant( 1760f ), Constant( 880f ))
  def kr( fundFreq: GE, formFreq: GE ) : GE = kr( fundFreq, formFreq, Constant( 880f ))
  
  def kr( fundFreq: GE, formFreq: GE, bwFreq: GE ) : GE = {
    UGen.multiNew( "Formant", control, List( control ), List( fundFreq, formFreq, bwFreq ))
  }
  
  def kr( fundFreq: GE, formFreq: GE, bwFreq: GE, mul: GE ) : GE = {
    kr( fundFreq, formFreq, bwFreq ).madd( mul, Constants.zero ) 
  }
  def kr( fundFreq: GE, formFreq: GE, bwFreq: GE, mul: GE, add: GE ) : GE = {
    kr( fundFreq, formFreq, bwFreq ).madd( mul, add ) 
  }
}

object LFSaw {	
  def ar( freq: GE = 440, iphase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, iphase )) yield this( audio, f, p ))
  }
  
  def kr( freq: GE = 440, iphase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, iphase )) yield this( control, f, p ))
  }
}

case class LFSaw( rate: Rate, freq: UGenInput, iphase: UGenInput )
extends SingleOutUGen( freq, iphase )

object LFPar {	
  def ar : GE = ar( Constant( 440f ), Constants.zero )
  def ar( freq: GE ) : GE = ar( freq, Constants.zero )
  
  def ar( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFPar", audio, List( audio ), List( freq, iphase ))
  }
  
  def ar( freq: GE, iphase: GE, mul: GE ) : GE = {
    ar( freq, iphase ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    ar( freq, iphase ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero )
  def kr( freq: GE ) : GE = kr( freq, Constants.zero )

  def kr( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFPar", control, List( control ), List( freq, iphase ))
  }

  def kr( freq: GE, iphase: GE, mul: GE ) : GE = {
    kr( freq, iphase ).madd( mul, Constants.zero )
  }
  def kr( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    kr( freq, iphase ).madd( mul, add )
  }
}

object LFCub {	
  def ar : GE = ar( Constant( 440f ), Constants.zero )
  def ar( freq: GE ) : GE = ar( freq, Constants.zero )
  
  def ar( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFCub", audio, List( audio ), List( freq, iphase ))
  }
  
  def ar( freq: GE, iphase: GE, mul: GE ) : GE = {
    ar( freq, iphase ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    ar( freq, iphase ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero )
  def kr( freq: GE ) : GE = kr( freq, Constants.zero )

  def kr( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFCub", control, List( control ), List( freq, iphase ))
  }

  def kr( freq: GE, iphase: GE, mul: GE ) : GE = {
    kr( freq, iphase ).madd( mul, Constants.zero )
  }
  def kr( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    kr( freq, iphase ).madd( mul, add )
  }
}

object LFTri {	
  def ar : GE = ar( Constant( 440f ), Constants.zero )
  def ar( freq: GE ) : GE = ar( freq, Constants.zero )
  
  def ar( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFTri", audio, List( audio ), List( freq, iphase ))
  }
  
  def ar( freq: GE, iphase: GE, mul: GE ) : GE = {
    ar( freq, iphase ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    ar( freq, iphase ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero )
  def kr( freq: GE ) : GE = kr( freq, Constants.zero )

  def kr( freq: GE, iphase: GE ) : GE = {
    UGen.multiNew( "LFTri", control, List( control ), List( freq, iphase ))
  }

  def kr( freq: GE, iphase: GE, mul: GE ) : GE = {
    kr( freq, iphase ).madd( mul, Constants.zero )
  }
  def kr( freq: GE, iphase: GE, mul: GE, add: GE ) : GE = {
    kr( freq, iphase ).madd( mul, add )
  }
}

object LFPulse {	
  def ar : GE = ar( Constant( 440f ), Constants.zero, Constant( 0.5f ))
  def ar( freq: GE ) : GE = ar( freq, Constants.zero, Constant( 0.5f ) )
  def ar( freq: GE, iphase: GE ) : GE = ar( freq, iphase, Constant( 0.5f ) )
  
  def ar( freq: GE, iphase: GE, width: GE ) : GE = {
    UGen.multiNew( "LFPulse", audio, List( audio ), List( freq, iphase, width ))
  }
  
  def ar( freq: GE, iphase: GE, width: GE, mul: GE ) : GE = {
    ar( freq, iphase, width ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, iphase: GE, width: GE, mul: GE, add: GE ) : GE = {
    ar( freq, iphase, width ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero, Constant( 0.5f ))
  def kr( freq: GE ) : GE = kr( freq, Constants.zero, Constant( 0.5f ) )
  def kr( freq: GE, iphase: GE ) : GE = kr( freq, iphase, Constant( 0.5f ) )
  
  def kr( freq: GE, iphase: GE, width: GE ) : GE = {
    UGen.multiNew( "LFPulse", control, List( control ), List( freq, iphase, width ))
  }
  
  def kr( freq: GE, iphase: GE, width: GE, mul: GE ) : GE = {
    kr( freq, iphase, width ).madd( mul, Constants.zero ) 
  }
  def kr( freq: GE, iphase: GE, width: GE, mul: GE, add: GE ) : GE = {
    kr( freq, iphase, width ).madd( mul, add ) 
  }
}

object VarSaw {	
  def ar : GE = ar( Constant( 440f ), Constants.zero, Constant( 0.5f ))
  def ar( freq: GE ) : GE = ar( freq, Constants.zero, Constant( 0.5f ) )
  def ar( freq: GE, iphase: GE ) : GE = ar( freq, iphase, Constant( 0.5f ) )
  
  def ar( freq: GE, iphase: GE, width: GE ) : GE = {
    UGen.multiNew( "VarSaw", audio, List( audio ), List( freq, iphase, width ))
  }
  
  def ar( freq: GE, iphase: GE, width: GE, mul: GE ) : GE = {
    ar( freq, iphase, width ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, iphase: GE, width: GE, mul: GE, add: GE ) : GE = {
    ar( freq, iphase, width ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero, Constant( 0.5f ))
  def kr( freq: GE ) : GE = kr( freq, Constants.zero, Constant( 0.5f ) )
  def kr( freq: GE, iphase: GE ) : GE = kr( freq, iphase, Constant( 0.5f ) )
  
  def kr( freq: GE, iphase: GE, width: GE ) : GE = {
    UGen.multiNew( "VarSaw", control, List( control ), List( freq, iphase, width ))
  }
  
  def kr( freq: GE, iphase: GE, width: GE, mul: GE ) : GE = {
    kr( freq, iphase, width ).madd( mul, Constants.zero ) 
  }
  def kr( freq: GE, iphase: GE, width: GE, mul: GE, add: GE ) : GE = {
    kr( freq, iphase, width ).madd( mul, add ) 
  }
}

object Impulse {	
  def ar : GE = ar( Constant( 440f ), Constants.zero )
  def ar( freq: GE ) : GE = ar( freq, Constants.zero )
  
  def ar( freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "Impulse", audio, List( audio ), List( freq, phase ))
  }
  
  def ar( freq: GE, phase: GE, mul: GE ) : GE = {
    ar( freq, phase ).madd( mul, Constants.zero ) 
  }
  def ar( freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    ar( freq, phase ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constants.zero )
  def kr( freq: GE ) : GE = kr( freq, Constants.zero )

  def kr( freq: GE, phase: GE ) : GE = {
    UGen.multiNew( "Impulse", control, List( control ), List( freq, phase ))
  }

  def kr( freq: GE, phase: GE, mul: GE ) : GE = {
    kr( freq, phase ).madd( mul, Constants.zero )
  }
  def kr( freq: GE, phase: GE, mul: GE, add: GE ) : GE = {
    kr( freq, phase ).madd( mul, add )
  }
}

object SyncSaw {	
  def ar : GE = ar( Constant( 440f ), Constant( 440f ))
  def ar( syncFreq: GE ) : GE = ar( syncFreq, Constant( 440f ))
  
  def ar( syncFreq: GE, sawFreq: GE ) : GE = {
    UGen.multiNew( "SyncSaw", audio, List( audio ), List( syncFreq, sawFreq ))
  }
  
  def ar( syncFreq: GE, sawFreq: GE, mul: GE ) : GE = {
    ar( syncFreq, sawFreq ).madd( mul, Constants.zero ) 
  }
  def ar( syncFreq: GE, sawFreq: GE, mul: GE, add: GE ) : GE = {
    ar( syncFreq, sawFreq ).madd( mul, add ) 
  }

  def kr : GE = kr( Constant( 440f ), Constant( 440f ))
  def kr( syncFreq: GE ) : GE = kr( syncFreq, Constant( 440f ))

  def kr( syncFreq: GE, sawFreq: GE ) : GE = {
    UGen.multiNew( "SyncSaw", control, List( control ), List( syncFreq, sawFreq ))
  }

  def kr( syncFreq: GE, sawFreq: GE, mul: GE ) : GE = {
    kr( syncFreq, sawFreq ).madd( mul, Constants.zero )
  }
  def kr( syncFreq: GE, sawFreq: GE, mul: GE, add: GE ) : GE = {
    kr( syncFreq, sawFreq ).madd( mul, add )
  }
}

object Index {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constants.zero )
  
  def ar( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "Index", audio, List( audio ), List( bufNum, in ))
  }
  
  def ar( bufNum: GE, in: GE, mul: GE ) : GE = {
    ar( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, in ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constants.zero )
  
  def kr( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "Index", scalar, List( scalar ), List( bufNum, in ))
  }
  
  def kr( bufNum: GE, in: GE, mul: GE ) : GE = {
    kr( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def kr( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, in ).madd( mul, add ) 
  }
}

object WrapIndex {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constants.zero )
  
  def ar( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "WrapIndex", audio, List( audio ), List( bufNum, in ))
  }
  
  def ar( bufNum: GE, in: GE, mul: GE ) : GE = {
    ar( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, in ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constants.zero )
  
  def kr( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "WrapIndex", scalar, List( scalar ), List( bufNum, in ))
  }
  
  def kr( bufNum: GE, in: GE, mul: GE ) : GE = {
    kr( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def kr( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, in ).madd( mul, add ) 
  }
}

object IndexInBetween {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constants.zero )
  
  def ar( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "IndexInBetween", audio, List( audio ), List( bufNum, in ))
  }
  
  def ar( bufNum: GE, in: GE, mul: GE ) : GE = {
    ar( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, in ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constants.zero )
  
  def kr( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "IndexInBetween", scalar, List( scalar ), List( bufNum, in ))
  }
  
  def kr( bufNum: GE, in: GE, mul: GE ) : GE = {
    kr( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def kr( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, in ).madd( mul, add ) 
  }
}

object DetectIndex {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constants.zero )
  
  def ar( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "DetectIndex", audio, List( audio ), List( bufNum, in ))
  }
  
  def ar( bufNum: GE, in: GE, mul: GE ) : GE = {
    ar( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, in ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constants.zero )
  
  def kr( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "DetectIndex", scalar, List( scalar ), List( bufNum, in ))
  }
  
  def kr( bufNum: GE, in: GE, mul: GE ) : GE = {
    kr( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def kr( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, in ).madd( mul, add ) 
  }
}

object Shaper {
  def ar( bufNum: GE ) : GE = ar( bufNum, Constants.zero )
  
  def ar( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "Shaper", audio, List( audio ), List( bufNum, in ))
  }
  
  def ar( bufNum: GE, in: GE, mul: GE ) : GE = {
    ar( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def ar( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    ar( bufNum, in ).madd( mul, add ) 
  }

  def kr( bufNum: GE ) : GE = kr( bufNum, Constants.zero )
  
  def kr( bufNum: GE, in: GE ) : GE = {
    UGen.multiNew( "Shaper", scalar, List( scalar ), List( bufNum, in ))
  }
  
  def kr( bufNum: GE, in: GE, mul: GE ) : GE = {
    kr( bufNum, in ).madd( mul, Constants.zero ) 
  }
  def kr( bufNum: GE, in: GE, mul: GE, add: GE ) : GE = {
    kr( bufNum, in ).madd( mul, add ) 
  }
}

// IndexL XXX
// DegreeToKey XXX
// Select XXX
// Vibrato XXX
// TChoose XXX
// TWChoose XXX

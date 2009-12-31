/*
 *  Delays.scala
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
object Delay1 {
	def ar( in: GE ) : GE = {
		UGen.multiNew( "Delay1", audio, List( audio ), List( in ))
	}

	def kr( in: GE ) : GE = {
    	UGen.multiNew( "Delay1", control, List( control ), List( in ))
	}
}

object Delay2 {
  def ar( in: GE ) : GE = {
    UGen.multiNew( "Delay2", audio, List( audio ), List( in ))
  }
  
  def ar( in: GE, mul: GE ) : GE = ar( in ).madd( mul, Constants.zero ) 
  def ar( in: GE, mul: GE, add: GE ) : GE = ar( in ).madd( mul, add ) 

  def kr( in: GE ) : GE = {
    UGen.multiNew( "Delay2", control, List( control ), List( in ))
  }

  def kr( in: GE, mul: GE ) : GE = kr( in ).madd( mul, Constants.zero ) 
  def kr( in: GE, mul: GE, add: GE ) : GE = kr( in ).madd( mul, add ) 
}

// these delays use real time allocated memory.

object DelayN {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ))
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayN", audio, List( audio ), List( in, maxDelayTime, delayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ))
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayN", control, List( control ), List( in, maxDelayTime, delayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, add )
  }
}

object DelayL {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ))
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayL", audio, List( audio ), List( in, maxDelayTime, delayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ))
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayL", control, List( control ), List( in, maxDelayTime, delayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, add )
  }
}

object DelayC {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ))
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayC", audio, List( audio ), List( in, maxDelayTime, delayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ))
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )))
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = {
    UGen.multiNew( "DelayC", control, List( control ), List( in, maxDelayTime, delayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime ).madd( mul, add )
  }
}

object CombN {
  def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( audio, i, m, dl, dc ))
  }
  
  def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( control, i, m, dl, dc ))
  }
}

case class CombN( rate: Rate, in: UGenInput, maxDelayTime: UGenInput,
                  delayTime: UGenInput, decayTime: UGenInput )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object CombL {
  def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( audio, i, m, dl, dc ))
  }

  def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( control, i, m, dl, dc ))
  }
}

case class CombL( rate: Rate, in: UGenInput, maxDelayTime: UGenInput,
                  delayTime: UGenInput, decayTime: UGenInput )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object CombC {
  def ar( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( audio, i, m, dl, dc ))
  }

  def kr( in: GE, maxDelayTime: GE = 0.2f, delayTime: GE = 0.2f, decayTime: GE = 1 ) : GE = {
    simplify( for( List( i, m, dl, dc ) <- expand( in, maxDelayTime, delayTime, decayTime ))
      yield this( control, i, m, dl, dc ))
  }
}

case class CombC( rate: Rate, in: UGenInput, maxDelayTime: UGenInput,
                  delayTime: UGenInput, decayTime: UGenInput )
extends SingleOutUGen( in, maxDelayTime, delayTime, decayTime )

object AllpassN {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = ar( in, maxDelayTime, delayTime, Constants.one )
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassN", audio, List( audio ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = kr( in, maxDelayTime, delayTime, Constants.one )
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassN", control, List( control ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }
}

object AllpassL {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = ar( in, maxDelayTime, delayTime, Constants.one )
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassL", audio, List( audio ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = kr( in, maxDelayTime, delayTime, Constants.one )
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassL", control, List( control ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }
}

object AllpassC {
  def ar( in: GE ) : GE = ar( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def ar( in: GE, maxDelayTime: GE ) : GE = ar( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def ar( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = ar( in, maxDelayTime, delayTime, Constants.one )
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassC", audio, List( audio ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def ar( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    ar( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }

  def kr( in: GE ) : GE = kr( in, Constant( 0.2f ), Constant( 0.2f ), Constants.one )
  def kr( in: GE, maxDelayTime: GE ) : GE = kr( in, maxDelayTime, maxDelayTime.max( Constant( 0.2f )), Constants.one )
  def kr( in: GE, maxDelayTime: GE, delayTime: GE ) : GE = kr( in, maxDelayTime, delayTime, Constants.one )
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE ) : GE = {
    UGen.multiNew( "AllpassC", control, List( control ), List( in, maxDelayTime, delayTime, decayTime ))
  }
  
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, Constants.zero )
  }
  def kr( in: GE, maxDelayTime: GE, delayTime: GE, decayTime: GE, mul: GE, add: GE ) : GE = {
    kr( in, maxDelayTime, delayTime, decayTime ).madd( mul, add )
  }
}

// these delays use shared buffers.

// BufDelayN XXX
// BufDelayL XXX
// BufDelayC XXX
// BufCombN XXX
// BufCombL XXX
// BufCombC XXX
// BufAllpassN XXX
// BufAllpassL XXX
// BufAllpassC XXX

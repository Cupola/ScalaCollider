/*
 *  Trig.scala
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
//import Rates._

/**
 *	@version	0.11, 07-Apr-09
 */

object Trig1 {
  def ar( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def ar( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "Trig1", audio, List( audio ), List( in, dur ))
  }
  
  def kr( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def kr( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "Trig1", control, List( control ), List( in, dur ))
  }
}

object Trig {
  def ar( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def ar( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "Trig", audio, List( audio ), List( in, dur ))
  }
  
  def kr( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def kr( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "Trig", control, List( control ), List( in, dur ))
  }
}

object SendTrig {
  def ar( in: GE ) : GE = ar( in, Constants.zero )
  def ar( in: GE, id: GE ) : GE = ar( in, id, Constants.zero )
  def ar( in: GE, id: GE, value: GE ) : GE = {
    UGen.multiNew( "SendTrig", audio, List(), List( in, id, value ))
  }
  
  def kr( in: GE ) : GE = kr( in, Constants.zero )
  def kr( in: GE, id: GE ) : GE = kr( in, id, Constants.zero )
  def kr( in: GE, id: GE, value: GE ) : GE = {
    UGen.multiNew( "SendTrig", control, List(), List( in, id, value ))
  }
}

object TDelay {
  def ar( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def ar( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "TDelay", audio, List( audio ), List( in, dur ))
  }
  
  def kr( in: GE ) : GE = ar( in, Constant( 0.1f ))
  def kr( in: GE, dur: GE ) : GE = {
    UGen.multiNew( "TDelay", control, List( control ), List( in, dur ))
  }
}

object Latch {
  def ar( in: GE, trig: GE ) : GE = {
    UGen.multiNew( "Latch", audio, List( audio ), List( in, trig ))
  }
  
  def kr( in: GE, trig: GE ) : GE = {
    UGen.multiNew( "Latch", control, List( control ), List( in, trig ))
  }
}

object PulseCount {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "Peak", audio, List( audio ), List( trig, reset ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "Peak", control, List( control ), List( trig, reset ))
  }
}

object Peak {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "Peak", audio, List( audio ), List( trig, reset ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "Peak", control, List( control ), List( trig, reset ))
  }
}

object RunningMin {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "RunningMin", audio, List( audio ), List( trig, reset ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "RunningMin", control, List( control ), List( trig, reset ))
  }
}

object RunningMax {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "RunningMax", audio, List( audio ), List( trig, reset ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "RunningMax", control, List( control ), List( trig, reset ))
  }
}

object Stepper {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = ar( trig, reset, Constants.zero )
  def ar( trig: GE, reset: GE, min: GE ) : GE = ar( trig, reset, min, Constant( 7 ))
  def ar( trig: GE, reset: GE, min: GE, max: GE ) : GE = ar( trig, reset, min, max, Constants.one )
  def ar( trig: GE, reset: GE, min: GE, max: GE, step: GE ) : GE = ar( trig, reset, min, max, step, min )
  def ar( trig: GE, reset: GE, min: GE, max: GE, step: GE, resetVal: GE ) : GE = {
    UGen.multiNew( "Stepper", audio, List( audio ), List( trig, reset, min, max, step, resetVal ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = kr( trig, reset, Constants.zero )
  def kr( trig: GE, reset: GE, min: GE ) : GE = kr( trig, reset, min, Constant( 7 ))
  def kr( trig: GE, reset: GE, min: GE, max: GE ) : GE = kr( trig, reset, min, max, Constants.one )
  def kr( trig: GE, reset: GE, min: GE, max: GE, step: GE ) : GE = kr( trig, reset, min, max, step, min )
  def kr( trig: GE, reset: GE, min: GE, max: GE, step: GE, resetVal: GE ) : GE = {
    UGen.multiNew( "Stepper", control, List( control ), List( trig, reset, min, max, step, resetVal ))
  }
}

object PulseDivider {
  def ar( trig: GE ) : GE = ar( trig, Constant( 2 ))
  def ar( trig: GE, div: GE ) : GE = ar( trig, div, Constants.zero )
  def ar( trig: GE, div: GE, start: GE ) : GE = {
    UGen.multiNew( "PulseDivider", audio, List( audio ), List( trig, div, start ))
  }
  
  def kr( trig: GE ) : GE = kr( trig, Constant( 2 ))
  def kr( trig: GE, div: GE ) : GE = kr( trig, div, Constants.zero )
  def kr( trig: GE, div: GE, start: GE ) : GE = {
    UGen.multiNew( "PulseDivider", control, List( control ), List( trig, div, start ))
  }
}

object SetResetFF {
  def ar( trig: GE ) : GE = ar( trig, Constants.zero )
  def ar( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "SetResetFF", audio, List( audio ), List( trig, reset ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.zero )
  def kr( trig: GE, reset: GE ) : GE = {
    UGen.multiNew( "SetResetFF", control, List( control ), List( trig, reset ))
  }
}

object ToggleFF {
  def ar( trig: GE ) : GE = {
    UGen.multiNew( "ToggleFF", audio, List( audio ), List( trig ))
  }
  
  def kr( trig: GE ) : GE = {
    UGen.multiNew( "ToggleFF", control, List( control ), List( trig ))
  }
}

object ZeroCrossing {
  def ar( in: GE ) : GE = {
    UGen.multiNew( "ZeroCrossing", audio, List( audio ), List( in ))
  }
  
  def kr( in: GE ) : GE = {
    UGen.multiNew( "ZeroCrossing", control, List( control ), List( in ))
  }
}

object Timer {
  def ar( trig: GE ) : GE = {
    UGen.multiNew( "Timer", audio, List( audio ), List( trig ))
  }
  
  def kr( trig: GE ) : GE = {
    UGen.multiNew( "Timer", control, List( control ), List( trig ))
  }
}

object Sweep {
  def ar( trig: GE ) : GE = ar( trig, Constants.one )
  def ar( trig: GE, rate: GE ) : GE = {
    UGen.multiNew( "Sweep", audio, List( audio ), List( trig, rate ))
  }
  
  def kr( trig: GE ) : GE = ar( trig, Constants.one )
  def kr( trig: GE, rate: GE ) : GE = {
    UGen.multiNew( "Sweep", control, List( control ), List( trig, rate ))
  }
}

object Phasor {
  def ar( trig: GE ) : GE = ar( trig, Constants.one )
  def ar( trig: GE, rate: GE ) : GE = ar( trig, rate, Constants.zero )
  def ar( trig: GE, rate: GE, start: GE ) : GE = ar( trig, rate, start, Constants.one )
  def ar( trig: GE, rate: GE, start: GE, end: GE ) : GE = ar( trig, rate, start, end, Constants.zero )
  def ar( trig: GE, rate: GE, start: GE, end: GE, resetPos: GE ) : GE = {
    UGen.multiNew( "Phasor", audio, List( audio ), List( trig, rate, start, end, resetPos ))
  }
  
  def kr( trig: GE ) : GE = kr( trig, Constants.one )
  def kr( trig: GE, rate: GE ) : GE = kr( trig, rate, Constants.zero )
  def kr( trig: GE, rate: GE, start: GE ) : GE = kr( trig, rate, start, Constants.one )
  def kr( trig: GE, rate: GE, start: GE, end: GE ) : GE = kr( trig, rate, start, end, Constants.zero )
  def kr( trig: GE, rate: GE, start: GE, end: GE, resetPos: GE ) : GE = {
    UGen.multiNew( "Phasor", control, List( control ), List( trig, rate, start, end, resetPos ))
  }
}

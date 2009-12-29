/*
 *  MacUGens.scala
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

package de.sciss.tint.sc

import Rates._

object MouseX {
	def kr : GE = kr( Constants.zero, Constants.one, Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE ) : GE = kr( minVal, minVal.max( Constants.one ), Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE, maxVal: GE ) : GE = kr( minVal, maxVal, Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE, maxVal: GE, warp: GE ) : GE = kr( minVal, maxVal, warp, Constant( 0.2f ))

	def kr( minVal: GE, maxVal: GE, warp: GE, lag: GE ) : GE = {
//	  if (warp === 'linear, { warp = 0 });	// XXX
//	  if (warp === 'exponential, { warp = 1 });	// XXX
      UGen.multiNew( "MouseX", control, List( control ), List( minVal, maxVal, warp, lag ))
	}
}

object MouseY {
	def kr : GE = kr( Constants.zero, Constants.one, Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE ) : GE = kr( minVal, minVal.max( Constants.one ), Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE, maxVal: GE ) : GE = kr( minVal, maxVal, Constants.zero, Constant( 0.2f ))

    def kr( minVal: GE, maxVal: GE, warp: GE ) : GE = kr( minVal, maxVal, warp, Constant( 0.2f ))

	def kr( minVal: GE, maxVal: GE, warp: GE, lag: GE ) : GE = {
//	  if (warp === 'linear, { warp = 0 });	// XXX
//	  if (warp === 'exponential, { warp = 1 });	// XXX
      UGen.multiNew( "MouseY", control, List( control ), List( minVal, maxVal, warp, lag ))
	}
}

// MouseButton XXX
// KeyState XXX


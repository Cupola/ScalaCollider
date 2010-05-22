/*
 *  MacUGens.scala
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

/**
 *  @version  0.11, 01-Jan-10
 */
object MouseX extends UGen4RArgs {
   def kr : GE = kr()
	def kr( minVal: GE = 0, maxVal: GE = 1, warp: GE = 0, lag: GE = 0.2f ) : GE =
      make( minVal, maxVal, warp, lag )
}
case class MouseX( minVal: UGenIn, maxVal: UGenIn, warp: UGenIn, lag: UGenIn )
extends SingleOutUGen( minVal, maxVal, warp, lag ) with ControlRated

object MouseY extends UGen4RArgs {
   def kr : GE = kr()
	def kr( minVal: GE = 0, maxVal: GE = 1, warp: GE = 0, lag: GE = 0.2f ) : GE =
      make( minVal, maxVal, warp, lag )
}
case class MouseY( minVal: UGenIn, maxVal: UGenIn, warp: UGenIn, lag: UGenIn )
extends SingleOutUGen( minVal, maxVal, warp, lag ) with ControlRated

object MouseButton extends UGen3RArgs {
   def kr : GE = kr()
	def kr( minVal: GE = 0, maxVal: GE = 1, lag: GE = 0.2f ) : GE =
      make( minVal, maxVal, lag )
}
case class MouseButton( minVal: UGenIn, maxVal: UGenIn, lag: UGenIn )
extends SingleOutUGen( minVal, maxVal, lag ) with ControlRated

object KeyState extends UGen4RArgs {
	def kr( keyCode: GE, minVal: GE = 0, maxVal: GE = 1, lag: GE = 0.2f ) : GE =
      make( keyCode, minVal, maxVal, lag )
}
case class KeyState( keyCode: UGenIn, minVal: UGenIn, maxVal: UGenIn, lag: UGenIn )
extends SingleOutUGen( keyCode, minVal, maxVal, lag ) with ControlRated

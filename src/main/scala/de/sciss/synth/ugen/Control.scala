/*
 *  Control.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq }
import de.sciss.synth._

// YYY could be a case class if specialIndex
// was passed as a parameter

/**
 *    @version 0.11, 22-Apr-10
 */
class Control( val rate: Rate, val values: IIdxSeq[ Float ])
extends MultiOutUGen( rate, values.size, Nil )
{
//  override val specialIndex = SynthDef.buildSynthDef.map( _.allocControl( numOutputs )).getOrElse( 0 )

	override val specialIndex = SynthDef.builder.addControl( this )

//	*isControlUGen { ^true }
}

class TrigControl( r: Rate, values: IIdxSeq[ Float ])
extends Control( r, values )

object Control {
	def kr( values: Float* ) : Control = new Control( control, Vector( values: _* ))
	def ir( values: Float* ) : Control = new Control( scalar, Vector( values: _* ))
}

object TrigControl {
	def kr( values: Float* ) : Control = new TrigControl( control, Vector( values: _* ))
	def ir( values: Float* ) : Control = new TrigControl( scalar, Vector( values: _* ))
}

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import collection.immutable.{ IndexedSeq => IIdxSeq }
import de.sciss.tint.sc._

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

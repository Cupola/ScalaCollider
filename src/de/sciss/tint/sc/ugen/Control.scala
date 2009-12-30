/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._

class Control( name: String, rate: Rate, val values: Seq[ Float ])
extends MultiOutUGen( name, rate, (0 until values.size).map (i => rate), Nil )
{
//  override val specialIndex = SynthDef.buildSynthDef.map( _.allocControl( numOutputs )).getOrElse( 0 )

	override val specialIndex = SynthDef.buildSynthDef.map( _.addControl( this )).getOrElse( 0 )

//	*isControlUGen { ^true }
}

object Control {
	def kr( values: Seq[ Float ]) : Control = new Control( "Control", control, values )
	def ir( values: Seq[ Float ]) : Control = new Control( "Control", scalar, values )
}

object TrigControl {
	def kr( values: Seq[ Float ]) : Control = new Control( "TrigControl", control, values )
	def ir( values: Seq[ Float ]) : Control = new Control( "TrigControl", scalar, values )
}

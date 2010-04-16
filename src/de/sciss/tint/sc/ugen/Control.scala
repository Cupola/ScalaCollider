/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._

class Control( val rate: Rate, val values: Seq[ Float ])
extends MultiOutUGen( List.fill( values.size )( rate ), Nil )
{
//  override val specialIndex = SynthDef.buildSynthDef.map( _.allocControl( numOutputs )).getOrElse( 0 )

	override val specialIndex = SynthDef.builder.getOrElse( error( "Executed outside build context" )).addControl( this )

//	*isControlUGen { ^true }
}

class TrigControl( r: Rate, v: Seq[ Float ]) extends Control( r, v )

object Control {
	def kr( values: Seq[ Float ]) : Control = new Control( control, values )
	def ir( values: Seq[ Float ]) : Control = new Control( scalar, values )
}

object TrigControl {
	def kr( values: Seq[ Float ]) : Control = new TrigControl( control, values )
	def ir( values: Seq[ Float ]) : Control = new TrigControl( scalar, values )
}

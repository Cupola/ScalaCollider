/*
 *  Constant.scala
 *  InOut
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

import Predef._
import Rates._

/*
object AbstractOut {
//	numOutputs { ^0 }
//	writeOutputSpecs {}
                
/* XXX
 	checkInputs {
 		if (rate == audio', {
 			for(this.class.numFixedArgs, inputs.size - 1, { arg i;
 				if (inputs.at(i).rate != audio', {
 					^(" input at index " + i + 
 						"(" + inputs.at(i) + ") is not audio rate");
 				});
 			});
 		});
 		^this.checkValidInputs
 	}
*/

// 	*isOutputUGen { ^true }
}
*/

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.10, 15-Jun-08
 */
object Out /* extends AbstractOut */ {
  def ar( bus: GE, channelsArray: GE ) : GE = {
//    println( "bus : " + bus.toUGenInputs.first )
    UGen.multiNew( "Out", audio, Nil, List( bus ) ++ channelsArray.toUGenInputs )
    // XXX ^0.0		// Out has no output
  }

  def kr( bus: GE, channelsArray: Seq[ GE ]) : GE = {
    UGen.multiNew( "Out", control, Nil, List( bus ) ++ channelsArray )
    // XXX ^0.0		// Out has no output
  }

//	*numFixedArgs { ^1 }
}

object ReplaceOut /* extends AbstractOut */ {
  def ar( bus: GE, channelsArray: GE ) : GE = {
//    println( "bus : " + bus.toUGenInputs.first )
    UGen.multiNew( "ReplaceOut", audio, Nil, List( bus ) ++ channelsArray.toUGenInputs )
    // XXX ^0.0		// Out has no output
  }

  def kr( bus: GE, channelsArray: Seq[ GE ]) : GE = {
    UGen.multiNew( "ReplaceOut", control, Nil, List( bus ) ++ channelsArray )
    // XXX ^0.0		// Out has no output
  }

//	*numFixedArgs { ^1 }
}

// AbstractIn : MultiOutUGen {
//  	*isInputUGen { ^true }
// }

object In { /* AbstractIn */
	def ar( bus: GE ) : GE = ar( bus, 1 )
	  
	def ar( bus: GE, numChannels: Int ) : GE = {
	  UGen.multiNew( "In", audio, dup( audio, numChannels ), List( bus ))
	}

 	def kr( bus: GE ) : GE = kr( bus, 1 )

 	def kr( bus: GE, numChannels: Int ) : GE = {
	  UGen.multiNew( "In", control, dup( control, numChannels ), List( bus ))
	}
  
//	init { arg numChannels ... argBus;
//		inputs = argBus.asArray;
//		^this.initOutputs(numChannels, rate)
//	}
}

/**
 *	A descriptor class for a control
 *	UGen, similar to SClang's ControlName class.
 *	Note that the <code>lag</code> parameter
 *	is currently unused.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.31, 08-Oct-07
 */

/*
case class ControlDesc( val name: Symbol, val rate: Symbol, val defaultValue: Float, val lag: Float ) {
//  def this( name: Symbol, rate: Symbol, defaultValue: Float ) = this( name, rate, defaultValue, 0f )
}

object Control {
	private def named( name: Symbol, rate: Symbol, values: Seq[ Float ]) : GE = {
		val synthDef = SynthDef.buildSynthDef.get
//		val index = synthDef.controlIndex

		synthDef.addControlDesc(
		  ControlDesc( name, /* index + i, */ rate, values.first, 0f ))
		UGen.multiNew( "Control", rate, dup( rate, values.length ), Nil )
	}
 
	def kr( name: Symbol, values: Float* ) : GE = named( name, control, values )
 
	def ir( name: Symbol, values: Float* ) : GE = named( name, 'scalar, values )
 
//	init { arg ... argValues;
//		values = argValues;
//		if (synthDef.notNil) {
//			specialIndex = synthDef.controls.size;
//			synthDef.controls = synthDef.controls.addAll(values);
//			synthDef.controlIndex = synthDef.controlIndex + values.size;
//		};
//		^this.initOutputs(values.size, rate)
//	}

//	*isControlUGen { ^true }
}
*/
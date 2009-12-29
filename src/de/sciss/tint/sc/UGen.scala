/*
 *  UGen.scala
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

import _root_.scala.collection.mutable.ListBuffer
import _root_.scala.math._

import _root_.java.io.{ DataOutputStream, IOException }

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.13, 29-Dec-09
 */
object Rates extends Enumeration { // with Ordering[ Enumeration#Value ]
  type Rate = Value
  val scalar, control, audio, demand = Value
//  def compare( a: Enumeration#Value, b: Enumeration#Value ) = a.compare( b )

  def highest( rates: Seq[ Rate ]) : Rate = {
    rates.foldLeft( scalar )( (a, b) => if( a > b ) a else b )
  }
}

import Rates._

trait RatedGE extends GE {
  val rate : Rate
}

trait UGenInput extends RatedGE {
  final val numOutputs = 1
  final def toUGenInputs = List( this )
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit;
}

trait UGenProxy {
  def source : UGen
}

abstract class UGen( val name: String, val rate: Rate, val outputRates: Seq[ Rate ], val inputs: Seq[ UGenInput ])
extends RatedGE with UGenProxy /* UGenInput */ {
  var synthIndex = -1
  val specialIndex = 0
    
  def numInputs = inputs.size
//  val outputs = outputRates.foreach (rate => { })
//  def toUGenInputs = inputs
//  val numOutputs = outputRates.size
  def source = this

    // XXX BEGIN XXX
//  protected var synthDef : SynthDef = null;
  var antecedents : ListBuffer[ UGen ]	= new ListBuffer[ UGen ]()
  var descendants : ListBuffer[ UGen ]	= new ListBuffer[ UGen ]()
  
  addToSynth

  protected def addToSynth {
   	SynthDef.buildSynthDef.foreach( synthDef => synthDef.addUGen( this ))
  }
  
  def initTopoSort {
    inputs.foreach (input => {
      if( input.isInstanceOf[ UGenProxy ]) {
        var ugen = input.asInstanceOf[ UGenProxy ]
        antecedents.append( ugen.source )
        ugen.source.descendants.append( this )
      }
    })
  }

//  def makeAvailable {
//    if( antecedents.isEmpty ) {
//      synthDef.available.append( this );
//    }
//  }
  
//  def schedule( outStack: Buffer[ UGen ]) {
//    descendants.reverse.foreach (ugen => {
//      ugen.removeAntecedent( this )
//    })
//    outStack.append( this )
//  }

  def checkInputs : Option[String] = {
    // checkValidInputs
    None
  }

//  protected def checkValidInputs : Option[String] = {
//    inputs.foreach (input => {
//      if( input.isValidUGenInput.not ) {
//        val argName = this.argNameForInputAt(i) ? i
//        return Some( "arg: '" + argName + "' has bad input: " + input )
//      })
//    })
//    None
//  }

  def argNameForInputAt( idx: Int ) : Option[String] = {
// XXX
//    var method = this.class.class.findMethod(this.methodSelectorForRate);
//    if(method.isNil or: {method.argNames.isNil},{ ^nil });
//    ^method.argNames.at(i + this.argNamesInputsOffset)
    None
  }

  def dumpArgs {
    println( " ARGS:" )
    var count = 0
    inputs.foreach (input => {
      println( "   " + (argNameForInputAt( count ).getOrElse( count.toString )) +
               ": " + input + " " + input.getClass )
      count += 1
    })
  }
  
//  def removeAntecedent( ugen: UGen ) {
//    antecedents -= ugen
//	  makeAvailable
//  }

  def optimizeGraph {
    // nothing
  }
  
//  def collectConstants {
//    inputs.foreach (input => {
//      if( input.isInstanceOf[ Constant ]) synthDef.addConstant( input.asInstanceOf[ Constant ])
//    }) 	
//  }
  
  // XXX END XXX

//  def *( b: GE ) : GE = {
//    println( "WARNING:\nUGen * mul not yet working" )
//    this	// XXX
//  }
}

object UGen {
//  private val rateSymbols = List( 'scalar, 'control, 'audio, 'demand )
  
//  def getRateID( rate: Symbol ) : Int = {
//    rateSymbols.indexOf( rate )
//  }
  
//  def getRateSymbol( rate: Int ) : Symbol = {
//    rateSymbols( rate )
//  }

  def multiNew( name: String, rate: Rate, outputRates: Seq[ Rate ], inputs: Seq[ GE ]) : GE = {
    var chanExp = 0;
    var allOne = true
    var hasZero = false
    for( input <- inputs ) {
      chanExp = max( chanExp, input.numOutputs )
      allOne  = allOne && (input.numOutputs == 1)
      hasZero = hasZero || (input.numOutputs == 0)
    }
    if( hasZero ) return new GESeq( Nil )	// cannot wrap zero size seq
//  if( allOne )  return new UGen( name, rate, outputRates, inputs map (_.asInstanceOf[ UGenInput ]))
//  if( allOne )  return new MultiOutUGen( name, rate, outputRates, inputs map (_.asInstanceOf[ UGenInput ]))
    if( allOne )  return new MultiOutUGen( name, rate, outputRates, inputs.flatMap (_.toUGenInputs ))

    val results = new Array[ GE ]( chanExp )
    val ugenInputs = inputs map (_.toUGenInputs)
        
    for( chan <- (0 until chanExp)) {
      val newArgs = ugenInputs map (multiInput => multiInput( chan % ugenInputs.size ))
//    results.update( chan, new UGen( name, rate, outputRates, newArgs ));
      results.update( chan, new MultiOutUGen( name, rate, outputRates, newArgs ))
    }
    val res2 = results flatMap (_.toUGenInputs)
    GraphBuilder.seq( res2 )
  }
             
//  def outputProxy( rate: Symbol, source: UGen, outputIndex: Int ) : UGen {
//    new UGen( source.name + "[" + outputIndex + "]", rate, List[ rate ], List[ source ])
  }

class MultiOutUGen( override val name: String, override val rate: Rate, override val outputRates: Seq[ Rate ], override val inputs: Seq[ UGenInput ])
extends UGen( name, rate, outputRates, inputs ) {
	// a class for UGens with multiple outputs
	val outputs : Seq[ OutputProxy ] = (0 until outputRates.size) map (i => { 
			new OutputProxy( this, i ); 
    })
    val numOutputs = outputRates.size
    def toUGenInputs = outputs
}

class SingleOutUGen( override val name: String, override val rate: Rate, outputRate: Rate, override val inputs: Seq[ UGenInput ] )
extends UGen( name, rate, List( outputRate ), inputs ) with UGenInput {
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//      val ugenIndex	= synthDef.getUGenIndex( this )
      if( SynthDef.verbose ) println( "  SingleOutUGen.writeInputSpec. ugenIndex = " + synthIndex /* ugenIndex */)
//      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + this )
//      dos.writeShort( ugenIndex )
      if( synthIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + this )
      dos.writeShort( synthIndex )
      dos.writeShort( 0 )
  }
}

class OutputProxy( val source: UGen, val channel: Int )
extends UGenInput with UGenProxy
// extends UGen( source.name + "[" + channel + "]", source.rate, List( rate ), Nil )
{
  val rate = source.rate
//  def toUGenInputs = List( this )
//  val numOutputs = 1
  
//  override protected def addToSynth {
//    synthDef = SynthDef.buildSynthDef
//  }
 
//	init { arg argSource, argIndex;
//		synthIndex = source.synthIndex;
// }
	
//	dumpName {
//		^this.source.dumpName ++ "[" ++ outputIndex ++ "]"
//	}
  
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//      val ugenIndex	= synthDef.getUGenIndex( source )
	  val ugenIndex = source.synthIndex
      if( SynthDef.verbose ) println( "  OutputProxy.writeInputSpec. ugenIndex = " + ugenIndex + "; channel = " + channel )
      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + source )
      dos.writeShort( ugenIndex )
      dos.writeShort( channel )
  }
}

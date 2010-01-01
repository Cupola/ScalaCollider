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

import GraphBuilder._

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.13, 31-Dec-09
 */
/*
object Rates extends Enumeration {
  type Rate = Value
  val scalar, control, audio, demand = Value

  def highest( rates: Seq[ Rate ]) : Rate = {
    rates.foldLeft( scalar )( (a, b) => if( a > b ) a else b )
  }
}
*/

sealed abstract class Rate( val id: Int )

// extends Ordered[ Rate ] {
//   def compare( that: Rate ) = this.id compare that.id
// }

object Rates {
  def highest( rates: Rate* ) = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id > b.id ) a else b )
}

case object scalar  extends Rate( 0 )
case object control extends Rate( 1 )
case object audio   extends Rate( 2 )
case object demand  extends Rate( 3 )

//import Rates._

trait RatedGE extends GE {
  val rate : Rate
}

trait UGenIn extends RatedGE {
  final val numOutputs = 1
  final def toUGenIns = List( this )
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit;
}

trait UGenProxy {
  def source : UGen
}

abstract class UGen
extends RatedGE with UGenProxy /* UGenIn */ {
  def name = { val cn = getClass.getName; cn.substring( cn.lastIndexOf( '.' ) + 1 )}
  def outputRates: Seq[ Rate ]
  def inputs: Seq[ UGenIn ]

  var synthIndex = -1
  val specialIndex = 0
    
  def numInputs = inputs.size
//  val outputs = outputRates.foreach (rate => { })
//  def toUGenIns = inputs
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
//      if( input.isValidUGenIn.not ) {
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

  override def toString: String = {
    name + "." + (rate match {
    case `scalar` => "ir";
    case `control` => "kr";
    case `audio` => "ar";
    case `demand` => "dr";
    case _ => "?";
    }) + inputs.mkString( "(", ", ", ")" )
  }
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
    error( "OBSOLETE ")
  }
  
/*
  def multiNew( name: String, rate: Rate, outputRates: Seq[ Rate ], inputs: Seq[ GE ]) : GE = {
    var chanExp = 0;
    var allOne = true
    var hasZero = false
    for( input <- inputs ) {
      chanExp = max( chanExp, input.numOutputs )
      allOne  = allOne && (input.numOutputs == 1)
      hasZero = hasZero || (input.numOutputs == 0)
    }
    if( hasZero ) return new GESeq()	// cannot wrap zero size seq
//  if( allOne )  return new UGen( name, rate, outputRates, inputs map (_.asInstanceOf[ UGenIn ]))
//  if( allOne )  return new MultiOutUGen( name, rate, outputRates, inputs map (_.asInstanceOf[ UGenIn ]))
    if( allOne )  return new MultiOutUGen( name, rate, outputRates, inputs.flatMap (_.toUGenIns ))

    val results = new Array[ GE ]( chanExp )
    val UGenIns = inputs map (_.toUGenIns)
        
    for( chan <- (0 until chanExp)) {
      val newArgs = UGenIns map (multiInput => multiInput( chan % UGenIns.size ))
//    results.update( chan, new UGen( name, rate, outputRates, newArgs ));
      results.update( chan, new MultiOutUGen( name, rate, outputRates, newArgs ))
    }
    val res2 = results flatMap (_.toUGenIns)
    GraphBuilder.seq( res2: _* )
  }
*/
             
//  def outputProxy( rate: Symbol, source: UGen, outputIndex: Int ) : UGen {
//    new UGen( source.name + "[" + outputIndex + "]", rate, List[ rate ], List[ source ])
  }

trait UGen1Args {
  def apply( rate: Rate, arg1: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE ) : GE =
    simplify( for( List( a1 ) <- expand( arg1 )) yield this( rate, a1 ))

  protected def arExp( arg1: GE ) : GE = make( audio, arg1 )
  protected def krExp( arg1: GE ) : GE = make( control, arg1 )
  protected def irExp( arg1: GE ) : GE = make( scalar, arg1 )
}

trait UGen2Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE ) : GE =
    simplify( for( List( a1, a2 ) <- expand( arg1, arg2 ))
      yield this( rate, a1, a2 ))

  protected def arExp( arg1: GE, arg2: GE ) : GE = make( audio, arg1, arg2 )
  protected def krExp( arg1: GE, arg2: GE ) : GE = make( control, arg1, arg2 )
  protected def irExp( arg1: GE, arg2: GE ) : GE = make( scalar, arg1, arg2 )
}

trait UGen3Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE, arg3: GE ) : GE =
    simplify( for( List( a1, a2, a3 ) <- expand( arg1, arg2, arg3 ))
      yield this( rate, a1, a2, a3 ))

  protected def arExp( arg1: GE, arg2: GE, arg3: GE ) : GE =
    make( audio, arg1, arg2, arg3 )
  protected def krExp( arg1: GE, arg2: GE, arg3: GE ) : GE =
    make( control, arg1, arg2, arg3 )
  protected def irExp( arg1: GE, arg2: GE, arg3: GE ) : GE =
    make( scalar, arg1, arg2, arg3 )
}

trait UGen4Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE, arg3: GE, arg4: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4 ) <- expand( arg1, arg2, arg3, arg4 ))
      yield this( rate, a1, a2, a3, a4 ))
  
  protected def arExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE ) : GE =
    make( audio, arg1, arg2, arg3, arg4 )
  protected def krExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE ) : GE =
    make( control, arg1, arg2, arg3, arg4 )
  protected def irExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE ) : GE =
    make( scalar, arg1, arg2, arg3, arg4 )
}

trait UGen5Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn, arg5: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4, a5 ) <- expand( arg1, arg2, arg3, arg4, arg5 ))
      yield this( audio, a1, a2, a3, a4, a5 ))
  
  protected def arExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE ) : GE =
    make( audio, arg1, arg2, arg3, arg4, arg5 )
  protected def krExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE ) : GE =
    make( control, arg1, arg2, arg3, arg4, arg5 )
  protected def irExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE ) : GE =
    make( scalar, arg1, arg2, arg3, arg4, arg5 )
}

abstract class MultiOutUGen( val outputRates: Seq[ Rate ], val inputs: Seq[ UGenIn ])
extends UGen {
	// a class for UGens with multiple outputs
	val outputs : Seq[ OutputProxy ] = (0 until outputRates.size) map (i => { 
			new OutputProxy( this, i ); 
    })
    val numOutputs = outputRates.size
    def toUGenIns = outputs
}

//  = List.make( inputs.size, rate )
abstract class SingleOutUGen( /* override val name: String, */ val inputs: UGenIn* )
extends UGen with UGenIn {
  def outputRates: Seq[ Rate ] = List( rate )

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
extends UGenIn with UGenProxy
// extends UGen( source.name + "[" + channel + "]", source.rate, List( rate ), Nil )
{
  val rate = source.rate
//  def toUGenIns = List( this )
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

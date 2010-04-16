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

import collection.mutable.ListBuffer
import math._

import java.io.{ DataOutputStream, IOException }

import GraphBuilder._

/**
 *    @author	Hanns Holger Rutz
 *    @version 0.14, 14-Apr-10
 */

sealed abstract class Rate( val id: Int )

object Rates {
  def highest( rates: Rate* ) = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id > b.id ) a else b )
}

case object scalar  extends Rate( 0 )
case object control extends Rate( 1 )
case object audio   extends Rate( 2 )
case object demand  extends Rate( 3 )

sealed abstract class DoneAction( val id: Int )

case object doNothing         extends DoneAction( 0 )
case object pauseSelf         extends DoneAction( 1 )
case object freeSelf          extends DoneAction( 2 )
case object freeSelfPred      extends DoneAction( 3 )
case object freeSelfSucc      extends DoneAction( 4 )
case object freeSelfPredAll   extends DoneAction( 5 )
case object freeSelfSuccAll   extends DoneAction( 6 )
case object freeSelfToHead    extends DoneAction( 7 )
case object freeSelfToTail    extends DoneAction( 8 )
case object freeSelfPausePred extends DoneAction( 9 )
case object freeSelfPauseSucc extends DoneAction( 10 )
case object freeSelfPredDeep  extends DoneAction( 11 )
case object freeSelfSuccDeep  extends DoneAction( 12 )
case object freeAllInGroup    extends DoneAction( 13 )
case object freeGroup         extends DoneAction( 14 )

trait RatedGE extends GE {
  def rate : Rate
}

trait ScalarRated  { def rate = scalar }
trait ControlRated { def rate = control }
trait AudioRated   { def rate = audio }

trait UGenIn extends RatedGE {
//   def outputIndex: Int

   final def numOutputs = 1
   final def toUGenIns = List( this )

//   final def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) { }
   
//   def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) {
//      val ugenIndex = synthDef.getSynthIndex( this )
////	   val ugenIndex = source.synthIndex
////      if( SynthDef.verbose ) println( "  writeInputSpec. ugenIndex = " + ugenIndex + "; channel = " + channel )
//      if( ugenIndex == -1 ) error( "UGen not listed in graph function : " + this )
//      dos.writeShort( ugenIndex )
//      dos.writeShort( outputIndex )
//   }
}

trait UGenProxy {
   def source : UGen
   def outputIndex : Int
}

//trait ExclusiveUGen     // marker trait: UGen can only occur once in a synthdef
//trait SideEffectUGen    // marker trait: UGen has side effects

abstract class UGen
extends RatedGE with UGenProxy {
//   var synthIndex = -1

//   var antecedents : ListBuffer[ UGen ]	= new ListBuffer[ UGen ]()
//   var descendants : ListBuffer[ UGen ]	= new ListBuffer[ UGen ]()

   // ---- constructor ----
   {
//      addToSynth
      SynthDef.builder.foreach( _.addUGen( this ))
   }

   def name = { val cn = getClass.getName; cn.substring( cn.lastIndexOf( '.' ) + 1 )}
   def outputRates: Seq[ Rate ]
   def inputs: Seq[ UGenIn ]
   def numInputs = inputs.size
   def source = this
   def specialIndex = 0
   def outputIndex = 0

//   protected def addToSynth {
//   	SynthDef.buildSynthDef.foreach( synthDef => synthDef.addUGen( this ))
//   }
  
//   def initTopoSort {
//      inputs.foreach (input => {
//         if( input.isInstanceOf[ UGenProxy ]) {
//            var ugen = input.asInstanceOf[ UGenProxy ]
//            antecedents.append( ugen.source )
//            ugen.source.descendants.append( this )
//         }
//      })
//   }

   def checkInputs : Option[String] = {
      // checkValidInputs
      None
   }
   
//   def optimizeGraph {
//      // nothing
//   }

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

//trait UGenIndiv {
//   protected def individuate: Int = SynthDef.graphBuilder.map( _.individuate ) getOrElse 0
//}

trait UGen1Args {
   def apply( rate: Rate, arg1: UGenIn ) : GE
   private def make( rate: Rate, arg1: GE ) : GE =
      simplify( for( List( a1 ) <- expand( arg1 )) yield this( rate, a1 ))

   protected def arExp( arg1: GE ) : GE = make( audio, arg1 )
   protected def krExp( arg1: GE ) : GE = make( control, arg1 )
   protected def irExp( arg1: GE ) : GE = make( scalar, arg1 )
}

trait UGen1RArgs { // single rate
   def apply( arg1: UGenIn ) : GE
   protected def make( arg1: GE ) : GE =
      simplify( for( List( a1 ) <- expand( arg1 )) yield this( a1 ))
}

trait UGen1ArgsIndiv {
   def apply( rate: Rate, arg1: UGenIn, _indiv: Int ) : GE
   def apply( rate: Rate, arg1: UGenIn ) : GE =
      apply( rate, arg1, SynthDef.individuate )
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

trait UGen2RArgs { // single rate
  def apply( arg1: UGenIn, arg2: UGenIn ) : GE
  protected def make( arg1: GE, arg2: GE ) : GE =
    simplify( for( List( a1, a2 ) <- expand( arg1, arg2 )) yield this( a1, a2 ))
}

trait UGen2ArgsIndiv {
   def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, _indiv: Int ) : GE
   def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn ) : GE =
      apply( rate, arg1, arg2, SynthDef.individuate )
}

trait UGen2RArgsIndiv {
   def apply( arg1: UGenIn, arg2: UGenIn, _indiv: Int ) : GE
   def apply( arg1: UGenIn, arg2: UGenIn ) : GE =
      apply( arg1, arg2, SynthDef.individuate )
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

trait UGen3RArgs { // single rate
  def apply( arg1: UGenIn, arg2: UGenIn, arg3: UGenIn ) : GE
  protected def make( arg1: GE, arg2: GE, arg3: GE ) : GE =
    simplify( for( List( a1, a2, a3 ) <- expand( arg1, arg2, arg3 ))
      yield this( a1, a2, a3 ))
}

trait UGen3ArgsIndiv {
   def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, _indiv: Int ) : GE
   def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn ) : GE =
      apply( rate, arg1, arg2, arg3, SynthDef.individuate )
}

trait UGen3RArgsIndiv {
   def apply( arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, _indiv: Int ) : GE
   def apply( arg1: UGenIn, arg2: UGenIn, arg3: UGenIn ) : GE =
      apply( arg1, arg2, arg3, SynthDef.individuate )
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

trait UGen4RArgs {
  def apply( arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn ) : GE
  protected def make( arg1: GE, arg2: GE, arg3: GE, arg4: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4 ) <- expand( arg1, arg2, arg3, arg4 ))
      yield this( a1, a2, a3, a4 ))
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

trait UGen5RArgs {
  def apply( arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn, arg5: UGenIn ) : GE
  protected def make( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4, a5 ) <- expand( arg1, arg2, arg3, arg4, arg5 ))
      yield this( a1, a2, a3, a4, a5 ))
}

trait UGen6Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn,
             arg5: UGenIn, arg6: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE, arg3: GE, arg4: GE,
                    arg5: GE, arg6: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4, a5, a6 ) <- expand( arg1, arg2, arg3, arg4, arg5, arg6 ))
      yield this( audio, a1, a2, a3, a4, a5, a6 ))

  protected def arExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE, arg6: GE ) : GE =
    make( audio, arg1, arg2, arg3, arg4, arg5, arg6 )
  protected def krExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE, arg6: GE ) : GE =
    make( control, arg1, arg2, arg3, arg4, arg5, arg6 )
  protected def irExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE, arg6: GE ) : GE =
    make( scalar, arg1, arg2, arg3, arg4, arg5, arg6 )
}

trait UGen7Args {
  def apply( rate: Rate, arg1: UGenIn, arg2: UGenIn, arg3: UGenIn, arg4: UGenIn,
             arg5: UGenIn, arg6: UGenIn, arg7: UGenIn ) : GE
  private def make( rate: Rate, arg1: GE, arg2: GE, arg3: GE, arg4: GE,
                    arg5: GE, arg6: GE, arg7: GE ) : GE =
    simplify( for( List( a1, a2, a3, a4, a5, a6, a7 ) <-
                expand( arg1, arg2, arg3, arg4, arg5, arg6, arg7 ))
      yield this( audio, a1, a2, a3, a4, a5, a6, a7 ))

  protected def arExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE,
                       arg6: GE, arg7: GE ) : GE =
    make( audio, arg1, arg2, arg3, arg4, arg5, arg6, arg7 )
  protected def krExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE,
                       arg6: GE, arg7: GE ) : GE =
    make( control, arg1, arg2, arg3, arg4, arg5, arg6, arg7 )
  protected def irExp( arg1: GE, arg2: GE, arg3: GE, arg4: GE, arg5: GE,
                       arg6: GE, arg7: GE ) : GE =
    make( scalar, arg1, arg2, arg3, arg4, arg5, arg6, arg7 )
}

abstract class MultiOutUGen( val outputRates: Seq[ Rate ], val inputs: Seq[ UGenIn ])
extends UGen {
	// a class for UGens with multiple outputs
   val numOutputs = outputRates.size
   // WARNING: lazy because proxy read source rate
	lazy val outputs : Seq[ OutputProxy ] = (0 until numOutputs) map (i => {
			new OutputProxy( this, i )
    })
    def toUGenIns = outputs
}

//  = List.make( inputs.size, rate )
abstract class SingleOutUGen( /* override val name: String, */ val inputs: UGenIn* )
extends UGen with UGenIn {
  def outputRates: Seq[ Rate ] = List( rate )

//  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//      if( SynthDef.verbose ) println( "  SingleOutUGen.writeInputSpec. ugenIndex = " + synthIndex /* ugenIndex */)
//      if( synthIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + this )
//      dos.writeShort( synthIndex )
//      dos.writeShort( 0 )
//  }
}

abstract class ZeroOutUGen( val inputs: UGenIn* )
extends UGen /* with SideEffectUGen */ {
  final def outputRates = Nil
  final def toUGenIns: Seq[ UGenIn ] = Nil
  final def numOutputs = 0

//  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//      if( SynthDef.verbose ) println( "  ZeroOutUGen.writeInputSpec. ugenIndex = " + synthIndex /* ugenIndex */)
//      if( synthIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + this )
//      dos.writeShort( synthIndex )
//      dos.writeShort( 0 )
//  }
}

class OutputProxy( final val source: UGen, final val outputIndex: Int )
extends UGenIn with UGenProxy {
   final def rate = source.rate

//   def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//	   val ugenIndex = source.synthIndex
//      if( SynthDef.verbose ) println( "  OutputProxy.writeInputSpec. ugenIndex = " + ugenIndex + "; channel = " + channel )
//      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + source )
//      dos.writeShort( ugenIndex )
//      dos.writeShort( channel )
//   }
}
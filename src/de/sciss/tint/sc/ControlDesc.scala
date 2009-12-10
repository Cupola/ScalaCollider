/*
 *  Constant.scala
 *  ControlDesc
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

import _root_.java.io.{ DataOutputStream, IOException }
import _root_.scala.math._

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 24-Nov-09
 */
case class ControlName( name: String )
{
  def ir : ControlDesc = ir( List( 0f ))
  def ir( value: Float ) : ControlDesc = ir( List( value ))
  def kr : ControlDesc = kr( List( 0f ))
  def kr( value: Float ) : ControlDesc = kr( List( value ))
//  def kr( value: Tuple2[ GE, Float ]) : ControlDesc = kr( Tuple2( value._1, List( value._2 )))
  def tr : ControlDesc = tr( List( 0f ))
  def tr( value: Float ) : ControlDesc = tr( List( value ))
  
  def ir( values: Seq[ Float ]) = new ControlDesc( Some( name ), 'scalar, values, None )
  def kr( values: Seq[ Float ]) = new ControlDesc( Some( name ), 'control, values, None )
  def tr( values: Seq[ Float ]) = new ControlDesc( Some( name ), 'trigger, values, None )

  def kr( values: Tuple2[ GE, Seq[ Float ]]) : ControlDesc = {
    val lags = values._1.toUGenInputs
    val inits = values._2
    val numCh = max( lags.size, inits.size )
//    val iter = lags.elements.counted
    new ControlDesc( Some( name ), 'control, wrapExtend( values._2, numCh ), Some( wrapExtend( lags, numCh )))
  }
  
  private def wrapExtend[T]( coll: Seq[T], size: Int ) : Seq[T] = {
    if( coll.size == size ) coll
    else if( coll.size > size ) coll.take( size )
    else {
//      val result = new Array[T]( size )
      val result = new scala.collection.mutable.ListBuffer[T]() // ( size )
      (0 until size).foreach (i => result.update( i, coll( i )))
      result
    }
  }
}

class ControlDesc( val name: Option[ String ], val rate: Symbol, val initValues: Seq[ Float ], val lag : Option[ Seq[ UGenInput ]])
extends RatedGE
{
  var ugen: UGen = null
  var ugenOutputIndex : Int = 0
  
  val outputs : Seq[ ControlProxy ] = (0 until initValues.size) map (i => { 
    new ControlProxy( this, i ); 
  })
  val numOutputs	= initValues.size
  
  addToSynth

  def toUGenInputs	= outputs

  private def addToSynth {
    SynthDef.buildSynthDef.foreach (_.addControlDesc( this ))
  }
}

class ControlProxy( val desc: ControlDesc, val channel: Int )
extends UGenInput with UGenProxy
{
  val rate = desc.rate

  def source = desc.ugen
    
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
//      val ugenIndex = synthDef.getUGenIndex( desc.ugen )
		val ugenIndex = desc.ugen.synthIndex
      if( SynthDef.verbose ) println( "  ControlProxy.writeInputSpec. ugenIndex = " + ugenIndex + "; ugenOutputIndex = " + desc.ugenOutputIndex + "; channel = " + channel )
      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in synth def : " + source )
      dos.writeShort( ugenIndex )
      dos.writeShort( desc.ugenOutputIndex + channel )
  }
}

class Control( name: String, rate: Symbol, val values: Seq[ Float ])
extends MultiOutUGen( name, rate, (0 until values.size).map (i => rate), Nil )
{
//  override val specialIndex = SynthDef.buildSynthDef.map( _.allocControl( numOutputs )).getOrElse( 0 )

	override val specialIndex = SynthDef.buildSynthDef.map( _.addControl( this )).getOrElse( 0 )
	
//	*isControlUGen { ^true }
}
                      
object Control {
	def kr( values: Seq[ Float ]) : Control = new Control( "Control", 'control, values )
	def ir( values: Seq[ Float ]) : Control = new Control( "Control", 'scalar, values )
}

object TrigControl {
	def kr( values: Seq[ Float ]) : Control = new Control( "TrigControl", 'control, values )
	def ir( values: Seq[ Float ]) : Control = new Control( "TrigControl", 'scalar, values )
}

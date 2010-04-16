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

import java.io.{ DataOutputStream, IOException }
import math._

/**
 *    @version	0.12, 14-Apr-10
 */
case class ControlName( name: String ) {
   def ir : ControlDesc = ir( List( 0f ))
   def ir( value: Float ) : ControlDesc = ir( List( value ))
   def kr : ControlDesc = kr( List( 0f ))
   def kr( value: Float ) : ControlDesc = kr( List( value ))

   def ir( values: Seq[ Float ]) = new ControlDesc( Some( name ), scalar, values, None )
   def kr( values: Seq[ Float ]) = new ControlDesc( Some( name ), control, values, None )

   def kr( values: Tuple2[ GE, Seq[ Float ]]) : ControlDesc = {
      val lags = values._1.toUGenIns
      val inits = values._2
      val numCh = max( lags.size, inits.size )
      new ControlDesc( Some( name ), control, wrapExtend( values._2, numCh ), Some( wrapExtend( lags, numCh )))
   }
  
   private def wrapExtend[T]( coll: Seq[T], size: Int ) : Seq[T] = {
      if( coll.size == size ) coll
      else if( coll.size > size ) coll.take( size )
      else {
         val result = new scala.collection.mutable.ListBuffer[T]() // ( size )
         (0 until size).foreach (i => result.update( i, coll( i )))
         result
      }
   }
}

class ControlDesc( val name: Option[ String ], val rate: Rate, val initValues: Seq[ Float ], val lag : Option[ Seq[ UGenIn ]])
extends RatedGE
{
   var ugen: UGen = null            // XXX mutable
   var ugenOutputIndex : Int = 0    // XXX mutable 

   // ---- constructor ----
   {
      addToSynth
   }

   def outputs : Seq[ ControlProxy ] = (0 until initValues.size) map (i => {
      new ControlProxy( this, i )
   })

   def numOutputs	= initValues.size
   def toUGenIns	= outputs

   private def addToSynth {
      SynthDef.builder.foreach (_.addControlDesc( this ))
   }
}

class ControlProxy( desc: ControlDesc, channel: Int )
extends UGenIn with UGenProxy {
   def rate          = desc.rate
   def source        = desc.ugen
   def outputIndex   = desc.ugenOutputIndex + channel
    
//  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
////      val ugenIndex = synthDef.getUGenIndex( desc.ugen )
//		val ugenIndex = desc.ugen.synthIndex
//      if( SynthDef.verbose ) println( "  ControlProxy.writeInputSpec. ugenIndex = " + ugenIndex + "; ugenOutputIndex = " + desc.ugenOutputIndex + "; channel = " + channel )
//      if( ugenIndex == -1 ) throw new IOException( "UGen not listed in synth def : " + source )
//      dos.writeShort( ugenIndex )
//      dos.writeShort( desc.ugenOutputIndex + channel )
//  }
}